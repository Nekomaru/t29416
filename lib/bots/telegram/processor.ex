# (c) TenderPro inc., 2016 
# http://tender.pro, cf@tender.pro, +7(495)215-14-38
# Authors: ...


defmodule Bots.Telegram.Processor do
	@moduledoc """
	Декодирует <b>json</b>-сообщения, делегирует готовые объекты нужному <b>commander</b> и отправляет ответ на сервер <b>telegram</b>.

	Модуль <b>Processor</b> декодирует приходящее в виде <b>json</b>-сообщение от сервера <b>telegram</b> в структуру <b>Bots.Telegram.TelegramResponse<b> в соответствии с официальной документацией Bot API.
	После парсинга структура передаётся в отдельный поток, где передаётся в <b>commander</b>, заданный в опциях бота.
	После получения ответа от <b>commander</b> происходит отправка подготовленных данных на сервер <b>telegram</b>.
	"""
	require Logger
	
	@doc """
	Метод предназначен для декодирование <b>json</b>-данных, пришедших методом <b>webhook</b>.
	Данные декодируются в структуру <b>Map</b>. После успешного декодирования результат отправляется в отдельный поток с функцией <b>process_messages</b>.
	
	В качестве входящего параметра принимает переменную <b>data</b> содержащую <b>json</b>-данные.
	Второй параметр <b>options</b> содержит <b>commander</b> и <b>token</b> бота.
	"""
	def decode_webhook_data(data, options) do
		{:ok, decoded} = Poison.Parser.parse(data)
		spawn(__MODULE__, :process_messages, [[decoded], options])
	end	

	@doc """
	Декодирование <b>json</b>-данных, пришедших в ответ на запрос методом <b>long-polling</b>.
	Данные декодируются в структуру <b>Map</b>. После успешного декодирования результат отправляется в отдельный поток со списком функций <b>process_messages<b>.
	Более того, этот метод возвращает максимальный идентификатор объекта <b>update</b> среди пришедших.
	Этот метод принимает два входящих параметра: <b>data</b> и <b>options</b>.
	
	Параметр <b>data</b> - это <b>json</b>-данные, полученные посредством <b>getUpdates</b> от сервера <b>telegram</b>.

	Второй параметр <b>options</b> содержит <b>commander</b> и <b>token</b> бота.
	
	В случае получения ошибочных данных от сервера <b>telegram</b>, метод возвращает соответствующее сообщение <b>"bad response"</b>.
	Если в ответе от сервера <b>telegram</b> пришли валидные данные, но они не имеют новых сообщений, то <b>decode_response_data</b> выдает сообщение <b>"no new message"</b>.
	"""
	def decode_response_data(data, options) do
		{:ok, decoded} = Poison.Parser.parse(data)
		case decoded["ok"] do
			true ->
				list_of_updates = decoded["result"]
				if length(list_of_updates) > 0 do
					max = Enum.max_by(list_of_updates, fn(x)-> 
						x["update_id"]
					end)
					spawn(__MODULE__, :process_messages, [list_of_updates, options])
					max["update_id"]
				else
					Logger.info "no new messages"
					:nil
				end
			_->
				Logger.error "bad response"
				:nil
		end
	end

	@doc """
	Метод для параллельной обработки сообщений. После обработки сообщение передает данные в метод подготовки для отправки сообщений.
	В качестве входящих параметром принимает два переменных: <b>data</b> и <b>options</b>.
	
	В <b>options</b> должен быть представлен <b>commander</b> для обработки сообщения и <b>token</b> для отправки ответа.
	Каждое сообщение из массива выполняется в своём потоке.
	
	Параметр <b>data</b> содержит <b>Map</b>-объект декодированный из <b>json</b>-данных, пришедших с сервера <b>telegram</b>.
	"""
	def process_messages(data, options) do
		commander = options[:commander]
		token = options[:token]
		Enum.each(data, fn(update)->
			spawn(__MODULE__, :prepare_and_send, [commander, update, token])
		end)
	end

	@doc """
	Метод <b>prepare_and_send</b> является методом подготовки сообщения для отправки. Выполняет получение ответа от <b>commander</b> и передачу его в метод отправки.
	Метод принимает 3 входящих параметра: <b>commander</b>, <b>update</b> и <b>token</b>.
	
	Параметр <b>commander</b> указывает на то, с каким <b>commander</b>ом необходимо работать.
	
	Параметр <b>update</b> содержит <b>Map</b>-объект сообщения.
	
	Параметр <b>token</b> содержит соответственно маркер (идентификатор) бота.
	"""
	def prepare_and_send(commander, update, token) do
		{response_type, response_data} = commander.get_response(update)
		send_msg(response_type, response_data, token)
	end

	@doc """
	Переопределяемый метод <b>send_msg</b> с первым параметром <b>batch</b>.
	Принимает в качестве входящих параметров 3 переменных и выполняет отправку сообщений.
	
	Параметр <b>batch</b> указывает на то, что одному запросу соответствует несколько ответных сообщений.
	
	Параметр <b>list_of_actions</b> содержит тела сообщений и дополнительные параметры.
	
	Параметр <b>token</b> содержит соответственно маркер (идентификатор) бота.
	"""
	defp send_msg(:batch, list_of_actions, token) do
		Enum.each(list_of_actions, fn({response_type, response_data})->
			send_msg(response_type, response_data, token)
		end)
	end

	@doc """
	Переопределяемый метод отправки сообщения или его редактирования.
	Метод <b>send_msg</b> в зависимости от запроса пытается определить необходимость отправки нового сообщения или же редактирования старого сообщения.
	В качестве входящих параметров принимает 3 переменных: <b>response_type</b>, <b>map_of_query_fields</b> и <b>token</b>.
	
	Первый параметр указывает на тип ответного сообщения. Может содержать в себе одно из трех типов: <b>send_message</b>, <b>edit_text</b> и <b>inline</b>.
		<b>send_message</b> - отправка нового сообщения.
		<b>edit_text</b> - редактирования существующего сообщения.
		<b>inline</b> - включение сообщения в тело другого сообщения.
	Для более подробной информацией о типах сообщений в <b>telegram</b> пройдите по <a href="https://core.telegram.org/bots/api">ссылке</a>.
	
	Параметр <b>map_of_query_fields</b> содержит <b>Map</b>-объект с сообщением и дополнительными параметрами.

	Параметр <b>token</b> содержит соответственно маркер (идентификатор) бота.
	"""
	defp send_msg(response_type, map_of_query_fields, token) do
		path = case response_type do
			:send_message->
				"sendMessage"
			:edit_text->
				"editMessageText"
			:inline->
				"answerInlineQuery"
		end
		url = HTTPotion.process_url("https://api.telegram.org/bot#{token}/#{path}")
		IO.puts "request : " <> inspect url
		
		encoded = URI.encode_query(map_of_query_fields)
		response = HTTPotion.post url, [body: encoded, headers: ['Content-Type': "application/x-www-form-urlencoded"]]
		IO.puts "response: " <> inspect response
	end

end
