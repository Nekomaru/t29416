# (c) TenderPro inc., 2016 
# http://tender.pro, cf@tender.pro, +7(495)215-14-38
# Authors: ...



defmodule Bots.Telegram.BotActive do
	@moduledoc """
	<b>BotActive</b> - модуль с поведением <b>gen_server</b>, т.е. хранит состояние, позволяет обрабатывать команды синхронно и асинхронно.
	Модуль имеет зацикленный вызов подключения к серверу телеграма.
	Подключение может быть <b>long-polling</b> - в этом случае мы отправляем запрос на сервер телеграма и телеграм держит соединение некоторое время.
	Если в это время у телеграма появились сообщения для нас, то он сразу же отдаёт их нам.
	Если сообщений не появилось за время, указанное в параметрах <b>long-polling</b>, то соединение обрывается. Следующая итерация аналогична.
	"""
	use GenServer
	require Logger

	defmodule State do
		defstruct options: nil, last_update_id: nil
	end

	@timeout_check 1000
	@timeout_polling 10 # in seconds
	@fetch_limit 100 # count of messages which can be fetched in one request


	def start_link(options) do
		GenServer.start_link(__MODULE__, %State{options: options}, [])
	end

	## Server Callbacks

	def init(state) do
		Logger.info "Starting bot #{inspect self()} with state: #{inspect state.options}"
		Process.send_after(self(), :check, 1000)
		{:ok, state}
	end

	def handle_call(_, _from, state) do
		{:reply, :none, state}
	end

	def handle_cast(_, state) do
		{:noreply, state}
	end

	@doc """
	Метод выполняет подключение к серверу <b>telegram</b>.
	Используется способ соединения <b>long-polling</b>, то есть выполняется запрос на сервер <b>telegram</b> и удержживается данное соединение определенное время.
	За это время метод ожидает ответа от сервера <b>telegram</b>. Сервер <b>telegram</b> должен передать список сообщений, предназначенный для данного бота.
	В случае успешной передачи сообщений соединение обрывается и по итерации устанавливается новое соединение.
	В случае же отсутствия сообщений, соединение обрывается по окончании времени соединения и по итерации устанавливается новое соединение.
	"""
	def handle_info(:check, state = %State{last_update_id: lui}) do
		token = state.options[:token]
		response = make_query(token, (if lui === :nil, do: 0, else: lui+1))

		{error, new_state} = case response do
			%HTTPotion.Response{}->
				case response.status_code do
					200->
						case Bots.Telegram.Processor.decode_response_data(response.body, state.options) do
							:nil->
								{false, state}
							max_last_update_id->
								# We must remember previous update id, it used as offset param in HTTP-query to fetch only new messages from telegram
								{false, %{state | last_update_id: max_last_update_id}}
						end
					_->
						Logger.error "bad response code: " <> inspect response
						{true, state}
				end
			_->
				Logger.error "very bad response: " <> inspect response
				{true, state}
		end

		# Don't use timeout when long-polling enabled
		timeout = if @timeout_polling > 0 and !error, do: 0, else: @timeout_check

		Process.send_after(self(), :check, timeout)
		{:noreply, new_state}
	end

	def handle_info(_, state) do
		{:noreply, state}
	end

	## Inner functions

	@doc """
	Метод запуска процесса <b>long-polling</b> и принятия собранных сервером <b>telegram</b> новых сообщений методом GET.
	Данный метод принимает два входящих параметра: <b>token</b> и <b>offset</b>.
	Параметр <b>token</b> содержит идентификатор бота на сервере <b>telegram</b>.
	Параметр <b>offset</b> инкрементированный на 1 единицу идентификатор последнего полученного списка <a href="https://core.telegram.org/bots/api#getupdates"><b>getUpdates</b></a>.
	Метод <b>make_query</b> возвращает собранные сообщения для данного бота.	
	"""
	defp make_query(token, offset) do
		# Just preparing url and making request via 'GET' method
		url = HTTPotion.process_url("https://api.telegram.org/bot" <> token <> "/getUpdates", [query: %{offset: offset, limit: @fetch_limit, timeout: @timeout_polling}])
		HTTPotion.get url, [timeout: 12000]
	end


end
