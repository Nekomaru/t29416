# (c) TenderPro inc., 2016 
# http://tender.pro, cf@tender.pro, +7(495)215-14-38
# Authors: ...


defmodule Bots.Telegram.Commander do
	@moduledoc """
	Модуль <b>Commander</b> откуда генерируется ответ и отправляется на сервер <b>telegram</b>.
	У каждого полученного блока сообщений от <b>processor</b> есть уникальный последовательный идентификатор.
	Его мы инкрементируем после каждой успешной обработки блока сообщений. Это нужно для исключения обработки старых сообщений.
	"""
	
	@doc """
	Данный метод принимая в качестве входящего параметра <b>Map</b>-объект <b>update</b>, который включает в себя новое сообщение, предназначенный для бота, генерирует ответное сообщение.
	В случае нахождения команды содержащегося в сообщении в списке зарегистрированных команд генерируется соответствующее ответное сообщение.
	В случае отсутствия команды в списке команд метод <b>get_response</b> генерирует стандартное сообщение от бота о невозможности распозновании команды от пользователя бота (<b>Unknown command</b>).
	"""
	def get_response(update) do
		IO.puts "msg: " <> inspect update
		cond do
			update["message"] ->
				message = update["message"]
				splited = String.split(message["text"], " ")
				command = hd(splited)
				chat_id = message["chat"]["id"]

				response = case command do
					"/hello"->
						{:send_message, %{text: "Hi, " <> message["from"]["first_name"], chat_id: chat_id}}
					"/batch"->
						{:batch, [
							{:send_message, %{text: "Batch response 1", chat_id: chat_id}},
							{:send_message, %{text: "Batch response 2", chat_id: chat_id}},
							{:send_message, %{text: "Batch response 3", chat_id: chat_id}}
						]}
					c->
						{:send_message, %{text: "Unknown command: #{c}.", chat_id: chat_id}}
				end
		end
		
	end

end
