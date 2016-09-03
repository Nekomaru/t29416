# (c) TenderPro inc., 2016 
# http://tender.pro, cf@tender.pro, +7(495)215-14-38
# Authors: ...

defmodule Bots.Telegram.BotPassive do
	@moduledoc """
	<b>BotPassive</b> - модуль с поведением <b>gen_server</b>. Используется ботами, работающими на <b>webhook</b>.
	Процессы <b>BotPassive</b> создаются супервизором ботов. Этот процесс может принимать данные от обработчика запросов <b>WebhookProcessor</b>.
	Приняв данные, процесс производит с ними точно такие же действия, как и бот с типом <b>long-polling</b>, за исключением ненадобности запоминания идентификатора блока сообщений.
	То есть, приняв последние сообщения от сервера <b>telegram</b> передает их на обработку.
	"""
	use GenServer
	require Logger
	

	defmodule State do
		defstruct options: nil
	end

	def start_link(options) do
		GenServer.start_link(__MODULE__, %State{options: options}, [])
	end

	## Server Callbacks

	def init(state) do
		:io.format "Starting bot ~p with state: ~p~n", [self(), state.options]
		{:ok, state}
	end
	
	@doc """
	Используется данный метод <b>handle_info</b> если от <b>webhook</b> приходят сообщения.
	В качестве входящих параметров принимает переменные объекта от <b>webhook</b> и state.
	"""
	def handle_info({:new_message, json}, state) do
		Bots.Telegram.Processor.decode_webhook_data(json, state.options)
		{:noreply, state}
	end

	def handle_info(_, state) do
		{:noreply, state}
	end

	def handle_call(_, _from, state) do
		{:reply, :none, state}
	end

	def handle_cast(_, state) do
		{:noreply, state}
	end


end
