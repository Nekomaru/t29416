# (c) TenderPro inc., 2016 
# http://tender.pro, cf@tender.pro, +7(495)215-14-38
# Authors: ...

defmodule Bots.WebSupervisor do
	@moduledoc """
	Модуль <b>WebSupervisor</b> предназначен для слежения за входящими сообщениями с сервера <b>telegram</b> посредством <b>webhook</b>.
	Данный модуль начинает работать при запуске приложения.
 	"""
	use Supervisor

	def start_link() do
		Supervisor.start_link(__MODULE__, :ok)
	end

	def init(:ok) do
		cfg = case Application.get_env(:bots, :webserver) do
			:nil->
				[port: 8080]
			config->
				config
		end
		children = [
			Plug.Adapters.Cowboy.child_spec((if cfg[:keyfile] do :https else :http end), Bots.WebRouter, [], cfg)
		]
		supervise(children, strategy: :one_for_one)
	end
end
