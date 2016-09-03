# (c) TenderPro inc., 2016 
# http://tender.pro, cf@tender.pro, +7(495)215-14-38
# Authors: ...


defmodule Bots do
	@moduledoc """
	Модуль <b>Bots</b> - основной модуль, которая является точкой входа приложения.
	В данном модуле происходит запуск корневого <b>RootSupervisor</b>а.
	"""
	use Application

	@doc """
	Функция <b>start</b> - это функция запуска <b>RootSupervisor</b>.
	Принимает два входящих параметра - _type и _args.
	
	"""
	def start(_type, _args) do
		Supervisor.start_link(Bots.RootSupervisor, [], [])
	end
end
