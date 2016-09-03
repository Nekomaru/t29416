# (c) TenderPro inc., 2016 
# http://tender.pro, cf@tender.pro, +7(495)215-14-38
# Authors: ...


defmodule Bots.RootSupervisor do
	@moduledoc """
	Данный модуль является основным <b>RootSupervisor</b> процессом.
	В зависимости от режима использования бота модуль <b>RootSupervisor</b> запускает процесс <b>BotsSupervisor</b> для работы в режиме <b>active</b> (long-polling) и процесс <b>WebSupervisor</b> для работы в режиме <b>passive</b> (web-сервера).
	"""
	use Supervisor
	
	@doc """
	Метод <b>init</b> инициалиизирует процесс запуска <b>Supervisor</b> модулей.
	Спецификация ботов может быть представлена в виде переменной <b>bots_spec</b>.
	Эта переменная должна включать в себя все необходимые параметры (наименование бота, режим работы, настройки и тд).
	Данная переменная разбирается на части и из этих частей генерируется список ботов для запуска.
	"""
	def init(_) do
		list_of_bots = case Application.get_env(:bots, :bots_spec) do
			bots_spec when is_list(bots_spec) and length(bots_spec) > 0 ->
				bots_spec
			_->
				[]
		end		

		children = [
			supervisor(Bots.BotsSupervisor, [list_of_bots]),
			supervisor(Bots.WebSupervisor, [])
		]
		supervise(children, strategy: :one_for_one)
	end


end
