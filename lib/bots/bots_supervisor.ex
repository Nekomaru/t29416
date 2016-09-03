# (c) TenderPro inc., 2016 
# http://tender.pro, cf@tender.pro, +7(495)215-14-38
# Authors: ...


defmodule Bots.BotsSupervisor do
	@moduledoc """
	Модуль <b>BotsSupervisor</b> предназначен для ведения контроля за работоспособностью ботов.
	Все процессы по созданию, уничтожению, поиска процесса бота по имени и отправка сообщения в этот процесс возложены на этот модуль.
	При создании ботов отслеживается режим работы бота и создаётся соответствующий этому режиму <b>gen_server</b> процесс. 
	Если режим процесса <b>active</b> будет создан процесс с реализацией <b>long-polling</b>. 
	Если режим процесса <b>passive</b> будет создан процесс с использованием <b>webhook</b>.
 	"""
	use Supervisor
	require Logger

	@supervisor_name MainBotsSupervisor
	
	
	def start_link(bots_specs) do
		Supervisor.start_link(__MODULE__, bots_specs, [{:name, @supervisor_name}])
	end

	@doc """
	Метод <b>init</b> принимает в качестве входящего параметра спецификацию ботов (<b>bots_specs</b>).
	Проводит создание бота по спецификации, которые прошли валидацию.
	
	"""
	def init(bots_specs) do
		# Creating workers from bots specifications
		children = Enum.map(bots_specs, fn(bot)->
			case prepare_worker_spec(bot) do
				{:ok, spec}->
					spec
				_->
					[]
			end
		end)
		supervise(List.flatten(children), strategy: :one_for_one)
	end

	@doc """
	Метод <b>create_bot</b> предназначен для создания процесса бота по переданному типу.
	В качестве входящего параметра принимает спецификацию бота <b>bot_spec</b>.
	Проводит проверку спецификацию на валидность после чего пытается создать процесс запуска бота.
	При создании процесса запуска учитывает такие факторы как: бот с таким именем уже сущетвует (<b>already_present</b>),
	бот с таким именем уже запушен (<b>already_started</b>). В случае обнаружения таких ошибок возвращает соответствующее сообщение с ошибкой.
 	При успешном создании процесса запуска бота, выдает сообщение <b>ok</b>.
	"""
	def create_bot(bot_spec) do
		case prepare_worker_spec(bot_spec) do
			{:ok, spec} ->
				case Supervisor.start_child(@supervisor_name, spec) do
					{:ok, _}->
						:ok
					{:ok, _, _}->
						:ok
					{:error, {:already_started, _}}->
						:already_started
					{:error, :already_present}->
						:already_present
					e->
						IO.puts inspect e
						e
				end
			Error->
				Error
		end
	end

	@doc """
	Данный метод предназначен для проверки на валидность спецификации бота.
	В качестве входящего параметра принимает переменную <b>bot_spec</b> (спецификация бота).
	Спецификация должна содержать кортеж с тремя параметрами (имя бота, режим работы бота, список параметров).
	При не прохождении на валидность метод возвращает соответствующее сообщение - <b>bad_bot_spec</b>.
	В случае успешного прохождения на валидность возвращает сообщение <b>ok</b> со всеми параметрами.
	"""
	def prepare_worker_spec(bot_spec) do
		try do
			case bot_spec do
				_ when is_tuple(bot_spec) and tuple_size(bot_spec) == 3 ->
					{name, type, options} = bot_spec
					module = case type do
						:active->
							Bots.Telegram.BotActive
						:passive->
							Bots.Telegram.BotPassive
						_->
							throw :bad_bot_type
					end
					{:ok, worker(module, [options], [id: name])}
				_->
					throw :bad_bot_spec
			end
		catch
			error->
				Logger.error "Error: #{inspect error} in spec #{inspect bot_spec}"
				error
		end
	end

	@doc """
	Этот метод проводит поиск процесса бота по имени и, в случае нахождения, пытается отправить сообщение в этот процесс.
	Принимает два входящих параметра (<b>name</b> и <b>msg</b>).
	Параметр <b>name</b> - это имя бота, в процесс которого необходимо передать сообщение <b>msg</b>.
	При успешной передачи сообщения возвращает соббщение <b>ok</b>.
	В случае не нахождения процесса бота с именем <b>name</b> возвращает сообщение <b>bot_not_found</b>.
	"""
	def send_msg_to_worker(name, msg) do
		children = Supervisor.which_children(@supervisor_name)
		IO.puts inspect children
		case :lists.keyfind(name, 1, children) do
			{_, pid, _, _}->
				send pid, {:new_message, msg}
				:ok
			false->
				Logger.error "Bot not found: #{inspect name}"
				:bot_not_found
		end
	end

end
