defmodule Bots.Mixfile do
	@moduledoc """
	Конфигурационный файл сборщика проектов Mix. В нём прописаны зависимости внешних библиотек и точка входа в приложение.
	Все зависимости берутся из стандартного репозитория библиотек <a href="https://hex.pm">https://hex.pm</a>.
	Зависимость <b>pgsql</b> - берётся с <a href="https://github.com">https://github.com</a> с явным указанием тэга.
	"""
	use Mix.Project

	@doc """
	Функция <b>project</b> возвращает список параметров проекта.
	"""
	def project do
		[
			app: :bots,
			version: "0.1.0",
			elixir: "~> 1.3",
			build_embedded: Mix.env == :prod,
			start_permanent: Mix.env == :prod,
			deps: deps(),
			description: "System of deploying bots for popular instant messengers",
			name: "Bots",
			docs: [extras: ["README.md"]]
		]
	end

	@doc """
	В функции <b>application</b> описана точка входа в приложение (модуль Bots) и список зависимых приложений, которые должны быть обязательно запущены до запуска текущего приложения.
	"""
	def application do
		[
			applications: [:logger, :httpotion, :poison, :cowboy, :plug, :ssl],
			mod: {Bots, []}
		]
	end

	@doc """
	Функция <b>deps</b> возвращает список зависимостей, необходимых для сборки проекта (это, в основном, сторонние библиотеки).
	"""
	defp deps do
		[
			{:httpotion, "~> 3.0.0"},
			{:poison, "~> 2.2.0"},
			{:exrm, "~> 1.0.6"},
			{:cowboy, "~> 1.0.0"},
			{:plug, "~> 1.0"},
			{:ex_doc, "~> 0.12", only: :dev}
		]
	end
end