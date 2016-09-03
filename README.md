# Bots
Данная система предназначена для развертывания набирающих популярность ботов на разных мессенджерах.
На данный момент он включает в себя поддержку ботов для мессенджера <b>telegram</b>.
Поддерживает два способа обмена данными с сервером <b>telegram</b>: <a href="https://core.telegram.org/bots/api#setwebhook"><b>webhook</b></a> и <a href="https://core.telegram.org/bots/api#getupdates"><b>long-polling</b></a>.

<h2>Configuring</h2>
Все настройки приложения должны быть прописаны в файле конфигурации <b>config.exs</b> и иметь следующую схему:

```elixir
config :bots, :bots_spec, [
	{"worker_bot", :active, [{:token, "PlaceYourTokenHere"}, {:commander, Bots.Telegram.Commander}]},
	{"webhook_bot", :passive, [{:token, "PlaceYourTokenHere"}, {:commander, Bots.Telegram.Commander}]}
]
```
Первый элемент в каждом кортеже конфигурации содержит наименование бота в строковом типе, который должен быть уникальным.
Второй элемент представляет собой режим работы бота и может принимать одно из двух значений: <b>active</b> и <b>passive</b>.
Режим <b>active</b> предназначен для выгрузки, предназначенных для нашего бота, сообщений из сервера телеграм посредством <b>long-polling</b> соединения.
Режим <b>passive</b> используется для получения изменений из сервера <b>telegram</b> через <b>webhook</b>.
Третий, не менее важный элемент, содержит список параметров бота. Он включает в себя <b>token</b> (маркер) бота, <b>commander</b> (модуль, предназначенный для подготовки ответного сообщения на предоставленный запрос).

<h2>Commander</h2>
Модуль <b>commander</b> предназначен для генерации ответного сообщения на запрос.
Данный модуль должен иметь <b>public</b>-функцию <b>get_response(message)</b>, где <b>message</b> должен быть сконфигурирован по спецификации <b>telegram</b> (<a href="https://core.telegram.org/bots/api#message" target="_blank">Telegram's message object</a>).