# (c) TenderPro inc., 2016 
# http://tender.pro, cf@tender.pro, +7(495)215-14-38
# Authors: ...

use Mix.Config

# bots_spec is a list of tuples {BotModule, Options} where BotModule is a name of gen_server module, Options is a proplist
config :bots, :bots_spec, [
	#{"worker_bot", :active, [{:token, "***"}, {:commander, Bots.Telegram.Commander}]}
	{"webhook_bot", :passive, [{:token, "***"}, {:commander, Bots.Telegram.Commander}]}
]

config :bots, :webserver, [{:port, 8443}, {:keyfile, "private.key"}, {:certfile, "public.crt"}, {:cacertfile, "ca.crt"}]
