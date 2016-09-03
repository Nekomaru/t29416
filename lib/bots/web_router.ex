# (c) TenderPro inc., 2016 
# http://tender.pro, cf@tender.pro, +7(495)215-14-38
# Authors: ...


defmodule Bots.WebRouter do
	@moduledoc """
	Маршрутизатор для ботов, работающих по webhook
 	"""
	use Plug.Router

	plug :match
	plug :dispatch

	post "/bot/:name", do: Bots.WebhookProcessor.process(name, conn)

	match _, do: send_resp(conn, 404, "not found")
end
