all:
	erlc +debug_info -o ebin/ src/tcp_reply_fsm.erl
	erlc +debug_info -o ebin/ src/tcp_listener.erl
	erlc +debug_info -o ebin/ src/rest_server_app.erl
	erlc +debug_info -o ebin/ src/route_handler.erl
	erlc +debug_info -o ebin/ src/http_parser.erl
	erlc +debug_info -o ebin/ src/battle_ship.erl

clean:
	rm -rf ebin/*.beam
