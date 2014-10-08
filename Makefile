all:
	erlc -o ebin/ src/tcp_reply.erl
	erlc -o ebin/ src/tcp_listener.erl
	erlc -o ebin/ src/rest_server_app.erl
	erlc -o ebin/ src/route_handler.erl
	erlc -o ebin/ src/http_parser.erl
	erlc -o ebin/ src/battle_ship.erl
	erlc -o ebin/ src/commands.erl

clean:
	rm -rf ebin/*.beam
