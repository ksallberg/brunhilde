all: src/tcp_echo_fsm.erl src/tcp_listener.erl src/rest_server_app.erl
	erlc +debug_info -o ebin/ src/tcp_echo_fsm.erl
	erlc +debug_info -o ebin/ src/tcp_listener.erl
	erlc +debug_info -o ebin/ src/rest_server_app.erl
