build:
	rebar3 compile

start: servers
	erl -boot start_sasl -pa _build/default/lib/*/ebin \
            -pa priv -eval "application:start(rest_server)" \
            -config ebin/sys.config

.PHONY: start

# Demo of how to embed brunhilde in another application
start_external: servers
	erlc -o priv/ test/external.erl -pa _build/default/lib/rest_server/ebin/
	erlc -o priv/ test/external_app.erl -pa _build/default/lib/rest_server/ebin/
	erl -boot start_sasl -pa _build/default/lib/*/ebin \
            -pa priv -eval "application:start(external_app)" -config ebin/sys.config

clean:
	rm -rf _build
	rm -f ebin/*.beam
	rm -f priv/*.beam
	rm -rf lux_logs
	rm -rf log/*.log
.PHONY: clean

servers: build
	erlc -o priv/ servers/*.erl -pa _build/default/lib/rest_server/ebin/
.PHONY: servers

# if no plt file:
# dialyzer --build_plt --apps mnesia
# dialyzer --add_to_plt ./_build/default/lib/rest_server/
dialyzer:
	dialyzer --src src/
.PHONY: dialyzer

test: build
	lux test/
.PHONY: test
