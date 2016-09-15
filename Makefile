build:
	rebar3 compile

start:
	sh start.sh
.PHONY: start

clean:
	rm -rf _build
	rm -f ebin/*.beam
	rm -f priv/*.beam
	rm -rf lux_logs
	rm -rf log/*.log
.PHONY: clean

# if no plt file:
# dialyzer --build_plt --apps mnesia
# dialyzer --add_to_plt ./_build/default/lib/rest_server/
dialyzer:
	dialyzer --src src/
.PHONY: dialyzer

test: build
	lux test/
.PHONY: test
