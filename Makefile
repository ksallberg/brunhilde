build:
	rebar3 compile

start: servers
	erl -boot start_sasl -pa _build/default/lib/*/ebin \
            -pa priv -eval "application:start(brunhilde)" \
            -config ebin/sys.config

.PHONY: start

clean:
	rm -rf _build
	rm -f ebin/*.beam
	rm -f priv/*.beam
	rm -rf lux_logs
	rm -rf log/*.log
.PHONY: clean

servers: build
	erlc -o priv/ servers/*.erl -pa _build/default/lib/brunhilde/ebin/
.PHONY: servers

dialyzer:
	rebar3 dialyzer
.PHONY: dialyzer

xref:
	rebar3 xref

test: build
	lux test/
.PHONY: test

proper:
	rebar3 proper
.PHONY: proper
