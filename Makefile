build:
	rebar3 compile

start:
	sh start.sh
.PHONY: start

clean:
	rm -rf _build
	rm -f ebin/*.beam
	rm -f priv/*.beam
.PHONY: clean

# if no plt file:
# dialyzer --build_plt --apps mnesia
# dialyzer --add_to_plt ./_build/default/lib/rest_server/
dialyzer:
	dialyzer --src src/
.PHONY: dialyzer

test: build
	_build/default/lib/lux/bin/lux test/run.lux
.PHONY: test
