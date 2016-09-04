build:
	rebar3 compile

start:
	sh start.sh

clean:
	rm -rf _build
	rm -f ebin/*.beam
	rm -f priv/*.beam
