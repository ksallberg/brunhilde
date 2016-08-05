#!/bin/bash
erlc -pa ebin -o priv/ servers/*.erl
erl -boot start_sasl -pa ebin -pa deps/jiffy/ebin \
    -pa priv -eval "application:start(rest_server)" \
    -config ebin/sys.config
