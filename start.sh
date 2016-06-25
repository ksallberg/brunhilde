#!/bin/bash
erlc -pa ebin -o servers/ servers/*.erl
erl -boot start_sasl -pa ebin -pa deps/jiffy/ebin \
    -pa servers -eval "application:start(rest_server)" \
    -config ebin/sys.config
