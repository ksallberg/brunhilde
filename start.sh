#!/bin/bash
erl -boot start_sasl -pa ebin -pa deps/jiffy/ebin \
    -eval "application:start(rest_server)" \
    -config ebin/sys.config
