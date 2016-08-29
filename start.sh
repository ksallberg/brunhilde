#!/bin/bash
erlc -pa -pa _build/default/lib/*/ebin -o priv/ servers/*.erl
erl -boot start_sasl -pa _build/default/lib/*/ebin \
    -pa priv -eval "application:start(rest_server)" \
    -config ebin/sys.config
