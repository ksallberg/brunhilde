#!/bin/bash
erlc -o priv/ servers/helloworld.erl -pa _build/default/lib/rest_server/ebin/
erl -boot start_sasl -pa _build/default/lib/*/ebin \
    -pa priv -eval "application:start(rest_server)" \
    -config ebin/sys.config
