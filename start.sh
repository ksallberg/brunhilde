#!/bin/bash
erl -noshell -sname rest_server -setcookie adm -boot start_sasl -pa ebin -eval "commands:start()" &
