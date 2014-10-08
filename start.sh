#!/bin/bash
erl -noshell -sname rest_server -setcookie adm -pa ebin -eval "commands:start()" &
