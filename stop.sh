#!/bin/bash
erl -noshell -sname stopper -setcookie adm -pa ebin -eval "commands:stop()"
