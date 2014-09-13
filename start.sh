#!/bin/bash
erl -pa ebin -eval "application:start(rest_server)"
