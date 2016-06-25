erlrest (tested with Erlang OTP R19)
=======

Erlang/OTP rest server listening for user specified routes.

#Requires:
* rebar: sudo apt-get install rebar (or equivalent)
* erlang-jiffy: automatic git clone through rebar

#Usage:

```
rebar get-deps
rebar compile
./start.sh
```

#Dialyzer:
dialyzer --src src/

#Running virtual servers/apps:

Virtual servers are defined in servers/ and have to use the
rest_handler behaviour. Give a list of virtual servers to
start in servers.conf. The format is the following four tuple:

```
{name of rest_handler implementation (available in servers/),

 encoding (currently only json is available through jiffy),

 port,

 amount of worker processes to simultaneously listen for
 connecting clients and handle them.}
```

For example:
```erlang
{erlrest_servers,
 [{battleship, json, 28251, 10},
  {helloworld, json, 5030, 5}
 ]}.
```
