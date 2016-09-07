erlrest (tested with Erlang OTP R19)
=======

Erlang/OTP rest server listening for user specified routes.

#Requires:
* rebar3 (https://www.rebar3.org/)
* jsx: git clone through rebar3

#Usage:

```
rebar3 compile
./start.sh
```

#Dialyzer:
dialyzer --src src/

#Running virtual servers/apps:

Virtual servers are defined in servers/ and have to use the
rest_handler behaviour. Give a list of virtual servers to
start in servers.conf.

For example:
```erlang
#{collect_stats  => true,
  start_observer => false,
  servers =>
   [#{name     => battleship,
      port     => 28251,
      workers  => 10},
    #{name     => helloworld,
      port     => 5030,
      workers  => 5},
    #{name     => stats,
      port     => 8080,
      workers  => 2}
   ]}.
```

#Supervisor tree:
![alt tag](doc/sup_tree.png)
