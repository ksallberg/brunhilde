brunhilde (tested with Erlang OTP R19, R20)
=======

![alt tag](static/brunhilde.jpg)

Serving many users concurrently is what brunhilde does
for a living!

brunhilde is a minimal (~715 LOC) Erlang/OTP web server,
designed for rapid prototyping and hackathons/coding
competitions. As simple as possible, so that it can
quickly be modified or debugged. It is not designed
to provide high performance or be used in production.

# Dependencies:
* rebar3 (https://www.rebar3.org/)
* jsx: fetched from rebar3
* erlydtl: fetched from rebar3

# Usage:

To start brunhilde standalone, do:
```
make build start
```

To embed brunhilde in another OTP application, see:
[brunhilde_ext](https://github.com/ksallberg/brunhilde_ext).

For an example of a small (but covering POST, GET, redirects, etc), see:
[musiklistan](https://github.com/ksallberg/musiklistan).


# Dialyzer:
make dialyzer (requires existing PLT)

# Running virtual servers/apps:

Virtual servers are defined in servers/ and have to use the
rest_handler behaviour. Give a list of virtual servers to
start in brunhilde.conf.

For example:
```erlang
#{start_observer => false,
  start_debugger => false,
  use_reloader   => true,
  servers =>
   [ #{server_name   => battleship,
       instance_name => battleship_serv,
       port          => 28251,
       workers       => 10,
       transport     => http}

   , #{server_name   => helloworld,
       instance_name => helloworld_serv,
       port          => 5030,
       workers       => 5,
       transport     => http}

   , #{server_name   => helloworld,
       instance_name => helloworld_serv2,
       port          => 5031,
       workers       => 10,
       transport     => http}

   , #{server_name   => secure,
       instance_name => secure_serv,
       port          => 4430,
       workers       => 10,
       transport     => {https,
                        "cert.pem",
                        "privkey.pem",
                        "fullchain.pem"
                        }
      }
   ]}.
```

# Supervisor tree:
![alt tag](static/sup_tree.png)

# Publishing to hex

1) Update the version number in src/brunhilde.app.src
2) Push the tag to git
3) "make build"
4) rebar3 hex publish
