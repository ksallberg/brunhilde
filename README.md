brunhilde
=======

brunhilde is a minimal Erlang/OTP web server.

It can be embedded, or used standalone.

# Dependencies:
* rebar3 (https://www.rebar3.org/)

# Usage:

To start brunhilde standalone, do:
```
make build start
```

For an example of a site using brunhilde see:
[purestyle](https://github.com/ksallberg/purestyle).

# Dialyzer:
make dialyzer (requires existing PLT)

# Publishing to hex

1) Update the version number in src/brunhilde.app.src
2) Push the tag to git
3) "make build"
4) rebar3 hex publish
