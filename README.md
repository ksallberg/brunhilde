erlrest (tested with Erlang OTP R19)
=======

Erlang/OTP rest server listening for user specified routes.
<br/>
Communicating in json.
<h3>Requires:</h3>
rebar:<br>
sudo apt-get install rebar (or equivalent)
<h3>Uses:</h3>
erlang-jiffy (automatic git clone through rebar)
<h3>Usage:</h3>
rebar get-deps
<br/>
rebar compile
<br/>
./start.sh
<h5>To stop:</h5>
1> application:stop(rest_server).
<h3>Dialyzer:</h3>
dialyzer --src src/
<br/>
Port is specified in ebin/rest_server.app
<br/>
The idea is to specify REST routes in src/route_handler.erl
<br>