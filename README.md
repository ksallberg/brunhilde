erlrest
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
. start.sh
<h5>To stop:</h5>
1> application:stop(rest_server).
<h3>Info:</h3>
rest_server_app :: OTP application<br/>
route_handler   :: Normal erl file<br/>
battle_ship     :: Normal erl file<br/>
tcp_listener    :: OTP gen_server<br/>
tcp_reply       :: OTP gen_server<br/>
<br/>
OTP design (and a lot of the code) taken from:<br/>
http://erlangcentral.org/wiki/index.php?title=Building_a_Non-blocking_TCP_server_using_OTP_principles
