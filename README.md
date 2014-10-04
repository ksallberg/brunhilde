erlrest
=======

Erlang/OTP rest server listening for user specified routes.
<br/>
Communicating in json.
<br/>
<h3>Requires:</h3>
erlang-jiffy
<br/>
<h3>Usage:</h3>
make all
<br/>
. start.sh
<br/>
<h5>To stop:</h5>
<strong>
Eshell V6.0  (abort with ^G)<br/>
1> application:stop(rest_server).<br/>
ok
</strong>
<h3>Info:</h3>
rest_server_app :: OTP application<br/>
route_handler   :: Normal erl file<br/>
battle_ship     :: Normal erl file<br/>
tcp_listener    :: OTP gen_server<br/>
tcp_reply_fsm   :: OTP gen_fsm<br/>
<br/>
OTP design (and a lot of the code) taken from:<br/>
http://erlangcentral.org/wiki/index.php?title=Building_a_Non-blocking_TCP_server_using_OTP_principles
