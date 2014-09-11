erlrest
=======

Erlang/OTP rest server listening for user specified routes.
<br/>
Communicating in json.
<br/>
<h3>Usage:</h3>
make all
<br/>
In erlang shell:
<br/>
<strong>
Eshell V6.0  (abort with ^G)<br/>
1> application:start(rest_server).<br/>
ok
</strong>
<h3>Info:</h3>
rest_server_app.erl :: OTP application<br/>
route_handler       :: Normal erl file<br/>
tcp_listener        :: OTP gen_server<br/>
tcp_reply_fsm       :: OTP gen_fsm<br/>
