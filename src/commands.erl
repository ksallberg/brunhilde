-module(commands).
-export([start/0, stop/0]).

start() ->
    spawn(fun() -> admin_loop() end).

stop() ->
    {ok, Host} = inet:gethostname(),
    {rest_admin, list_to_atom("rest_server@" ++ Host)} ! stop,
    halt().

admin_loop() ->
    io:format("~nREST server starting ---- ~n"),
    register(rest_admin, self()),
    application:start(rest_server),
    io:format("REST server running ---- ~n"),
    receive
        stop ->
            io:format("REST server stopping ----~n"),
            unregister(rest_admin),
            application:stop(rest_server),
            io:format("REST server stopped ----~n"),
            halt()
    end.
