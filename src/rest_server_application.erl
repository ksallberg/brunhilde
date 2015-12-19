-module(rest_server_application).
-author('kristian@purestyle.se').

-behaviour(application).
-export([start/2, stop/1]).

-spec start(any(), term()) -> {ok, pid()}
                           |  {ok, pid(), term()}
                           |  {error, any()}.
start(_Type, _Args) ->
    ets:new(erlrest_global_memory, [public, set, named_table]),
    %% let the user defined module do initialization
    route_handler:init(),
    tcp_supervisor:start_link().

-spec stop(any()) -> ok.
stop(_State) ->
    ets:delete(erlrest_global_memory),
    ok.
