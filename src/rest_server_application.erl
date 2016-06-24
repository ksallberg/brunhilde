-module(rest_server_application).
-author('kristian@purestyle.se').

-behaviour(application).
-export([start/2, stop/1]).

-include("include/erlrest.hrl").

-spec start(any(), term()) -> {ok, pid()}
                           |  {ok, pid(), term()}
                           |  {error, any()}.
start(_Type, _Args) ->
    ets:new(erlrest_global_memory, [public, set, named_table]),
    {ok, [{erlrest_servers, ServLs}]} = file:consult("servers.conf"),
    %% Convert settings file to #server{} records
    Servers = lists:map(fun({Name, Encoding, Port}) ->
                                #server{name     = Name,
                                        encoding = Encoding,
                                        port     = Port}
                        end, ServLs),
    rest_server_supervisor:start_link(Servers).

-spec stop(any()) -> ok.
stop(_State) ->
    ets:delete(erlrest_global_memory),
    ok.
