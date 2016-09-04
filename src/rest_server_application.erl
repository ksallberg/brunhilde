-module(rest_server_application).
-author('kristian@purestyle.se').

-behaviour(application).
-export([start/2, stop/1]).

-include("include/erlrest.hrl").

-spec start(any(), term()) -> {ok, pid()}
                           |  {ok, pid(), term()}
                           |  {error, any()}.
start(_Type, _Args) ->
    {ok, [#{collect_stats  := CollectStats,
            start_observer := StartObserver,
            servers        := Servers}]} = file:consult("servers.conf"),
    Flags = mk_flags([ {CollectStats,  ?COLLECT_STATS}
                     , {StartObserver, ?START_OBSERVER}
                     ]),
    rest_server_supervisor:start_link(Servers, Flags).

-spec stop(any()) -> ok.
stop(_State) ->
    ok.

mk_flags([]) ->
    0;
mk_flags([{true, Flag}|Rest]) ->
    Flag bor mk_flags(Rest);
mk_flags([{false, _}|Rest]) ->
    mk_flags(Rest).
