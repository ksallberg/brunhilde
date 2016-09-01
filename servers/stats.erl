-module(stats).
-export([match/4, init/0]).

-behaviour(rest_handler).

%% Called upon the start of the server, server
%% can do whatever it wishes do to here.
-spec init() -> atom().
init() ->
    ok.

-spec match(atom(), string(), tuple() | atom(), [{atom(), atom()}]) -> tuple().

match(get, _WhateverPath, _Whatever, _Parameters) ->
    Stats = tracker_server:get_stats(),
    FormatFun = fun({Name, Connections}) ->
                           #{<<"server name">> => atom_to_binary(Name, utf8),
                             <<"total connections handled">> => Connections}
                end,
    #{<<"stats">> => lists:map(FormatFun, Stats)}.
