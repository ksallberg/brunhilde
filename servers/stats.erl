-module(stats).

-export([ init/0
        , routes/0
        , wildcard/2]).

-behaviour(rest_handler).

%% Called upon the start of the server, server
%% can do whatever it wishes do to here.
-spec init() -> atom().
init() ->
    ok.

routes() ->
    [{json, get, "/", fun handle_stats/2}].

handle_stats(_Data, _Parameters) ->
    Stats = tracker_server:get_stats(),
    FormatFun = fun({Name, Connections}) ->
                        #{<<"server name">> => atom_to_binary(Name, utf8),
                          <<"total connections handled">> => Connections}
                end,
    #{<<"stats">> => lists:map(FormatFun, Stats)}.

wildcard(_Data, _Parameters) ->
    <<"404">>.
