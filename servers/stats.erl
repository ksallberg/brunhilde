-module(stats).

-export([ init/0
        , routes/0]).

-behaviour(http_handler).

%% Called upon the start of the server, server
%% can do whatever it wishes do to here.
-spec init() -> atom().
init() ->
    ok.

routes() ->
    [ {json, get, "/",            fun handle_stats/2}
    , {file, get, "/stats.html",  fun handle_file/2}
    , {file, get, "/favicon.ico", fun handle_icon/2}
    , {'*',                       fun handle_wildcard/2}].

handle_stats(_Data, _Parameters) ->
    Stats = tracker_server:get_stats(),
    FormatFun = fun({Name, Connections}) ->
                        #{<<"server_name">> => atom_to_binary(Name, utf8),
                          <<"total_connections_handled">> => Connections}
                end,
    #{<<"stats">> => lists:map(FormatFun, Stats)}.

handle_file(_, _) ->
    {ok, Binary} = file:read_file("static/stats.html"),
    Binary.

handle_icon(_, _) ->
    {ok, Binary} = file:read_file("static/favicon.ico"),
    Binary.

handle_wildcard(_Data, _Parameters) ->
    <<"404">>.
