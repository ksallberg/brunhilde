-module(stats).

-export([ init/1
        , routes/0]).

-behaviour(http_handler).

%% Called upon the start of the server, server
%% can do whatever it wishes do to here.
-spec init(atom()) -> atom().
init(_InstanceName) ->
    ok.

routes() ->
    [ {json, get, "/",            fun handle_stats/3}
    , {file, get, "/stats.html",  fun handle_file/3}
    , {file, get, "/favicon.ico", fun handle_icon/3}
    , {'*',                       fun handle_wildcard/3}].

handle_stats(_Data, _Parameters, _Headers) ->
    Stats = tracker_server:get_stats(),
    FormatFun = fun({Name, Connections}) ->
                        #{<<"instance_name">> => atom_to_binary(Name, utf8),
                          <<"total_connections_handled">> => Connections}
                end,
    #{<<"stats">> => lists:map(FormatFun, Stats)}.

handle_file(_, _, _Headers) ->
    {ok, Binary} = file:read_file("static/stats.html"),
    Binary.

handle_icon(_, _, _Headers) ->
    {ok, Binary} = file:read_file("static/favicon.ico"),
    Binary.

handle_wildcard(_Data, _Parameters, _Headers) ->
    <<"404">>.
