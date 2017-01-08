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
    [ {json, get, "/",            fun handle_stats/4}
    , {file, get, "/stats.html",  fun handle_file/4}
    , {file, get, "/favicon.ico", fun handle_icon/4}
    , {'*',                       fun handle_wildcard/4}].

handle_stats(_Data, _Parameters, _Headers, _InstanceName) ->
    Stats = tracker_server:get_stats(),
    FormatFun = fun({Name, Connections}) ->
                        #{<<"instance_name">> => atom_to_binary(Name, utf8),
                          <<"total_connections_handled">> => Connections}
                end,
    #{<<"stats">> => lists:map(FormatFun, Stats)}.

handle_file(_, _, _Headers, _InstanceName) ->
    {ok, Binary} = file:read_file("static/stats.html"),
    Binary.

handle_icon(_, _, _Headers, _InstanceName) ->
    {ok, Binary} = file:read_file("static/favicon.ico"),
    Binary.

handle_wildcard(_Data, _Parameters, _Headers, _InstanceName) ->
    <<"404">>.
