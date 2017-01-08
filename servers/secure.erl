-module(secure).

-export([ init/1
        , routes/0]).

-behaviour(http_handler).

-spec init(atom()) -> atom().
init(_InstanceName) ->
    ok.

routes() ->
    [ {json, get, "/", fun handle_stats/4}
    , {'*',            fun handle_wildcard/4}].

handle_stats(_Data, _Parameters, _Headers, _InstanceName) ->
    #{<<"message">> => <<"hello!">>}.

handle_wildcard(_Data, _Parameters, _Headers, _InstanceName) ->
    <<"404">>.
