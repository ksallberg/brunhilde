-module(secure).

-export([ init/0
        , routes/0]).

-behaviour(http_handler).

-spec init() -> atom().
init() ->
    ok.

routes() ->
    [ {json, get, "/", fun handle_stats/3}
    , {'*',            fun handle_wildcard/3}].

handle_stats(_Data, _Parameters, _Headers) ->
    #{<<"message">> => <<"hello!">>}.

handle_wildcard(_Data, _Parameters, _Headers) ->
    <<"404">>.
