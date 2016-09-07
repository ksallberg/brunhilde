-module(helloworld).

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
    [{json, get, "/helloworld/hello/", fun handle_hello/2}].

handle_hello(_Data, _Parameters) ->
    #{<<"hello">> => <<"hello2u">>}.

wildcard(_Data, _Parameters) ->
    <<"i dont know what youre saying">>.
