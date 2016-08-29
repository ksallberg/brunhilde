-module(helloworld).
-export([match/4, init/0]).

-behaviour(rest_handler).

%% Called upon the start of the server, server
%% can do whatever it wishes do to here.
-spec init() -> atom().
init() ->
    ok.

-spec match(atom(), string(), tuple() | atom(), [{atom(), atom()}]) -> tuple().
match(get, "/helloworld/hello/", _Whatever, _Parameters) ->
    #{<<"hello">> => <<"hello2u">>};

match(get, _WhateverPath, _Whatever, _Parameters) ->
    #{<<"hello">> => <<"i dont know what youre saying">>}.
