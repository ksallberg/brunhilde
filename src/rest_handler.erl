-module(rest_handler).

-callback init() -> atom().

%% protocol, verb, address, callback
-callback routes() ->
    [{atom(), atom(), string(), term()}].

-callback wildcard(term(), term()) ->
    term().
