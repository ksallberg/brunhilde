-define(liof(M, X), io:format("~s:~B: ~s ~p~n", [?MODULE, ?LINE, M, X])).
-define(l2b(V), list_to_binary(V)).
-define(b2l(V), binary_to_list(V)).
-define(a2l(V), atom_to_list(V)).

-type handler_fun() ::
    fun((binary(), [binary()], [binary()], atom()) -> binary() | map()).

-record(route, { verb            :: atom()
               , address         :: binary()
               , subdomain = '*' :: atom() | binary()
               , callback        :: handler_fun()
               }).
