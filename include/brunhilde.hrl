-define(liof(M, X), io:format("~s:~B: ~s ~p~n", [?MODULE, ?LINE, M, X])).
-define(l2b(V), list_to_binary(V)).
-define(b2l(V), binary_to_list(V)).
-define(a2l(V), atom_to_list(V)).

-define(START_OBSERVER, 1 bsl 1).
-define(USE_RELOADER,   1 bsl 2).

-define(flag_set, fun(F, Fs) ->
                          F band Fs == F
                  end).

-type brunhilde_proto() :: html | file.

-record(route, { protocol        :: brunhilde_proto()
               , verb            :: atom()
               , address         :: binary()
               , subdomain = '*' :: atom() | binary()
               , callback        :: term()
               }).
