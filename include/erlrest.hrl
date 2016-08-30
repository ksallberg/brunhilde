-define(liof(M, X), io:format("~s:~B: ~s ~p~n", [?MODULE, ?LINE, M, X])).
-define(l2b(V), list_to_binary(V)).
-define(l2a(V), list_to_atom(V)).
-define(a2l(V), atom_to_list(V)).

-record(server, { name      :: atom(),
                  encoding  :: atom(),
                  port      :: integer(),
                  workers   :: integer()
                }).
