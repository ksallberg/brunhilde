-define(liof(M, X), io:format("~s:~B: ~s ~p~n", [?MODULE, ?LINE, M, X])).

-record(server, { name      :: atom(),
                  encoding  :: atom(),
                  port      :: integer()
                }).
