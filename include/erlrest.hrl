-define(liof(M, X), io:format("~s:~B: ~s ~p~n", [?MODULE, ?LINE, M, X])).
-define(l2b(V), list_to_binary(V)).

-define(COLLECT_STATS, 0 bsl 1).

-define(flag_set, fun(F, Fs) ->
                          F band Fs == F
                  end).
