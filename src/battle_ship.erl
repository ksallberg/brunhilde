-module(battle_ship).
-export([new_board/0]).

new_board() -> only_water().

%% A 10x10 board of water only. Note that the string is a list of chars
only_water() ->
    [["~~~~~~~~~~"],
     ["~~~~~~~~~~"],
     ["~~~~~~~~~~"],
     ["~~~~~~~~~~"],
     ["~~~~~~~~~~"],
     ["~~~~~~~~~~"],
     ["~~~~~~~~~~"],
     ["~~~~~~~~~~"],
     ["~~~~~~~~~~"],
     ["~~~~~~~~~~"]
    ].
