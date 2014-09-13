-module(battle_ship).
-export([new_board/0, only_water/0, add_shots/2, add_shot/2]).

new_board() -> hard_coded_board().

%% A 10x10 board of water only. Note that the string is a list of chars
only_water() ->
    ["~~~~~~~~~~",
     "~~~~~~~~~~",
     "~~~~~~~~~~",
     "~~~~~~~~~~",
     "~~~~~~~~~~",
     "~~~~~~~~~~",
     "~~~~~~~~~~",
     "~~~~~~~~~~",
     "~~~~~~~~~~",
     "~~~~~~~~~~"].

hard_coded_board() ->
    ["~~~~~~~o~~",
     "~o~o~~~~~~",
     "~o~~~~oo~~",
     "~o~~o~~~~~",
     "~o~~o~o~~~",
     "~~~~~~~~~~",
     "~~o~~~~~~~",
     "~~o~~~ooo~",
     "~~o~o~~~~~",
     "~~~~o~o~~~"].

%% Test: battle_ship:add_shots([[0,0], [0,1], [0,2], [9,9]], Board).
add_shots(Shots, Board) ->
    lists:foldl(fun(Shot, AccBoard) ->
                    add_shot(Shot, AccBoard)
                end, Board, Shots).

add_shot([XCord, YCord], Board) ->
    {A, LinesAfter} = lists:split(YCord+1, Board),
    LinesBefore     = lists:sublist(A, length(A) - 1),
    LineToMod       = lists:last(A),
    {B,  ChsAfter}  = lists:split(XCord+1, LineToMod),
    ChsBefore       = lists:sublist(B, length(B) - 1),
    LinesBefore ++ [ChsBefore ++ "x" ++ ChsAfter] ++ LinesAfter.
