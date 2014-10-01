-module(battle_ship).
-export([new_board/0, add_shots/2,
         add_shot/2, to_binary/1, to_visual/1, finished/1]).

%% {x,y}
-record(board, { ship_list :: [[{integer(), integer()}]]
               , size      :: integer()
               , shots     :: [{integer(), integer()}]
               }).

new_board() -> hard_coded_board().

% ~~~~~~~o~~
% ~o~o~~~~~~
% ~o~~~~oo~~
% ~o~~o~~~~~
% ~o~~o~o~~~
% ~~~~~~~~~~
% ~~o~~~~~~~
% ~~o~~~ooo~
% ~~o~~~~~~~
% ~~~ooo~~~~
hard_coded_board() ->
    #board{ship_list = [[{1,1}, {1,2}, {1,3}, {1,4}],
                        [{8,0}],
                        [{3,1}],
                        [{4,3}, {4,4}],
                        [{6,4}],
                        [{6,2}, {7,2}],
                        [{2,6}, {2,7}, {2,8}],
                        [{6,7}, {7,7}, {8,7}],
                        [{3,9}, {4,9}, {5,9}]],
           size  = 10,
           shots = []}.

%% Test: battle_ship:add_shots([[0,0], [0,1], [0,2], [9,9]], Board).
add_shots(Shots, Board) ->
    lists:foldl(fun(Shot, AccBoard) ->
                    add_shot(Shot, AccBoard)
                end, Board, Shots).

add_shot([XCord, YCord], #board{shots=Shots}=Board) ->
    Board#board{shots=Shots++[{XCord, YCord}]}.

%% If all ship coords are part of the shot list, then all ships are sunk.
finished(#board{ship_list=Ships, shots=Shots}) ->
    All = lists:all(fun(ShipCoord) ->
                        lists:member(ShipCoord, Shots)
                    end, lists:flatten(Ships)),
    case All of
        true -> <<"yes">>;
        false -> <<"no">>
    end.

ship_is_sunk(Ship, Shots) ->
    case lists:all(fun(Coord) -> lists:member(Coord, Shots) end, Ship) of
        true  -> {sunk, Ship};
        false -> {seaworthy, Ship}
    end.

repr_for_shot({_X,_Y}=Shot, ShipListMod) ->
    CoordLs = [[{Coord, ShipStatus} || Coord <- Ship]
                                    || {ShipStatus, Ship} <- ShipListMod],
    case lists:keyfind(Shot, 1, lists:flatten(CoordLs)) of
        false -> % shot hit the water
            "m";
        {_, sunk} -> % ship is totally sunk
            "s";
        {_, seaworthy} -> % ship has been hit but is still seaworthy
            "h"
    end.

% get a ship list, and some shots, return [{alive, [{x,y}]}]
check_sunk_ships(ShipList, Shots) ->
    [ship_is_sunk(Ship, Shots) || Ship <- ShipList].

to_visual(#board{ship_list=Ls, size=Size, shots=Shots}) ->
    Matrix      = matrix:new(Size, fun(_,_,_,_) -> "~" end),
    ShipListMod = check_sunk_ships(Ls, Shots),
    ShotsAdded  = lists:foldl(fun({X, Y}, AccMatrix) ->
                                  Repr = repr_for_shot({X, Y}, ShipListMod),
                                  matrix:element_set(X + 1,
                                                     Y + 1,
                                                     Repr,
                                                     AccMatrix)
                              end, Matrix, Shots),
                              ShotsAdded,
    [lists:concat(Line) || Line <- ShotsAdded].

to_binary(Board) ->
    [list_to_binary(Line) || Line <- Board].
