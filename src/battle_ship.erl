-module(battle_ship).
-export([new_board/0, add_shots/2, to_visual_ships/1,
         add_shot/2, to_binary/1, to_visual/1, finished/1]).

%% {x,y}
-record(board, { ship_list :: [ship()]
               , size      :: integer()
               , shots     :: [coordinate()]
               }).

%% @see http://stackoverflow.com/questions/15840842/erlang-records-with-
%%      both-type-and-value-restrictions-as-well-as-default-values
-type coordinate() :: {integer(), integer()}.
-type ship()       :: [coordinate()].
-type board()      :: #board{}.
-type matrix()     :: [[any()]].
%% _______ MATRIX FUNCTIONS ________
%% author: https://github.com/majelbstoat/Morgana/blob/master/src/matrix.erl
-spec new_matrix(integer(), integer(), any()) -> matrix().
new_matrix(Columns, Rows, ContentGenerator) ->
    [[ContentGenerator(Column, Row, Columns, Rows) ||
      Column <- lists:seq(1, Columns)] || Row <- lists:seq(1, Rows)].

-spec element_set(integer(), integer(), any(), matrix()) -> matrix().
element_set(ElementColumn, ElementRow, Value, Matrix) ->
    {Width, Height} = {length(lists:nth(1, Matrix)), length(Matrix)},
    new_matrix(Width, Height, fun(Column, Row, _, _) ->
            case (Column == ElementColumn) andalso (Row == ElementRow) of
                true ->
                    Value;
                false ->
                    lists:nth(Column, lists:nth(Row, Matrix))
            end
        end).
%% _______ END OF MATRIX FUNCTIONS _______

-spec new_board() -> board().
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
-spec hard_coded_board() -> board().
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
-spec add_shots([[integer()]], board()) -> board().
add_shots(Shots, Board) ->
    lists:foldl(fun(Shot, AccBoard) ->
                    add_shot(Shot, AccBoard)
                end, Board, Shots).

-spec add_shot([integer()], board()) -> board().
add_shot([XCord, YCord], #board{shots=Shots}=Board) ->
    Board#board{shots=Shots++[{XCord, YCord}]}.

%% If all ship coords are part of the shot list, then all ships are sunk.
-spec finished(board()) -> binary().
finished(#board{ship_list=Ships, shots=Shots}) ->
    All = lists:all(fun(ShipCoord) ->
                        lists:member(ShipCoord, Shots)
                    end, lists:flatten(Ships)),
    case All of
        true -> <<"yes">>;
        false -> <<"no">>
    end.

-spec ship_is_sunk(ship(), [coordinate()]) -> {sunk | seaworthy, ship()}.
ship_is_sunk(Ship, Shots) ->
    case lists:all(fun(Coord) -> lists:member(Coord, Shots) end, Ship) of
        true  -> {sunk, Ship};
        false -> {seaworthy, Ship}
    end.

-spec repr_for_shot(coordinate(), [{atom(), ship()}]) -> string().
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
-spec check_sunk_ships([ship()], [coordinate()]) -> [{atom(), ship()}].
check_sunk_ships(ShipList, Shots) ->
    [ship_is_sunk(Ship, Shots) || Ship <- ShipList].

-spec to_visual(board()) -> [string()].
to_visual(#board{ship_list=Ls, size=Size, shots=Shots}) ->
    Matrix      = new_matrix(Size, Size, fun(_,_,_,_) -> "~" end),
    ShipListMod = check_sunk_ships(Ls, Shots),
    ShotsAdded  = lists:foldl(fun({X, Y}, AccMatrix) ->
                                  Repr = repr_for_shot({X, Y}, ShipListMod),
                                  element_set(X + 1, Y + 1, Repr, AccMatrix)
                              end, Matrix, Shots),
                              ShotsAdded,
    [lists:concat(Line) || Line <- ShotsAdded].

-spec to_visual_ships(board()) -> [string()].
to_visual_ships(#board{ship_list=Ls, size=Size, shots=Shots}) ->
    Matrix      = new_matrix(Size, Size, fun(_,_,_,_) -> "~" end),
    MatrixShips = lists:foldl(fun({X, Y}, AccMatrix) ->
                                  element_set(X + 1, Y + 1, "o", AccMatrix)
                              end, Matrix, lists:flatten(Ls)),
    ShipListMod = check_sunk_ships(Ls, Shots),
    ShotsAdded  = lists:foldl(fun({X, Y}, AccMatrix) ->
                                  Repr = repr_for_shot({X, Y}, ShipListMod),
                                  element_set(X + 1, Y + 1, Repr, AccMatrix)
                              end, MatrixShips, Shots),
                              ShotsAdded,
    [lists:concat(Line) || Line <- ShotsAdded].

-spec to_binary([string()]) -> [binary()].
to_binary(Board) -> [list_to_binary(Line) || Line <- Board].
