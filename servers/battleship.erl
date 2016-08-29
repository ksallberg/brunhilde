-module(battleship).
-export([match/4, init/0]).

-behaviour(rest_handler).

%% tip: there is an application wide ets table
%% called erlrest_global_memory that might come in handy here

%% Global memory schema:
%%     {players, [player]}
%%     {game_board, GameBoard}

-record(player,
        { player_name :: string(),
          shots       :: [integer()]
        }).

%% Called upon the start of the server
-spec init() -> atom().
init() ->
    ets:delete_all_objects(erlrest_global_memory),
    NewBoard = new_board(),
    ets:insert(erlrest_global_memory, {game_board, NewBoard}),
    ok.

-spec match(atom(), string(), tuple() | atom(), [{atom(), atom()}]) -> tuple().
%% Get the player name from Json,
%% Register the player, OR error
match(post, "/battleship/register/", Json, _Parameters) ->
    %% look at the Json, do something, and reply with a new json
    #{<<"player_name">> := PlayerName} = Json,
    Players    = ets:lookup(erlrest_global_memory, players),
    NewPlayer  = #player{player_name = PlayerName, shots = []},
    WelcomeMsg = #{<<"status">> => <<"welcome">>},
    case Players of
        [] -> ets:insert(erlrest_global_memory, {players, [NewPlayer]}),
              WelcomeMsg;
        _  -> [{players, Ls}] = Players,
              case lists:keysearch(PlayerName, #player.player_name, Ls) of
                 false -> ets:insert(erlrest_global_memory,
                                     {players, Ls ++ [NewPlayer]}),
                          WelcomeMsg;
                 _ -> #{<<"status">> => <<"error, already registered">>}
              end
    end;

%% Get the player name from Json,
%% Find its corresponding game board,
%% Shoot and update the game board, OR error
%% FIXME: Right now assumes the input format is correct, check it is
match(post, "/battleship/shoot/", Json, _Parameters) ->
    PlayerName  = maps:get(<<"player_name">>, Json),
    Coordinates = maps:get(<<"shoot_at">>, Json),
    [{players, Players}] = ets:lookup(erlrest_global_memory, players),
    Player = lists:keyfind(PlayerName, 2, Players),
    case Player of
        false ->
            #{<<"status">> => <<"error, no such player">>};
        _ ->
            Shots      = Player#player.shots ++ [Coordinates],
            NewPlayer  = Player#player{shots=Shots},
            NewPlayers = lists:keyreplace(PlayerName, 2, Players, NewPlayer),
            ets:insert(erlrest_global_memory, {players, NewPlayers}),
            #{<<"status">> => <<"ok, shot added">>}
    end;

%% returns the game board
match(post, "/battleship/radar/", #{<<"player_name">> := PlayerName}, _Ps) ->
    [{players, Players}] = ets:lookup(erlrest_global_memory, players),
    Player  = lists:keyfind(PlayerName, 2, Players),
    case Player of
        false ->
            #{<<"status">> => <<"error, no such player">>};
        _ ->
            [{game_board, OriginalGameBoard}]
                = ets:lookup(erlrest_global_memory, game_board),
            Shots = Player#player.shots,
            PlayerGameBoard = add_shots(Shots, OriginalGameBoard),
            Visual = to_visual(PlayerGameBoard),
            #{<<"board">> => to_binary(Visual),
              <<"won">>   => finished(PlayerGameBoard)}
    end;

%% See all players and their current game boards.
match(get, "/battleship/radar_all/", _, _Parameters) ->
    case ets:lookup(erlrest_global_memory, players) of
        [] -> #{<<"radar_all">> => <<"no players registered">>};
        [{players, Players}] ->
            [{game_board, OriginalGameBoard}]
                = ets:lookup(erlrest_global_memory, game_board),
            ModPlayers =
                  lists:map(
                      fun(#player{shots=Shots, player_name=PlayerName}) ->
                          PGameBoard = add_shots(Shots, OriginalGameBoard),
                          Visual     = to_visual_ships(PGameBoard),
                          #{<<"player_name">> => PlayerName,
                            <<"board">>       => to_binary(Visual),
                            <<"finished">>    => finished(PGameBoard),
                            <<"shots">>       => length(Shots)}
                      end, Players),
            #{<<"radar_all">> => ModPlayers}
   end;

%% Clear the database and create a new round
%% of battleship with no players
match(post, "/battleship/reset/", #{<<"password">> := Pw}, _Parameters) ->
    case Pw of
        <<"pretty please">> ->
            ets:delete_all_objects(erlrest_global_memory),
            NewBoard = new_board(),
            ets:insert(erlrest_global_memory, {game_board, NewBoard}),
            #{<<"status">> => <<"ok, new round">>};
        _ ->
            #{<<"status">> => <<"error, wrong password">>}
    end;

match(get, "/battleship/info/", _Json, _Parameters) ->
    #{<<"info_text">> =>
      <<"This is a battleship game to show how to use erlrest">>};

%% Return a json object telling the client it
%% is requesting a non-existing route.
match(_Method, _Route, _Json, _Parameters) ->
    #{<<"error">> => <<"no route matching">>}.


%%%%%%%%%%%%%%%%%%%%%%%%
%% Battleship helpers %%
%%%%%%%%%%%%%%%%%%%%%%%%

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

%% Test: add_shots([[0,0], [0,1], [0,2], [9,9]], Board).
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
    Matrix      = new_matrix(Size, Size, fun(_,_,_,_) -> "-" end),
    ShipListMod = check_sunk_ships(Ls, Shots),
    ShotsAdded  = lists:foldl(fun({X, Y}, AccMatrix) ->
                                  Repr = repr_for_shot({X, Y}, ShipListMod),
                                  element_set(X + 1, Y + 1, Repr, AccMatrix)
                              end, Matrix, Shots),
                              ShotsAdded,
    [lists:concat(Line) || Line <- ShotsAdded].

-spec to_visual_ships(board()) -> [string()].
to_visual_ships(#board{ship_list=Ls, size=Size, shots=Shots}) ->
    Matrix      = new_matrix(Size, Size, fun(_,_,_,_) -> "-" end),
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
