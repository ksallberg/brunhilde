-module(route_handler).
-export([match/2]).
-import(battle_ship, [new_board/0]).

%% tip: there is an application wide ets table
%% called global_memory that might come in handy here

%% Global memory schema:
%%     {players, [player]}
%%     {game_board, GameBoard}

-record(player,
        { player_name :: string(),
          shots       :: [integer()]
        }).

%% Get the player name from Json,
%% Register the player, OR error
match("/battleship/register/", {[Json]}) ->
    %% look at the Json, do something, and reply with a new json
    {<<"player_name">>, PlayerName} = Json,
    Players   = ets:lookup(global_memory, players),
    NewPlayer = #player{player_name = PlayerName, shots = []},
    case Players of
        [] -> ets:insert(global_memory, {players, [NewPlayer]});
        _  -> [{players, Ls}] = Players,
              ets:insert(global_memory, {players, Ls ++ [NewPlayer]})
    end,
    {[{<<"welcome">>, PlayerName}]};

%% Get the player name from Json,
%% Find its corresponding game board,
%% Shoot and update the game board, OR error
match("/battleship/shoot/", Json) ->
    {[{<<"player_name">>, PlayerName},
      {<<"shoot_at">>, Coordinates}
     ]} = Json,
    [{players, Players}] = ets:lookup(global_memory, players),
    Player  = lists:keyfind(PlayerName, 2, Players),
    case Player of
        false ->
            {[{<<"error">>, <<"no such player">>}]};
        _ ->
            Shots      = Player#player.shots ++ [Coordinates],
            NewPlayer  = Player#player{shots=Shots},
            NewPlayers = lists:keyreplace(PlayerName, 2, Players, NewPlayer),
            ets:insert(global_memory, {players, NewPlayers}),
            {[{<<"ok">>, <<"shot added">>}]}
    end;

%% returns the game board
match("/battleship/radar/", {[{<<"player_name">>, PlayerName}]}) ->
    [{players, Players}] = ets:lookup(global_memory, players),
    Player  = lists:keyfind(PlayerName, 2, Players),
    case Player of
        false ->
            {[{<<"error">>, <<"no such player">>}]};
        _ ->
            [{game_board, OriginalGameBoard}]
                = ets:lookup(global_memory, game_board),
            Shots = Player#player.shots,
            PlayerGameBoard = battle_ship:add_shots(Shots, OriginalGameBoard),
            Hidden = battle_ship:hide_ships(PlayerGameBoard),
            {[{PlayerName, battle_ship:to_binary(Hidden)},
              {<<"won">>, battle_ship:finished(PlayerGameBoard)}]}
    end;

%% FIXME: Not implemented
%% See all players and their current game boards.
match("/battleship/radar_all/", _) ->
    [{game_board, OriginalGameBoard}] = ets:lookup(global_memory, game_board),
    {[{<<"radar for all players">>,
       OriginalGameBoard
      }]};

%% Clear the database and create a new round
%% of battleship with no players
match("/battleship/reset/", {[Json]}) ->
    {<<"password">>, Pw} = Json,
    case Pw of
        <<"pretty please">> ->
            ets:delete_all_objects(global_memory),
            NewBoard  = battle_ship:new_board(),
            ets:insert(global_memory, {game_board, NewBoard}),
            {[{<<"info_text">>, <<"ok, new round">>}]};
        _ ->
            {[{<<"info_text">>, <<"wrong password">>}]}
    end;

match("/battleship/info/", _) ->
    {[{<<"info_text">>,
       <<"This is a battleship game to show how to use erlrest">>}]};

%% Return a json object telling the client it
%% is requesting a non-existing route.
match(_, _) ->
    {[{<<"error">>, <<"no route matching">>}]}.
