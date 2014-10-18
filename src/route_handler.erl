-module(route_handler).
-export([match/4, init/0]).
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

%% Called upon the start of the server
-spec init() -> atom().
init() ->
    ets:delete_all_objects(global_memory),
    NewBoard  = battle_ship:new_board(),
    ets:insert(global_memory, {game_board, NewBoard}),
    ok.

-spec match(atom(), string(), tuple(), [{atom(), atom()}]) -> tuple().
%% Get the player name from Json,
%% Register the player, OR error
match(post, "/battleship/register/", {[Json]}, _Parameters) ->
    %% look at the Json, do something, and reply with a new json
    {<<"player_name">>, PlayerName} = Json,
    Players   = ets:lookup(global_memory, players),
    NewPlayer = #player{player_name = PlayerName, shots = []},
    case Players of
        [] -> ets:insert(global_memory, {players, [NewPlayer]}),
              {[{<<"status">>, <<"welcome">>}]};
        _  -> [{players, Ls}] = Players,
              case lists:keysearch(PlayerName, #player.player_name, Ls) of
                 false -> ets:insert(global_memory,
                                     {players, Ls ++ [NewPlayer]}),
                          {[{<<"status">>, <<"welcome">>}]};
                 _ -> {[{<<"status">>, <<"error, already registered">>}]}
              end
    end;

%% Get the player name from Json,
%% Find its corresponding game board,
%% Shoot and update the game board, OR error
%% FIXME: Right now assumes the input format is correct, check it is
match(post, "/battleship/shoot/", Json, _Parameters) ->
    {Objs} = Json,
    {_, PlayerName}  = lists:keyfind(<<"player_name">>, 1, Objs),
    {_, Coordinates} = lists:keyfind(<<"shoot_at">>, 1, Objs),
    [{players, Players}] = ets:lookup(global_memory, players),
    Player  = lists:keyfind(PlayerName, 2, Players),
    case Player of
        false ->
            {[{<<"status">>, <<"error, no such plauer">>}]};
        _ ->
            Shots      = Player#player.shots ++ [Coordinates],
            NewPlayer  = Player#player{shots=Shots},
            NewPlayers = lists:keyreplace(PlayerName, 2, Players, NewPlayer),
            ets:insert(global_memory, {players, NewPlayers}),
            {[{<<"status">>, <<"ok, shot added">>}]}
    end;

%% returns the game board
match(post, "/battleship/radar/", {[{<<"player_name">>, PlayerName}]}, _Ps) ->
    [{players, Players}] = ets:lookup(global_memory, players),
    Player  = lists:keyfind(PlayerName, 2, Players),
    case Player of
        false ->
            {[{<<"status">>, <<"error, no such player">>}]};
        _ ->
            [{game_board, OriginalGameBoard}]
                = ets:lookup(global_memory, game_board),
            Shots = Player#player.shots,
            PlayerGameBoard = battle_ship:add_shots(Shots, OriginalGameBoard),
            Visual = battle_ship:to_visual(PlayerGameBoard),
            {[{<<"board">>, battle_ship:to_binary(Visual)},
              {<<"won">>, battle_ship:finished(PlayerGameBoard)}]}
    end;

%% See all players and their current game boards.
match(get, "/battleship/radar_all/", _, Parameters) ->
    io:format("Battleship radar all, parameters! ~p~n", [Parameters]),
    case ets:lookup(global_memory, players) of
        [] -> {[{<<"radar_all">>, <<"no players registered">>}]};
        [{players, Players}] ->
            [{game_board, OriginalGameBoard}] = ets:lookup(global_memory,
                                                           game_board),
            ModPlayers =
                  lists:map(
                      fun(#player{shots=Shots, player_name=PlayerName}) ->
                          PGameBoard =
                              battle_ship:add_shots(Shots, OriginalGameBoard),
                          Visual = battle_ship:to_visual_ships(PGameBoard),
                          {[{<<"player_name">>, PlayerName},
                            {<<"board">>,    battle_ship:to_binary(Visual)},
                            {<<"finished">>, battle_ship:finished(PGameBoard)},
                            {<<"shots">>,    length(Shots)}]}
                      end, Players),
            {[{<<"radar_all">>, ModPlayers}]}
   end;

%% Clear the database and create a new round
%% of battleship with no players
match(post, "/battleship/reset/", {[Json]}, _Parameters) ->
    {<<"password">>, Pw} = Json,
    case Pw of
        <<"pretty please">> ->
            ets:delete_all_objects(global_memory),
            NewBoard  = battle_ship:new_board(),
            ets:insert(global_memory, {game_board, NewBoard}),
            {[{<<"status">>, <<"ok, new round">>}]};
        _ ->
            {[{<<"status">>, <<"error, wrong password">>}]}
    end;

match(get, "/battleship/info/", _Json, _Parameters) ->
    {[{<<"info_text">>,
       <<"This is a battleship game to show how to use erlrest">>}]};

%% Return a json object telling the client it
%% is requesting a non-existing route.
match(_Method, _Route, _Json, _Parameters) ->
    {[{<<"error">>, <<"no route matching">>}]}.
