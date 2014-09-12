-module(route_handler).
-export([match/2]).
-import(battle_ship, [new_board/0]).

%% Get the player name from Json,
%% Register the player, OR error
match("/battleship/register/", {[Json]}) ->
    %% look at the Json, do something, and reply with a new json
    {_PlayerNameTag, PlayerName} = Json,
    _NewBoard = battle_ship:new_board(),
    {[{<<"welcome">>, PlayerName}]};

%% Get the player name from Json,
%% Find its corresponding game board,
%% Shoot and update the game board, OR error
match("/battleship/shoot/", Json) ->
    Json;

%% returns the game board
match("/battleship/radar/", Json) ->
    Json;

%% Clear the database and create a new round
%% of battleship with no players
match("/battleship/reset/", Json) ->
    Json;

match("/battleship/info/", _) ->
    <<"This is a battleship game to show how to use erlrest">>;

%% Return a json object telling the client it
%% is requesting a non-existing route.
match(_, _) ->
    {error, no_route_matching}.
