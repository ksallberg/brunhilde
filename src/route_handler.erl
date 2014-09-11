-module(route_handler).
-export([match/2]).

match("/something/somethingelse/", Json) ->
    %% look at the Json, do something, and reply with a new json
    {Json};

match("/some/route/other", Json) ->
    {Json};

match("/many/more/routes", Json) ->
    {Json};

match(_, _) ->
    error_no__route_matching.
