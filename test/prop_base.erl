-module(prop_base).

-include_lib("proper/include/proper.hrl").

-export([ prop_test/0 ]).

%% rebar3 proper
%% rebar3 proper -n 10000
%% rebar3 proper -p prop_test
%% rebar3 proper -n 1000 -p prop_test

%% make start
%% > proper_gen:pick(proper_types:string()).
%% > proper_gen:pick(prop_base:prop_test()).

prop_test() ->
    ?FORALL(Some, term(),
            begin
                Some == Some
            end).
