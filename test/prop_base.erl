-module(prop_base).

-include_lib("proper/include/proper.hrl").

-export([ prop_subdomain/0 ]).

%% rebar3 proper
%% rebar3 proper -n 10000
%% rebar3 proper -p prop_subdomain
%% rebar3 proper -n 1000 -p prop_subdomain

%% make start
%% > proper_gen:pick(proper_types:string()).
%% > proper_gen:pick(prop_base:prop_subdomain()).

prop_subdomain() ->
    ?FORALL(Proplist, [{atom(), string()}],
            begin
                http_parser:get_subdomain(Proplist) == "*"
            end).
