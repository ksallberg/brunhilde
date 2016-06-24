%% Top level supervisor.

-module(rest_server_supervisor).
-behavior(supervisor).

-include("include/erlrest.hrl").

-export([start_link/1,
         init/1]).

start_link(Servers) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Servers]).

%% This supervisor starts a tcp_supervisor
%% for each server defined by users.
init([Servers]) ->
    {ok, {{one_for_one, 10, 60},
          [{rest_server_supervisor,
            {tcp_supervisor, start_link, [Servers]},
            permanent, 1000, supervisor, [tcp_supervisor]}
          ]}}.
