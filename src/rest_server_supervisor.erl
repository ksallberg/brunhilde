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
    ChildSpec = [#{id       => rest_server_supervisor,
                   start    => {tcp_supervisor,
                                start_link,
                                [Servers]},
                   restart  => permanent,
                   shutdown => 1000,
                   type     => supervisor,
                   modules  => [tcp_supervisor]}],
    SupFlags = #{strategy  => one_for_one,
                 intensity => 10,
                 preiod    => 60},
    {ok, {SupFlags, ChildSpec}}.
