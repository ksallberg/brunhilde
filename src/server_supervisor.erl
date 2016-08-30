%% Supervising a specific rest server instance.

-module(server_supervisor).
-behavior(supervisor).

-include("include/erlrest.hrl").

-export([start_link/1,
         init/1]).

start_link(Servers) ->
    ?liof("aaaa~n", []),
    supervisor:start_link({local, ?MODULE}, ?MODULE, Servers).

%% This supervisor starts a tcp_supervisor
%% for each server defined by users.
init(Servers) ->
    io:format("hej2..~p ~n", [Servers]),
    _ChildSpec = {server_supervisor,
                 {tcp_supervisor, start_link, []},
                 permanent, 1000, supervisor, [tcp_supervisor]},

    Ch2 = [{server_supervisor,
            {tcp_supervisor, start_link, []},
            transient, 1000, supervisor, [tcp_supervisor]}],

    %% Need to spawn the next supervisor (tcp_supervisor)
    %% in a separate process not to block here
    lists:foreach(fun(Server) ->
                          SpawnFun = fun () ->
                                             supervisor:start_child(?MODULE,
                                                                    [Server])
                                     end,
                          spawn_link(SpawnFun)
                  end, Servers),

    {ok, {{simple_one_for_one, 10, 60}, Ch2}}.
