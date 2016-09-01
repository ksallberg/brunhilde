-module(stats_supervisor).

-behavior(supervisor).

-include("include/erlrest.hrl").

-export([start_link/1,
         init/1]).

start_link(Servers) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Servers]).

init([Servers]) ->
    lists:foreach(fun start_server/1, Servers),
    ChildSpec = [#{id       => stats_supervisor,
                   start    => {stats_server,
                                start_link,
                                []},
                   restart  => transient,
                   shutdown => 1000,
                   type     => worker,
                   modules  => [stats_server]}],
    SupFlags = #{strategy  => simple_one_for_one,
                 intensity => 10,
                 preiod    => 60},
    {ok, {SupFlags, ChildSpec}}.

start_server(Server) ->
    SpawnFun = fun () ->
                       supervisor:start_child(?MODULE, [Server])
               end,
    spawn_link(SpawnFun).
