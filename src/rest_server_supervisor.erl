%% Top level supervisor.

-module(rest_server_supervisor).
-behavior(supervisor).

-include("include/erlrest.hrl").

-export([start_link/1,
         init/1]).

start_link(Servers) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Servers]).

init([Servers]) ->
    TCPSup = #{id       => rest_tcp_supervisor,
               start    => {tcp_supervisor,
                            start_link,
                            [Servers]},
               restart  => permanent,
               shutdown => 1000,
               type     => supervisor,
               modules  => [tcp_supervisor]},

    TrackerSup = #{id       => rest_tracker_supervisor,
                   start    => {tracker_supervisor,
                                start_link,
                                [Servers]},
                   restart  => permanent,
                   shutdown => 1000,
                   type     => supervisor,
                   modules  => [tracker_supervisor]},

    SupFlags = #{strategy  => one_for_one,
                 intensity => 10,
                 preiod    => 60},

    {ok, {SupFlags, [TCPSup, TrackerSup]}}.
