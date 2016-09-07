-module(tcp_supervisor).
-behavior(supervisor).

-include("include/erlrest.hrl").

-export([start_link/2,
         init/1,
         start_socket/3]).

start_link(Servers, Flags) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Servers, Flags]).

init([Servers, Flags]) ->
    Servers1 = [{Server, Flags} || Server <- Servers],
    lists:foreach(fun start_server/1, Servers1),
    ChildSpec = [#{id       => tcp_supervisor,
                   start    => {tcp_server,
                                start_link,
                                []},
                   restart  => transient,
                   shutdown => 1000,
                   type     => worker,
                   modules  => [tcp_server]}],
    SupFlags = #{strategy  => simple_one_for_one,
                 intensity => 10,
                 preiod    => 60},
    {ok, {SupFlags, ChildSpec}}.

start_server({#{name     := Name,
                port     := Port,
                workers  := Workers
               } = Server,
              Flags}) ->
    emit_terminal_box(Port),
    %% Initialize the server
    erlang:apply(Name, init, []),
    {ok, ListenSocket} = gen_tcp:listen(Port,
                                        [list,
                                         {packet, 0},
                                         {reuseaddr, true},
                                         {keepalive, true},
                                         {backlog, 30}]
                                       ),
    SpawnFun = fun() ->
                       [start_socket(ListenSocket, Server, Flags)
                        || _ <- lists:seq(1, Workers)],
                       ok
               end,
    spawn_link(SpawnFun).

start_socket(ListenSocket, Server, Flags) ->
    supervisor:start_child(?MODULE, [ListenSocket, Server, Flags]).

emit_terminal_box(Port) ->
    PortStr = lists:flatten(io_lib:format("~p", [Port])),
    Msg = lists:flatten(io_lib:format("% Erlrest started. "
                                      "Listening at port: ~s. %", [PortStr])),
    Line = [$% || _ <- lists:seq(1, length(Msg))],
    io:format("~n~s~n", [Line]),
    io:format("~s~n",   [Msg]),
    io:format("~s~n",   [Line]).
