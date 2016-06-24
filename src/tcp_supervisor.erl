-module(tcp_supervisor).
-behavior(supervisor).

-include("include/erlrest.hrl").

-export([start_link/1,
         init/1,
         start_socket/2]).

start_link(Servers) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Servers]).

init([Servers]) ->
    lists:foreach(fun start_server/1, Servers),
    {ok, {{simple_one_for_one, 10, 60},
          [{tcp_supervisor,
            {tcp_server, start_link, []},
            temporary, 1000, worker, [tcp_server]}
          ]}}.

start_server(#server{name     = Name,
                     encoding = _Encoding,
                     port     = Port} = Server) ->
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
                       [start_socket(ListenSocket, Server)
                        || _ <- lists:seq(1,10)],
                       ok
               end,
    spawn_link(SpawnFun).

start_socket(ListenSocket, Server) ->
    supervisor:start_child(?MODULE, [{ListenSocket, Server}]).

emit_terminal_box(Port) ->
    PortStr = lists:flatten(io_lib:format("~p", [Port])),
    Msg = lists:flatten(io_lib:format("% Erlrest started. "
                                      "Listening at port: ~s. %", [PortStr])),
    Line = [$% || _ <- lists:seq(1, length(Msg))],
    io:format("~n~s~n", [Line]),
    io:format("~s~n",   [Msg]),
    io:format("~s~n",   [Line]).
