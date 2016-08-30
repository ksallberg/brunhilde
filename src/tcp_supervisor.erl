-module(tcp_supervisor).
-behavior(supervisor).

-include("include/erlrest.hrl").

-export([start_link/1,
         init/1,
         start_socket/2]).

start_link(Server) ->
    io:format("tcp_sup start_link: ~p~n", [Server]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, Server).

init(Server) ->
    io:format("hej3..~p ~n", [get_server_id(Server)]),
    start_server(Server),
    {ok, {{simple_one_for_one, 10, 60},
          [{get_server_id(Server),
            {tcp_server, start_link, []},
            transient, 1000, worker, [tcp_server]}
          ]}}.

get_server_id(#server{name = Name}) ->
    ?l2a(?a2l(Name) ++ "_sup").

start_server(#server{name     = Name,
                     encoding = _Encoding,
                     port     = Port,
                     workers  = Workers
                    } = Server) ->
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
                        || _ <- lists:seq(1, Workers)],
                       ok
               end,
    spawn_link(SpawnFun).

start_socket(ListenSocket, Server) ->
    supervisor:start_child(get_server_id(Server), [{ListenSocket, Server}]).

emit_terminal_box(Port) ->
    PortStr = lists:flatten(io_lib:format("~p", [Port])),
    Msg = lists:flatten(io_lib:format("% Erlrest started. "
                                      "Listening at port: ~s. %", [PortStr])),
    Line = [$% || _ <- lists:seq(1, length(Msg))],
    io:format("~n~s~n", [Line]),
    io:format("~s~n",   [Msg]),
    io:format("~s~n",   [Line]).
