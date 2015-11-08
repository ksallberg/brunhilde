%% Credits: http://learnyousomeerlang.com/buckets-of-sockets

-module(tcp_supervisor).
-behavior(supervisor).

-export([start_link/0, start_socket/0, empty_listeners/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, Port} = application:get_env(port),
    %% Set the socket into {active_once} mode.
    %% See sockserv_serv comments for more details
    {ok, ListenSocket} = gen_tcp:listen(Port,
                                        [list,
                                         {packet, 0},
                                         {reuseaddr, true},
                                         {keepalive, true},
                                         {backlog, 30}]
                                       ),
    spawn_link(fun empty_listeners/0),
    {ok, {{simple_one_for_one, 10, 60},
          [{tcp_supervisor,
            {tcp_server, start_link, [ListenSocket]}, % pass the socket!
            temporary, 1000, worker, [tcp_server]}
          ]}}.

start_socket() ->
    supervisor:start_child(?MODULE, []).

%% Start with 20 listeners so that many multiple connections can
%% be started at once, without serialization. In best circumstances,
%% a process would keep the count active at all times to insure nothing
%% bad happens over time when processes get killed too much.
empty_listeners() ->
    [start_socket() || _ <- lists:seq(1,20)],
    ok.
