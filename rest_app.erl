-module(rest_app).
-author('kristian@purestyle.se').

-behaviour(application).

-export([start_client/0]).
-export([start/2, stop/1, init/1]).

-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).
-define(DEF_PORT,    2222).

start_client() ->
    supervisor:start_child(tcp_client_sup, []).

start(_Type, _Args) ->
    case application:get_env(application:get_application(), listen_port) of
        {ok, Val} -> Val;
        _         -> case init:get_argument(listen_port) of
                         [[Val | _]] -> Val;
                         error       -> ?DEF_PORT
                     end
    end.

stop(_S) -> ok.

% Supervisor behaviour callbacks
init([Port, Module]) ->
    {ok,
        {_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [ % TCP Listener
              { tcp_server_sup,                                    %% Id
                {tcp_listener, start_link, [Port, Module]},        %% StartFun
                permanent,                                         %% Restart
                2000,                                              %% Shutdown
                worker,                                            %% Type
                [tcp_listener]                                     %% Modules
              },
              % Client instance supervisor
              { tcp_client_sup,
                {supervisor, start_link, [{local, tcp_client_sup},
                                          ?MODULE,
                                          [Module]
                                         ]},
                permanent,
                infinity,
                supervisor,
                []
              }
            ]
        }
    };
% Supervisor behaviour callbacks
init([Module]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [ % TCP Client
              { undefined,                                         %% Id
                {Module, start_link, []},                          %% StartFun
                temporary,                                         %% Restart
                2000,                                              %% Shutdown
                worker,                                            %% Type
                []                                                 %% Modules
              }
            ]
        }
    }.
