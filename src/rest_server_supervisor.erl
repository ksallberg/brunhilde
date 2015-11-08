-module(rest_server_supervisor).

-behavior(supervisor).
-export([init/1]).

-define(MAX_RESTART, 5).
-define(MAX_TIME,    60).

%% Supervisor behaviour callbacks
%% FIXME: Very weak type specification... Almost worse than nothing at all...
-spec init([any()]) -> tuple().
init([Port, Module]) ->
    {ok,
     {_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
      _ChildSpecs =
          [% TCP Listener
           {tcp_server_sup,                             %% Id
            {tcp_listener, start_link, [Port, Module]}, %% StartFun
            permanent,                                  %% Restart
            2000,                                       %% Shutdown
            worker,                                     %% Type
            [tcp_listener]                              %% Modules
           },
           % Client instance supervisor
           {tcp_client_sup,
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
      _ChildSpecs =
          [% TCP Client
           {tcp_client_sup,           %% Id
            {Module, start_link, []}, %% StartFun
            temporary,                %% Restart
            2000,                     %% Shutdown
            worker,                   %% Type
            []                        %% Modules
           }
          ]
     }
    }.
