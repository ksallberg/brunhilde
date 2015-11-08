-module(rest_server_supervisor).

-behavior(supervisor).
-export([init/1]).

-define(MAX_RESTART, 5).
-define(MAX_TIME,    60).

%% Start supervisor for the tcp_listener module.
init([Port, Module]) ->
    io:format("Hej!~n"),
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

% Start supervisor fot
init([tcp_reply]) ->
    {ok,
     {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
      _ChildSpecs =
          [
           {tcp_reply_supervisor,        %% Id
            {tcp_reply, start_link, []}, %% StartFun
            temporary,                   %% Restart
            2000,                        %% Shutdown
            worker,                      %% Type
            []                           %% Modules
           }
          ]
     }
    }.
