%% run as: application:start(rest_server)
{application, rest_server,
 [{description,  "Rest server"},
  {vsn,          "1.1"},
  {id,           "rest_server"},
  {modules,      [exec_supervisor,
                  http_parser,
                  rest_server_application,
                  rest_server_supervisor,
                  tcp_server,
                  tcp_supervisor
                 ]
  },
  {registered,   [tcp_supervisor]},
  {applications, [kernel, stdlib]},
  %%
  %% mod: Specify the module name to start the application, plus args
  %%
  {mod, {rest_server_application, []}},
  {env, [{port, 28251}]}
 ]
}.
