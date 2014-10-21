%% run as: application:start(rest_server)
{application, rest_server,
 [{description,  "Rest server"},
  {vsn,          "1.0"},
  {id,           "rest_server"},
  {modules,      [tcp_listener,
                  tcp_reply,
                  battle_ship,
                  commands,
                  http_parser,
                  rest_server_app,
                  route_handler
                  ]
  },
  {registered,   [rest_server_sup, tcp_listener]},
  {applications, [kernel, stdlib]},
  %%
  %% mod: Specify the module name to start the application, plus args
  %%
  {mod, {rest_server_app, []}},
  {env, []}
 ]
}.
