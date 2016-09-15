%% run as: application:start(rest_server)
{application, rest_server,
 [{description,  "Rest server"},
  {vsn,          "1.2"},
  {id,           "rest_server"},
  {modules,      [http_parser,
                  rest_handler,
                  rest_server_application,
                  rest_server_supervisor,
                  tcp_server,
                  tcp_supervisor,
                  stats_supervisor,
                  stats_server,
                  tracker_server
                 ]
  },
  {registered,   [tcp_supervisor]},
  {applications, [kernel, stdlib]},
  {mod, {rest_server_application, []}},
  {lager,
   [ {log_root, "log/"}
   , {handlers, [ {lager_console_backend, info}
                , {lager_file_backend, [{file, "error.log"}, {level, error}]}
                , {lager_file_backend, [{file, "console.log"}, {level, info}]}
                ]}
   , {colored, true}
   ]
  }
 ]
}.
