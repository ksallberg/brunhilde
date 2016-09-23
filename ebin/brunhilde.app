{application, brunhilde,
 [ {description,  "Rest server"}
 , {vsn,          "1.2"}
 , {id,           "brunhilde"}
 , {modules,      [ http_parser
                  , http_handler
                  , brunhilde_application
                  , brunhilde_supervisor
                  , tcp_server
                  , tcp_supervisor
                  , stats_supervisor
                  , stats_server
                  , tracker_server
                  , reloader_server
                  , br_ext]}
 , {registered, [ tcp_supervisor
                , tracker_server]}
 , {applications, [kernel, stdlib]}
 , {mod, {brunhilde_application, []}}
 , {lager,
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
