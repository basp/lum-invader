{application, oni,
 [{description, "Oni (Lum Invader)"},
  {vsn, "1.0.0"},
  {modules, [oni, oni_ansi, oni_app, oni_bstr, oni_cmd, oni_db, oni_event,
             oni_event_logger, oni_match, oni_sockserv_serv, oni_sockserv_sup,
             oni_sup, oni_who]},
  {registered, [oni]},
  {applications, [kernel, stdlib]},
  {mod, {oni_app, []}},
  {env, [{port, 7777}]}
 ]}.