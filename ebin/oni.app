{application, oni,
 [{vsn, "0.1.0"},
  {modules, [oni_app, oni_sockserv_sup, oni_sockserv_serv]},
  {registered, [oni]},
  {applications, [kernel, stdlib]},
  {mod, {oni_app, []}},
  {env, [{port, 7777}]}
 ]}.