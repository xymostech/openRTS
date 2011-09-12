{application, rts,
  [{description, "Open-source RTS game server"},
   {vsn, "0.0.1"},
   {modules, [rts, rts_sup,
              player_sup, player_acceptor, player_client]},
   {registered, [rts_sup,
                 player_sup, player_client_sup, player_acceptor]},
   {applications, [kernel, stdlib]},
   {mod, {rts, []}},
   {env, []}
  ]}.
