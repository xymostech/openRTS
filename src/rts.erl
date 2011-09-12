-module(rts).

-behavior(application).

-export([start/2, stop/1]).

start(_, _) ->
  rts_sup:start_link().

stop(_) ->
  rts_sup:stop().
