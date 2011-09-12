-module(tcp_srv).

-export([behaviour_info/1]).

-export([start_link/2]).

behaviour_info(callbacks) ->
  [{init_tcp,1},
   {handle_tcp_data,3},
   {close_tcp,2}];
behaviour_info(_) ->
  undefined.

start_link(Port, Module) ->
  tcp_sup:start_link(Port, Module).

