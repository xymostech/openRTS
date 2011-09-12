-module(telnet_client).

-behavior(tcp_srv).

-export([start_link/0]).

-export([init_tcp/1, handle_tcp_data/3, close_tcp/2]).

start_link() ->
  tcp_srv:start_link(2222, ?MODULE).

init_tcp(_Socket) ->
  {ok, []}.

handle_tcp_data(Data, Socket, _) ->
  case strip(Data) of
    "start" ->
      step_srv:start();
    "stop" ->
      step_srv:stop();
    _ ->
      gen_tcp:send(Socket, "Invalid command\n"),
      ok
  end,
  {ok, []}.

close_tcp(_Socket, _) ->
  ok.

strip(Command) ->
  string:strip(string:strip(Command, right, 10), right, 13).
