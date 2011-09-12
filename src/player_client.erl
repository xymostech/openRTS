-module(player_client).

-behaviour(tcp_srv).

-record(state, {id}).

-export([init_tcp/1, handle_tcp_data/3, close_tcp/2]).

-export([start_link/0]).

-export([init/1]).

start_link() ->
  supervisor:start_link(?MODULE, ?MODULE).

init(?MODULE) ->
  {ok,
    {
      {one_for_all, 10, 60},
      [
        {
          tcp_server,
          {tcp_srv, start_link, [5280, ?MODULE]},
          permanent,
          infinity,
          supervisor,
          []
        },
        {
          player_register,
          {player_register, start_link, []},
          temporary,
          2000,
          worker,
          [player_register]
        }
      ]
    }
  }.

init_tcp(Socket) ->
  {ok, Id} = player_register:add_player(self(), Socket),
  {ok, #state{id=Id}}.

handle_tcp_data(Data, _Socket, State) ->
  {ok, State}.

close_tcp(_Socket, #state{id=Id}=_State) ->
  player_register:rem_player(Id),
  ok.
