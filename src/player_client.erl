%%%--------------------------------------------------
%%% module player_client
%%%--------------------------------------------------
%%% Supervisor module for handling player connections
%%%--------------------------------------------------
%%% Exports
%%%--------------------------------------------------
%%% init_tcp(Socket)
%%%   initializes the connector state, and registers
%%%   with the player register
%%%
%%% handle_tcp_data(Data, Socket, State)
%%%   handles data received from player
%%%   passes to player_command for interpretation
%%%
%%% close_tcp(Socket, State)
%%%   handles closing of socket
%%%   removes player from player register
%%%
%%% start_link
%%%   starts the supervisor
%%%
%%% init(Module)
%%%   callback for supervisor:start_link
%%%--------------------------------------------------

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
  {id, Id} = player_register:add_player(self(), Socket),
  {ok, #state{id=Id}}.

handle_tcp_data(Data, _Socket, State) ->
	player_commands:handle_command(Data, State#state.id),
  {ok, State}.

close_tcp(_Socket, State) ->
  player_register:rem_player(State#state.id),
  ok.
