-module(player_register).

-behavior(gen_server).

-export([start_link/0]).

-export([add_player/2, rem_player/1, get_players/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("include/player.hrl").

-record(state, {players=[]}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, ?MODULE, []).

add_player(Pid, Socket) ->
	gen_server:call(?MODULE, {add_player, Pid, Socket}).

rem_player(Pid) ->
	gen_server:cast(?MODULE, {rem_player, Pid}).

get_players() ->
	gen_server:call(?MODULE, {get_players}).

init(?MODULE) ->
	{ok, #state{}}.

handle_call({add_player, Pid, Socket}, _From, #state{players = Players} = State) ->
	Id = get_new_id(Players),
	NewPlayer = #player{pid=Pid, socket=Socket, id=Id},
	update_handler:connect(NewPlayer),
	{reply, {id, Id}, State#state{players=[NewPlayer|Players]}};
handle_call({get_players}, _From, #state{players = Players} = State) ->
	{reply, {players, Players}, State};
handle_call(Request, _From, State) ->
	{stop, {invalid_request, Request}, {invalid_request, Request}, State}.

handle_cast({rem_player, Id}, #state{players = Players} = State) ->
	{noreply, State#state{players=lists:keydelete(Id, #player.id, Players)}};
handle_cast(_Request, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_Old, State, _Other) ->
	{ok, State}.

get_new_id(Players) ->
	get_new_id(Players, 1).

get_new_id([], Max) ->
	Max;
get_new_id([#player{id=Id}|Players], Max) ->
	get_new_id(Players, if Id >= Max -> Id + 1; true -> Max end).
