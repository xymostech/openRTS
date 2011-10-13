-module(tcp_client).

-behavior(gen_fsm).

-export([init/1, handle_event/3, handle_sync_event/4,
         handle_info/3, terminate/3, code_change/4]).

-export([wait_for_socket/2, get_data/2]).

-export([start_link/1, start_socket/2]).

-record(state, {socket, module, client_state}).

start_link(Module) ->
	gen_fsm:start_link(?MODULE, Module, []).

start_socket(Pid, Socket) ->
	gen_fsm:send_event(Pid, {new_socket, Socket}).

init(Module) ->
	{ok, wait_for_socket, #state{module=Module}}.

wait_for_socket({new_socket, Socket}, State) ->
	case (State#state.module):init_tcp(Socket) of
		{ok, ClientState} ->
			inet:setopts(Socket, [{active, once}]),
			{next_state, get_data, State#state{socket = Socket, client_state = ClientState}};
		{error, Error} ->
			{stop, {error, Error}, State}
	end;
wait_for_socket(_Else, State) ->
	{next_state, wait_for_socket, State}.

get_data({data, Data}, State) ->
	case (State#state.module):handle_tcp_data(Data, State#state.socket, State#state.client_state) of
		{ok, NewState} ->
			{next_state, get_data, State#state{client_state = NewState}};
		{stop, Error} ->
			{stop, {error, Error}, State}
	end;
get_data(_Else, State) ->
	{next_state, get_data, State}.

handle_event(Event, StateName, State) ->
	{stop, {StateName, undefined_event, Event}, State}.

handle_sync_event(Event, _From, StateName, State) ->
	{stop, {StateName, undefined_event, Event}, {undefined_event, Event}, State}.

handle_info({tcp, Socket, Data}, StateName, #state{socket = Socket} = State) ->
	inet:setopts(Socket, [{active, once}]),
	?MODULE:StateName({data, Data}, State);
handle_info({tcp_closed, Socket}, _StateName, #state{socket = Socket} = State) ->
	{stop, normal, State};
handle_info(_Else, StateName, State) ->
	{next_state, StateName, State}.

terminate(_Reason, _StateName, State) ->
	case State#state.socket of
		undefined ->
			ok;
		_ ->
			(State#state.module):close_tcp(State#state.socket, State#state.client_state),
			(catch gen_tcp:close(State#state.socket))
	end.

code_change(_OldVsn, StateName, State, _Extra) ->
	{ok, StateName, State}.
