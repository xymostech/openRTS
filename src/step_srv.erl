-module(step_srv).

-behavior(gen_server).

-export([start_link/0]).
-export([start/0, stop/0, do_update/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {timer_ref}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, ?MODULE, []).

start() ->
  gen_server:cast(?MODULE, {start}).

stop() ->
  gen_server:cast(?MODULE, {stop}).

do_update() ->
  gen_server:cast(?MODULE, {do_update}).

init(?MODULE) ->
  {ok, #state{}}.

handle_call(Request, _From, State) ->
  {stop, {unknown_request, Request}, {unknown_request, Request}, State}.

handle_cast({start}, #state{timer_ref = Ref} = State) ->
  timer:cancel(Ref),
  {ok, NewRef} = timer:apply_after(5000, ?MODULE, do_update, []),
  {noreply, State#state{timer_ref = NewRef}};
handle_cast({stop}, #state{timer_ref = Ref} = State) ->
  timer:cancel(Ref),
  {noreply, State};
handle_cast({do_update}, State) ->
  update_handler:update(),
  io:format("~p: Did update.~n", [self()]),
  {ok, NewRef} = timer:apply_after(5000, ?MODULE, do_update, []),
  {noreply, State#state{timer_ref = NewRef}};
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, #state{timer_ref = Ref} = _State) ->
  timer:cancel(Ref),
  ok.

code_change(_Vsn, State, _Other) ->
  {ok, State}.
