-module(info_srv).

-behavior(gen_server).

-include("include/unit_data.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/0, get_data/1]).

-record(state, {data = []}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, ?MODULE, []).

get_data(Id) ->
  gen_server:call(?MODULE, {get_data, Id}).

init(?MODULE) ->
  {ok, #state{data = [#u_data{id=1, speed = 2}]}}.

handle_call({get_data, UnitId}, _From, #state{data = Data} = State) ->
  case lists:keyfind(UnitId, #u_data.id, Data) of
    false ->
      {reply, {no_unit}, State};
    Data ->
      {reply, {data, Data}, State}
  end;
handle_call(Request, _From, State) ->
  {reply, {invalid_request, Request}, {invalid_request, Request}, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_Vsn, State, _Other) ->
  {ok, State}.

