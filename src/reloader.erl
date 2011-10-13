-module (reloader).

-behaviour (gen_server).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_reload/0]).

-record (state, {}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_reload() ->
	gen_server:cast(?MODULE, reload).

init([]) ->
	{ok, #state{}}.

handle_call(Request, _From, State) ->
	{reply, {unknown_request, Request}, State}.

handle_cast(reload, State) ->
	reload(),
	{noreply, State};
handle_cast(_Request, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

reload() ->
	{ok, Modules} = application:get_key(modules),
	load_code(Modules).

load_code([]) ->
	ok;
load_code([Module|Modules]) ->
	code:purge(Module),
	code:load_file(Module),
	load_code(Modules).

-ifdef(EUNIT).
start_test() ->
	start_link().

reload_test_() ->
	{setup,
		fun() -> application:start(rts) end,
		fun(_) -> application:stop(rts) end,
		fun(_) ->
			[fun() -> start_reload() end] end}.
-endif.
