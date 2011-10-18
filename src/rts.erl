%%%--------------------------------------------------
%%% module rts
%%%--------------------------------------------------
%%% main application of openRTS
%%%--------------------------------------------------
%%% Exports
%%%--------------------------------------------------
%%% start(_StartType, _StartArgs)
%%%   application callback
%%%   starts the main rts_sup supervisor
%%%
%%% stop(_State)
%%%   application callback
%%%   stops the main rts_sup supervisor
%%%--------------------------------------------------

-module(rts).

-behavior(application).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([start/2, stop/1]).

%% starts the application
start(_StartType, _StartArgs) ->
	rts_sup:start_link().

%% stops the application
stop(_State) ->
	rts_sup:stop().

-ifdef(EUNIT).
%rts_start_test() ->
	%start([],[]).

%rts_stop_test_() ->
	%{setup, fun() -> {ok, Pid} = rts:start(normal,[]) end,
                      %fun(_) -> ok end,
                      %fun(_) -> [fun() -> rts:stop([]) end] end}.
-endif.
