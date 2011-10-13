-module(pos).

-compile({no_auto_import, [length/1]}).

-include("include/pos.hrl").
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([add/2, length/1, unit/1, mult/2]).

add(#pos{x=X1, y=Y1}, #pos{x=X2, y=Y2}) ->
	#pos{x=(X1+X2), y=(Y1+Y2)}.

length(#pos{x=X, y=Y}) ->
	math:sqrt(X*X+Y*Y).

unit(#pos{x=X, y=Y}=Pos) ->
	Len = length(Pos),
	#pos{x=(X/Len), y=(Y/Len)}.

mult(#pos{x=X, y=Y}, M) ->
	#pos{x=(X*M), y=(Y*M)}.

-ifdef(EUNIT).
pos_test_() ->
	[?_assert(#pos{} == #pos{x=0, y=0}),
	 ?_assert(length(#pos{}) == 0),
	 ?_assert(length(#pos{x=3, y=4}) == 5),
	 ?_assert(mult(#pos{}, 5) == #pos{}),
	 ?_assert(mult(#pos{x=1, y=1}, 5) == #pos{x=5, y=5}),
	 ?_assert(add(#pos{}, #pos{}) == #pos{}),
	 ?_assert(add(#pos{x=1, y=2}, #pos{x=3, y=4}) == #pos{x=4, y=6}),
	 ?_assertError(badarith, unit(#pos{})),
	 ?_assert(abs(1-length(unit(#pos{x=3, y=7}))) < 0.02),
	 ?_assertMatch(#pos{x=_X, y=_X}, unit(#pos{x=1, y=1}))].
-endif.
