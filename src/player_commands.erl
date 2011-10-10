-module(player_commands).

-include("include/unit.hrl").
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([handle_command/2]).

split_commands(Command) ->
  split_commands(Command, [], []).

split_commands([], Current, Commands) ->
  lists:map(fun(A) -> {I,_} = string:to_integer(A), I end,
            lists:reverse([lists:reverse(Current)|Commands]));
split_commands([10|Rest], Current, Commands) ->
  split_commands(Rest, Current, Commands);
split_commands([13|Rest], Current, Commands) ->
  split_commands(Rest, Current, Commands);
split_commands([$ |Rest], Current, Commands) ->
  split_commands(Rest, [], [lists:reverse(Current)|Commands]);
split_commands([C|Rest], Current, Commands) ->
  split_commands(Rest, [C|Current], Commands).

handle_command(Command, Id) ->
  [First|Rest] = split_commands(Command),
  case First of
    1 ->
      add_move(Rest, Id);
    2 ->
      add_spawn(Rest, Id);
    _ ->
      ok
  end.

add_move(Args, Id) ->
  [UnitId, PosX, PosY|_] = Args,
  unit_srv:add_move_command(UnitId, #pos{x=PosX, y=PosY}, Id).

add_spawn(Args, Id) ->
  [UnitId, NewUnitId|_] = Args,
  unit_srv:add_spawn_command(UnitId, NewUnitId, Id).

-ifdef(EUNIT).
split_test_() ->
	[?_test([[]] = split_commands([])),
	 ?_test(["a", "b", "c"] = split_commands("a b c")),
	 ?_test(["ab", "cd"] = split_commands("a\r\nb c\r\nd\r\n"))].
-endif.
