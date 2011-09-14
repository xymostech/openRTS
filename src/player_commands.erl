-module(player_commands).

-export([handle_command/2]).

-include("include/unit.hrl").

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
    _ ->
      ok
  end.

add_move(Args, Id) ->
  [UnitId, PosX, PosY|_] = Args,
  unit_srv:add_move_command(UnitId, #pos{x=PosX, y=PosY}, Id).
