-module(update_handler).

-export([connect/1, start/0, update/0, endgame/1]).

-include("include/player.hrl").
-include("include/unit.hrl").

connect(#player{socket = Socket, id=Id} = _Player) ->
	gen_tcp:send(Socket, io_lib:format("You are player ~p~n", [Id])).

start() ->
	{players, Players} = player_register:get_players(),
	{units, Units} = unit_srv:get_units(),
	lists:map(fun(#player{socket = Socket}) -> gen_tcp:send(Socket, "\nUnits:\n"), send_units(Units, Socket) end, Players).

update() ->
	{players, Players} = player_register:get_players(),
	{changed, Changed} = unit_srv:do_update(),
	lists:map(fun(#player{socket = Socket}) -> gen_tcp:send(Socket, "\nChanges:\n"), send_units(Changed, Socket) end, Players).

endgame(Winner) ->
	{players, Players} = player_register:get_players(),
	lists:map(fun(#player{socket = Socket}) -> gen_tcp:send(Socket, io_lib:format("~nGame Ended~nPlayer ~p won~n", [Winner])) end, Players).

send_units(Units, Socket) ->
	lists:map(fun(Unit) -> gen_tcp:send(Socket, format_unit(Unit)) end, Units).

format_unit(#unit{id=Id, type_id=Type, pos=#pos{x=X, y=Y}, owner=Owner, health=Health}) ->
	io_lib:format("Added:   ~p's unit ~p (~p) at (~p, ~p) with ~p dmg~n", [Owner, Type, Id, X, Y, Health]);
format_unit({changed, #unit{id=Id, type_id=Type, pos=#pos{x=X, y=Y}, owner=Owner, health=Health}}) ->
	io_lib:format("Changed: ~p's unit ~p (~p) at (~p, ~p) with ~p dmg~n", [Owner, Type, Id, X, Y, Health]);
format_unit({added,   #unit{id=Id, type_id=Type, pos=#pos{x=X, y=Y}, owner=Owner, health=Health}}) ->
	io_lib:format("Added:   ~p's unit ~p (~p) at (~p, ~p) with ~p dmg~n", [Owner, Type, Id, X, Y, Health]);
format_unit({removed, #unit{id=Id}}) ->
	io_lib:format("Removed: ~p~n", [Id]).
