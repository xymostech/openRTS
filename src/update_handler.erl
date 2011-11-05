-module(update_handler).

-export([connect/1, update/0]).

-include("include/player.hrl").
-include("include/unit.hrl").

connect(#player{socket = Socket} = _Player) ->
	{units, Units} = unit_srv:get_units(),
	gen_tcp:send(Socket, "Units:\n"),
	send_units(Units, Socket).

update() ->
	{players, Players} = player_register:get_players(),
	{changed, Changed} = unit_srv:do_update(),
	lists:map(fun(#player{socket = Socket}) -> gen_tcp:send(Socket, "\nChanges:\n"), send_units(Changed, Socket) end, Players).

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
