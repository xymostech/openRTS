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
	lists:map(fun(#unit{id=Id, pos=Pos}) -> gen_tcp:send(Socket, io_lib:format("Unit (~p) at (~p,~p)~n", [Id, Pos#pos.x, Pos#pos.y])) end,
      	          Units).
