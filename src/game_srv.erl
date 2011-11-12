%------------------------------------------------------
% game_srv
%------------------------------------------------------
% actually runs games
% gives players units at the beginning of the game,
% and checks each update to see if the game has ended
% (if only units from one player remain)
%------------------------------------------------------
-module(game_srv).

-behavior(gen_server).

-include("include/pos.hrl").
-include("include/player.hrl").
-include("include/unit.hrl").

-export([start_link/0]).
-export([start/0, check/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, ?MODULE, []).

start() ->
	gen_server:cast(?MODULE, {start}).

check() ->
	gen_server:cast(?MODULE, {check}).

init(?MODULE) ->
	{ok, ok}.

handle_call(Request, _From, State) ->
	{stop, {unknown_request, Request}, {unknown_request, Request}, State}.

handle_cast({start}, State) ->
	io:format("~p: Starting game.~n", [self()]),
	add_starting_units(),
	update_handler:start(),
	step_srv:start(),
	{noreply, State};
handle_cast({check}, State) ->
	case game_done() of
		false ->
			ok;
		Winner ->
			step_srv:stop(),
			update_handler:endgame(Winner)
	end,
	{noreply, State};
handle_cast(_Request, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_Vsn, State, _Other) ->
	{ok, State}.

add_starting_units() ->
	{players, Players} = player_register:get_players(),
	Angle = math:pi() * 2 / length(Players),
	add_starting_units(Players, Angle, 0, length(Players)).

add_starting_units([], _, _, _) ->
	ok;
add_starting_units([Player|Rest], Angle, Curr, Offset) ->
	unit_srv:add_unit(2, #pos{x=Offset*math:cos(Angle*Curr),
			          y=Offset*math:sin(Angle*Curr)},
			  Player#player.id),
	add_starting_units(Rest, Angle, Curr+1, Offset).

game_done() ->
	{units, [First|Units]} = unit_srv:get_units(),
	check_game_done(Units, First#unit.owner).

check_game_done([], Owner) ->
	Owner;
check_game_done([#unit{owner = Owner}|Units], Owner) ->
	check_game_done(Units, Owner);
check_game_done(_, _) ->
	false.
