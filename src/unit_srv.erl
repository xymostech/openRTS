-module(unit_srv).

-compile(export_all).

-behavior(gen_server).

-include("include/unit.hrl").
-include("include/unit_data.hrl").
-include("include/command.hrl").

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/0]).
-export([add_unit/3, get_units/0, do_update/0]).
-export([add_move_command/3, add_spawn_command/3, add_attack_command/3]).

-record(state, {units=[], commands=[]}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, ?MODULE, []).

add_unit(Type, Pos, Owner) ->
	gen_sever:cast(?MODULE, {add_unit, #unit{type_id=Type, pos=Pos, owner=Owner}}).

add_move_command(Unit, Pos, Owner) ->
	add_command(#command{id=move, unit_id=Unit, command=#move_command{pos=Pos}}, Owner).

add_spawn_command(Unit, NewUnitType, Owner) ->
	add_command(#command{id=spawn, unit_id=Unit, command=#spawn_command{spawn_type=NewUnitType}}, Owner).

add_attack_command(Unit, AttackUnit, Owner) ->
	add_command(#command{id=attack, unit_id=Unit, command=#attack_command{att_id=AttackUnit}}, Owner).

add_command(Command, Owner) ->
	gen_server:cast(?MODULE, {add_command, Command, Owner}).

get_units() ->
	gen_server:call(?MODULE, {get_units}).

do_update() ->
	gen_server:call(?MODULE, {do_update}).

init(?MODULE) ->
	{ok, #state{}}.

handle_call({get_units}, _From, #state{units = Units} = State) ->
	{reply, {units, Units}, State};
handle_call({do_update}, _From, #state{units = Units, commands = Commands} = State) ->
	{Changed, NewUnits, NewCommands} = do_update(Units, Commands),
	{reply, {changed, Changed}, State#state{units=NewUnits, commands=NewCommands}};
handle_call(Request, _From, State) ->
	{stop, {invalid_request, Request}, {invalid_request, Request}, State}.

handle_cast({add_unit, Unit}, #state{units = Units} = State) ->
	{noreply, State#state{units = [Unit#unit{id=get_new_id(Units)}|Units]}};
handle_cast({add_command, Command, Owner}, #state{units = Units, commands = Commands} = State) ->
	case validate(Command, Units) of
		{ok, NewCommand} ->
			NewCommands = add_owned_command(NewCommand, Units, Commands, Owner),
			{noreply, State#state{commands=NewCommands}};
		{bad, _} ->
			{noreply, State}
	end;
handle_cast(_Request, State) ->
	io:format("~p: Got invalid request: ~p~n", [self(), _Request]),
	{noreply, State}.

get_new_id(Units) ->
	get_new_id(1, Units).

get_new_id(Id, []) ->
	Id;
get_new_id(Id, [Unit|Units]) ->
	NewId = if
		Id > Unit#unit.id ->
			Id;
		true ->
			Unit#unit.id+1
	end,
	get_new_id(NewId, Units).

add_owned_command(#command{unit_id=ID} = Command, Units, Commands, Owner) ->
	case lists:keyfind(ID, #unit.id, Units) of
		#unit{owner = Owner} ->
			[Command|lists:filter(fun(#command{unit_id=TestID}) -> TestID /= ID end, Commands)];
		_ ->
			Commands
	end.

%TODO: move 0 speed check into validation
validate(#command{id=move, unit_id=Id} = Command, Units) ->
	try
		#unit{type_id=Type} = lists:keyfind(Id, #unit.id, Units),
		{data, #u_data{speed=Speed}} = info_srv:get_data(Type),
		case Speed of
			0 ->
				{bad, Command};
			_ ->
				{ok, Command}
		end
	catch
		error:{badmatch, _} ->
			{bad, Command}
	end;
validate(#command{id=spawn, unit_id=Id, command=SpawnCmd} = Command, Units) ->
	try
		io:format("~p: Validating spawn command.~n", [self()]),
		#unit{type_id=Type} = lists:keyfind(Id, #unit.id, Units),
		{data, #u_data{spawns=Spawns}} = info_srv:get_data(Type),
		#spawn{turns=Turns} = lists:keyfind(SpawnCmd#spawn_command.spawn_type, #spawn.id, Spawns),
		io:format("~p: Can spawn in ~p Turns~n", [self(), Turns]),
		{ok, Command#command{command=SpawnCmd#spawn_command{turns = Turns}}}
	catch
		error:{badmatch, _} ->
			{bad, Command}
	end;
validate(#command{id=attack, unit_id=Id, command=AttackCmd} = Command, Units) ->
	try 
		#unit{type_id = Type} = lists:keyfind(Id, #unit.id, Units),
		{data, #u_data{attack=#attack{turns=Turns}}} = info_srv:get_data(Type),
		{ok, Command#command{command=AttackCmd#attack_command{turns=Turns}}}
	catch
		error:{badmatch, _} ->
			{bad, Command}
	end.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_Vsn, State, _Other) ->
	{ok, State}.

do_update(Units, Commands) ->
	do_update(Units, Commands, [], []).

do_update(Units, [], CommandAcc, Changed) ->
	{Changed, Units, CommandAcc};
do_update(Units, [#command{id=CmdId}=Command|Commands], CommandAcc, Changed) ->
	{NewUnits, NewCommands, NewChanged} = case CmdId of
		move ->
			move_update(Units, Command, CommandAcc, Changed);
		spawn ->
			spawn_update(Units, Command, CommandAcc, Changed);
		attack ->
			attack_update(Units, Command, CommandAcc, Changed)
	end,
	do_update(NewUnits, Commands, NewCommands, NewChanged).

move_update(Units, #command{unit_id=UnitId, command=#move_command{pos=EndPos}} = Command, CommandAcc, Changed) ->
	case lists:keyfind(UnitId, #unit.id, Units) of
		false ->
			{Units, Changed, CommandAcc};
		Unit ->
			FilteredUnits = lists:keydelete(UnitId, #unit.id, Units),
			{Done, NewPos} = find_new_pos(Unit, EndPos),
			NewUnit = Unit#unit{pos=NewPos},
			NewCommandAcc = case Done of
				done ->
					CommandAcc;
				{next, NewPos} ->
					[Command|CommandAcc]
			end,
			{[NewUnit|FilteredUnits], NewCommandAcc, [NewUnit|Changed]}
	end.

find_new_pos(#unit{pos=Pos, type_id=Type}, FinalPos) ->
	MoveVect = pos:add(FinalPos, pos:mult(Pos, -1)),
	MoveLen  = pos:length(MoveVect),
	{data, #u_data{speed=Speed}} = info_srv:get_data(Type),
	if
		Speed == 0 ->
			{done, Pos};
		MoveLen < Speed ->
			{done, FinalPos};
		true ->
			{next, pos:add(Pos, pos:mult(pos:unit(MoveVect), Speed))}
	end.

spawn_update(Units, #command{unit_id=Id, command=#spawn_command{spawn_type=Type, turns=0}}, CommandAcc, Changed) ->
	case lists:keyfind(Id, #unit.id, Units) of
		false ->
			{Units, Changed, CommandAcc};
		Unit ->
			NewUnit = #unit{id=get_new_id(Units), type_id=Type, pos=Unit#unit.pos, owner=Unit#unit.owner},
			{[NewUnit|Units], CommandAcc, [NewUnit|Changed]}
	end;
spawn_update(Units, #command{command=#spawn_command{turns=Turns}=SpawnCmd}=Command, CommandAcc, Changed) ->
	{Units, [Command#command{command = SpawnCmd#spawn_command{turns = Turns-1}}|CommandAcc], Changed}.

attack_update(Units, #command{unit_id=Id, command=#attack_command{att_id=AttackId, turns=0}}, CommandAcc, Changed) ->
	try
		#unit{type_id=UnitType, pos=UnitPos} = lists:keyfind(Id, #unit.id, Units),
		AttackUnit = #unit{pos=AttackUnitPos, health=Health} = lists:keyfind(AttackId, #unit.id, Units),
		#u_data{attack=#attack{range=Range, damage=Damage}} = info_srv:get_info(UnitType),
		true = pos:length(pos:add(pos:mult(UnitPos, -1), AttackUnitPos)) < Range,
		Filtered = lists:keydelete(AttackId, #unit.id, Units),
		NewAttackUnit = AttackUnit#unit{health=Health+Damage},
		{[NewAttackUnit|Filtered], CommandAcc, [NewAttackUnit|Changed]}
	catch
		error:{badmatch, _} ->
			{Units, Changed, CommandAcc}
	end;
attack_update(Units, #command{command=#attack_command{turns=Turns} = AttackCmd} = Command, CommandAcc, Changed) ->
	{Units, [Command#command{command = AttackCmd#attack_command{turns=Turns-1}}|CommandAcc], Changed}.

-ifdef(EUNIT).
start_test() ->
	start_link().

get_new_id_test_() ->
	[?_assert(get_new_id([]) == 1),
	 ?_assert(get_new_id([#unit{id=1}]) == 2),
	 ?_assert(get_new_id([#unit{id=3},
	                      #unit{id=1}]) == 4),
	 ?_assert(get_new_id([#unit{id=1},
	                      #unit{id=3}]) == 4)].

add_owned_command_test_() ->
	{setup,
	 fun() -> {[#unit{id=1, owner=1}, #unit{id=2, owner=3}]} end,
	 fun(_) -> ok end,
	 fun({Units}) -> [
		?_assertMatch([_], add_owned_command(#command{unit_id=1}, Units, [], 1)),
		?_assertMatch([], add_owned_command(#command{unit_id=1}, Units, [], 3)),
		?_assertMatch([], add_owned_command(#command{unit_id=2}, Units, [], 1)),
		?_assertMatch([_], add_owned_command(#command{unit_id=2}, Units, [], 3))
	] end}.

validate_test_() ->
	{spawn,
		{setup,
		 fun() -> info_srv:start_link("unit_srv_test"), {[#unit{id=1, type_id=1, owner=1}, #unit{id=2, type_id=2, owner=3}]} end,
		 fun(_) -> ok end,
		 fun({Units}) -> [
			%move commands
			?_assertMatch({ok,  #command{}}, validate(#command{id=move, unit_id=1}, Units)),
			?_assertMatch({bad, #command{}}, validate(#command{id=move, unit_id=2}, Units)),
			?_assertMatch({bad, #command{}}, validate(#command{id=move, unit_id=3}, Units)),
			%spawn commands
			?_assertMatch({ok,  #command{command=#spawn_command{turns=2}}},
			              validate(#command{id=spawn, unit_id=2, command=#spawn_command{spawn_type=1}}, Units)),
			?_assertMatch({bad, #command{}},
			              validate(#command{id=spawn, unit_id=1, command=#spawn_command{spawn_type=2}}, Units)),
			?_assertMatch({bad, #command{}},
			              validate(#command{id=spawn, unit_id=2, command=#spawn_command{spawn_type=2}}, Units)),
			?_assertMatch({bad, #command{}},
			              validate(#command{id=spawn, unit_id=3, command=#spawn_command{spawn_type=2}}, Units)),
			%attack commands
			?_assertMatch({ok,  #command{command=#attack_command{turns=1}}},
			              validate(#command{id=attack, unit_id=1, command=#attack_command{att_id=2}}, Units)),
			?_assertMatch({bad, #command{}},
			              validate(#command{id=attack, unit_id=2, command=#attack_command{att_id=1}}, Units)),
			?_assertMatch({bad, #command{}},
			              validate(#command{id=attack, unit_id=3, command=#attack_command{att_id=1}}, Units))
		] end}}.
-endif.
