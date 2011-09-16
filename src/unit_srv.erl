-module(unit_srv).

-behavior(gen_server).

-export([start_link/0]).
-export([add_unit/4, get_units/0, do_update/0]).
-export([add_move_command/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("include/unit.hrl").
-include("include/unit_data.hrl").
-include("include/command.hrl").

-record(state, {units=[], commands=[]}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, ?MODULE, []).

add_unit(Id, Type, Pos, Owner) ->
  gen_server:cast(?MODULE, {add_unit, #unit{id=Id, type_id=Type, pos=Pos, owner=Owner}}).

add_move_command(Unit, Pos, Owner) ->
  add_command(#command{id=1, command=#move_command{unit_id=Unit, pos=Pos}}, Owner).

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
  case lists:keyfind(Unit#unit.id, #unit.id, Units) of
    false ->
      {noreply, State#state{units=[Unit|Units]}};
    _ ->
      {noreply, State}
  end;
handle_cast({add_command, #command{id=1, command=MoveCmd} = Command, Owner}, #state{units = Units, commands = Commands} = State) ->
	io:format("~p: Got add command.~n", [self()]),
  case lists:keyfind(MoveCmd#move_command.unit_id, #move_command.unit_id, Units) of
    #unit{owner = Owner} ->
      Cleaned = lists:keydelete(MoveCmd#move_command.unit_id, #move_command.unit_id, Commands),
			io:format("~p: Commands: ~p~n", [self(), [Command|Cleaned]]),
      {noreply, State#state{commands=[Command|Cleaned]}};
    false ->
      io:format("~p: Illegal command: no unit with that id~n", [self()]),
      {noreply, State};
    _ ->
      io:format("~p: Illegal command: not owner~n", [self()]),
      {noreply, State}
  end;
handle_cast(_Request, State) ->
	io:format("~p: Got invalid request: ~p~n", [self(), _Request]),
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_Vsn, State, _Other) ->
  {ok, State}.

do_update(Units, Commands) ->
  do_update(Units, Commands, [], [], []).

do_update(Units, [], UnitAcc, CommandAcc, Changed) ->
  {Changed, Units++UnitAcc, CommandAcc};
do_update(Units, [#command{id=1, command=#move_command{unit_id=UnitId, pos=EndPos}}=Command|Commands],
          UnitAcc, CommandAcc, Changed) ->
	io:format("Movement update~n"),
  case lists:keyfind(UnitId, #unit.id, Units) of
    false ->
			io:format("Didn't find unit~n"),
      do_update(Units, Commands, UnitAcc, CommandAcc, Changed);
    #unit{pos = Pos} = Unit ->
      MoveVect = pos:add(EndPos, pos:mult(Pos, -1)),
      MoveLen = pos:length(MoveVect),
      {data, #u_data{speed = Speed}} = info_srv:get_data(Unit#unit.type_id),
      {NewUnit, NewCommands} = if
        MoveLen < Speed ->
					io:format("Finished moving~n"),
          {Unit#unit{pos = EndPos}, CommandAcc};
        true ->
					{Unit#unit{pos = pos:add(Pos, pos:mult(pos:unit(MoveVect), Speed))}, [Command|CommandAcc]}
      end,
      do_update(lists:keydelete(UnitId, #unit.id, Units),
                Commands,
                [NewUnit|UnitAcc],
                NewCommands,
                [NewUnit|Changed])
  end.
