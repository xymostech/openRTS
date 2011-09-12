-module(unit_srv).

-behavior(gen_server).

-export([start_link/0]).
-export([add_unit/3, add_command/4, get_units/0, do_update/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("include/unit.hrl").

-record(state, {units=[], commands=[]}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, ?MODULE, []).

add_unit(Id, Pos, Owner) ->
  gen_server:cast(?MODULE, {add_unit, #unit{id=Id, pos=Pos, owner=Owner}}).

add_command(Owner, Id, Dir, Steps) ->
  gen_server:cast(?MODULE, {add_command, #command{id=Id, dir=Dir, steps=Steps}, Owner}).

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
handle_cast({add_command, Command, Owner}, #state{units = Units, commands = Commands} = State) ->
  case lists:keyfind(Command#command.id, #unit.id, Units) of
    #unit{owner = Owner} ->
      Cleaned = lists:keydelete(Command#command.id, #command.id, Commands),
      {noreply, State#state{commands=[Command|Cleaned]}};
    _ ->
      {noreply, State}
  end;
handle_cast(_Request, State) ->
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
do_update(Units, [#command{id=Id, dir=Dir, steps=Steps} = Command|Commands], UnitAcc, CommandAcc, Changed) ->
  case lists:keyfind(Id, #unit.id, Units) of
    false ->
      do_update(Units, Commands, UnitAcc, CommandAcc, Changed);
    #unit{pos = Pos} = Unit -> 
      NewUnit = Unit#unit{pos=pos:add(Pos,Dir)},
      do_update(lists:keydelete(Id, #unit.id, Units),
                Commands,
                [NewUnit|UnitAcc],
                if Steps > 1 -> [Command#command{steps=Steps-1}|CommandAcc]; true -> CommandAcc end,
                [NewUnit|Changed])
  end.
