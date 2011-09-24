%%%--------------------------------------------------
%%% module rts_sup
%%%--------------------------------------------------
%%% handles the supervision of the main openRTS
%%% supervisors and workers
%%%--------------------------------------------------
%%% Exports
%%%--------------------------------------------------
%%% start_link()
%%%   starts the main supervisor
%%%
%%% stop()
%%%   stops the main supervisor
%%%   (actually does nothing, fix?)
%%%
%%% init(Module)
%%%   supervisor callback
%%%   returns the workers and supervisors
%%%   to be created
%%%--------------------------------------------------

-module(rts_sup).

-export([start_link/0, stop/0]).
-export([init/1]).

%% starts the supervisor thread
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, ?MODULE).

%% supervisor callback to find out which supervisors and workers to create
init(?MODULE) ->
  {ok,
    {
      {one_for_one, 10, 60},
      [
        % player connection supervisor
        {
          player_sup,
          {player_client, start_link, []},
          permanent,
          infinity,
          supervisor,
          [player_client]
        },
        % telnet connection supervisor
        {
          telnet_sup,
          {telnet_client, start_link, []},
          permanent,
          infinity,
          supervisor,
          [telnet_client]
        },
        % unit server worker
        {
          unit_srv,
          {unit_srv, start_link, []},
          temporary,
          2000,
          worker,
          [unit_srv]
        },
        % step server worker
        {
          step_srv,
          {step_srv, start_link, []},
          temporary,
          2000,
          worker,
          [step_srv]
        },
        % reloader worker
        {
          reloader,
          {reloader, start_link, []},
          temporary,
          2000,
          worker,
          [reloader]
        },
        % unit info worker
        {
          info_srv,
          {info_srv, start_link, []},
          temporary,
          2000,
          worker,
          [info_srv]
        }
      ]
    }
  }.

%% stops the supervisor
stop() ->
  ok.
