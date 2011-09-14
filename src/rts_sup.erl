-module(rts_sup).

-export([start_link/0, stop/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, ?MODULE).

init(?MODULE) ->
  {ok,
    {
      {one_for_one, 10, 60},
      [
        {
          player_sup,
          {player_client, start_link, []},
          permanent,
          infinity,
          supervisor,
          [player_client]
        },
        {
          telnet_sup,
          {telnet_client, start_link, []},
          permanent,
          infinity,
          supervisor,
          [telnet_client]
        },
        {
          unit_srv,
          {unit_srv, start_link, []},
          temporary,
          2000,
          worker,
          [unit_srv]
        },
        {
          step_srv,
          {step_srv, start_link, []},
          temporary,
          2000,
          worker,
          [step_srv]
        },
				{
					reloader,
					{reloader, start_link, []},
					temporary,
					2000,
					worker,
					[reloader]
				}
      ]
    }
  }.

stop() ->
  ok.
