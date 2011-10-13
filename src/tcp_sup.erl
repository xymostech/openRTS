-module(tcp_sup).

-export([start_link/2]).
-export([start_client/0]).

-export([init/1]).

start_link(Port, Module) ->
	supervisor:start_link(?MODULE, {sup, Port, Module}).

start_client() ->
	supervisor:start_child(tcp_client_sup, []).

init({sup, Port, Module}) ->
	{ok,
		{
			{one_for_all, 10, 60},
			[
				{
					tcp_client_sup,
					{supervisor, start_link, [?MODULE, {client_sup, Module}]},
					permanent,
					infinity,
					supervisor,
					[]
				},
				{
					tcp_acceptor,
					{tcp_acceptor, start_link, [Port, self()]},
					temporary,
					2000,
					worker,
					[tcp_acceptor]
				}
			]
		}
	};
init({client_sup, Module}) ->
	{ok,
		{
			{simple_one_for_one, 10, 60},
			[
				{
					tcp_client,
					{tcp_client, start_link, [Module]},
					temporary,
					2000,
					worker,
					[tcp_client]
				}
			]
		}
	}.
