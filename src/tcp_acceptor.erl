-module(tcp_acceptor).

-behavior(gen_server).

-export([start_link/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {socket, accept_ref, sup}).

start_link(Port, Sup) ->
  gen_server:start_link(?MODULE, {acceptor, Port, Sup}, []).

init({acceptor, Port, Sup}) ->
  process_flag(trap_exit, true),
  Opts = [list, {packet, 0}, {reuseaddr, true}, {keepalive, true}, {backlog, 30}, {active, false}],
  case gen_tcp:listen(Port, Opts) of
    {ok, Socket} ->
      {ok, Ref} = prim_inet:async_accept(Socket, -1),
      {ok, #state{socket=Socket, accept_ref=Ref, sup=Sup}};
    {error, Reason} ->
      io:format("Error on listening to port!"),
      {error, Reason}
  end.

handle_call(Request, _From, State) ->
  {stop, {unknown_call, Request}, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({inet_async, Socket, Ref, {ok, NewSock}},
            #state{socket = Socket, accept_ref = Ref, sup = Sup} = State) ->
  try
    case set_sockopt(Socket, NewSock) of
      ok ->
        ok;
      {error, Reason} ->
        exit({error, Reason})
    end,

    ClientSup = case find_client_sup(supervisor:which_children(Sup)) of
      {ok, SupPid} ->
        SupPid;
      {error, SupError} ->
        exit({error, SupError})
    end,

    {ok, Pid} = supervisor:start_child(ClientSup, []),
    gen_tcp:controlling_process(NewSock, Pid),

    tcp_client:start_socket(Pid, NewSock),

    NewRef = case prim_inet:async_accept(Socket, -1) of
      {ok, New} ->
        New;
      {error, Error} ->
        exit({async_accept, inet_format:error(Error)})
    end,

    {noreply, State#state{accept_ref = NewRef}}
  catch exit:Why ->
    error_logger:error_msg("Error in async accept: ~p.~n", [Why]),
    {stop, Why, State}
  end;
handle_info({inet_async, Socket, Ref, Error},
            #state{socket = Socket, accept_ref = Ref} = State) ->
  error_logger:error_mst("Error in acceptor: ~p.~n", [Error]),
  {stop, Error, State};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
  gen_tcp:close(State#state.socket),
  ok.

code_change(_Old, State, _Other) ->
  {ok, State}.

set_sockopt(Sock, NewSock) ->
  true = inet_db:register_socket(NewSock, inet_tcp),
  case prim_inet:getopts(Sock, [active, nodelay, keepalive, delay_send, priority, tos]) of
    {ok, Opts} ->
      case prim_inet:setopts(NewSock, Opts) of
        ok ->
          ok;
        Error ->
          gen_tcp:close(NewSock),
          Error
      end;
    Error ->
      gen_tcp:close(NewSock),
      Error
  end.

find_client_sup([{tcp_client_sup, Pid, _, _}|_]) ->
  {ok, Pid};
find_client_sup([_|Rest]) ->
  find_client_sup(Rest);
find_client_sup([]) ->
  {error, no_pid}.

