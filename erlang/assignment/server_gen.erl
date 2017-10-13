%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%%
%%% Implements the behavior of a generic server that can handle both synchronous
%%% and asynchronous messages.
%%%
%%% There are a number of callbacks that need to be implemented for the server
%%% to work correctly as a specific server.
%%%
%%% Required callbacks.
%%%
%%% init(Args) where:
%%%   * Args::term() are the initial arguments that are to be passed to the
%%%     generic server.
%%%   Returns: {ok, State::term()} where State is the initial state used to
%%%            bootstrap the server.
%%%            | {stop, Reason::term()} where Reason is the reason the server
%%%            could not start.
%%%
%%% handle(Request, State) where:
%%%   * Request::term() is the request sent by the requesting process.
%%%   * State::term() is the current server state.
%%% Returns: {Reply::term(), NewState::term()} where Reply is the reply to be
%%%          sent back to the requesting process and NewState is the new server
%%%          state (if applicable).
%%% handle(Request, State) is used to handle synchronous communication with the
%%% server, where each message sent by the requesting process is paired with a
%%% reply. Synchronous messages can be sent to the server using rpc/2, where
%%% typically, each message in the specific server implementation that is sent
%%% using rpc/2 is serviced by a corresponding handle/2 clause.
%%%
%%% Optional callbacks.
%%%
%%% terminate(State) where:
%%%   * State::term() is the current server state.
%%% Returns: {ok, Reason::term()} where Reason is the successful termination
%%%          reason to be sent back to the requesting process.
%%%          | {error, Reason::term()} where Reason is the unsuccessful
%%%          termination reason to be sent back to the requesting process.
%%% Default: In not implemented, the default value {ok, stopped} is returned.
%%%
%%% handle_async(Request, State) where:
%%%   * Request::term() is the request sent by the requesting process.
%%%   * State::term() is the current server state.
%%% Returns: NewState::term(), the updated server state.
%%% Default: If not implemented, the current server State is returned by
%%%          default.
%%% handle_async(Request, State) is used to handle asynchronous communication
%%% with the server. Unlike the synchronous case, messages sent in an
%%% asynchronous fashion are not typically paired with a reply, as the sending
%%% process does not block, awaiting for an answer. Consequently, the return
%%% value from handle_async(Request, State) is never sent to the requesting
%%% in reply. Asynchronous messages can be sent to the server using rpc_async/2,
%%% where typically, each message in the specific server implementation that is
%%% sent using rpc_async/2 is serviced by a corresponding handle_async/2 clause.
%%%
%%% handle_exit(From, Reason, State) where:
%%%   * From::term() is the PID of the linked terminated process.
%%%   * Reason::term() is the linked process termination reason.
%%%   * State::term() is the current server state.
%%% Returns: NewState::term(), the updated server state.
%%% Default: If not implemented, the current server State is returned by
%%%          default.
%%% handle_exit(From, Reason, State) is used to handle process exit signals in
%%% an implementaiton-specific way, possibly altering the server state. Note
%%% that to receive process messages, process_flag(trap_exit, true) should
%%% be called in the init/1 callback of the implementor module.
%%% ----------------------------------------------------------------------------
-module(server_gen).
-include("log.hrl").

%%% Generic server API exports.
-export([start/2, start_link/2, stop/1, rpc/2, rpc_async/2]).

%%% Internal exports.
-export([init/2, loop/2]).


%%% ------------------------------------------------------------------------ %%%
%%% Generic server API.                                                      %%%
%%% ------------------------------------------------------------------------ %%%

%% -----------------------------------------------------------------------------
%% Starts a generic server.
%% start(Mod, Args) where:
%%   * Mod::term() is the name of the module that implements the callbacks.
%%   * Args:term() are any arguments that are to be passed to the generic
%%     server.
%% Returns: Pid::pid(), the PID of the newly spawned server process.
%% -----------------------------------------------------------------------------
start(Mod, Args) ->
  spawn(?MODULE, init, [Mod, Args]).

%% -----------------------------------------------------------------------------
%% Starts a generic server and links to it the process that called
%% start_link/1.
%% start_link(Mod, Args) where:
%%   * Mod::term() is the name of the module that implements the callbacks.
%%   * Args:term() are any arguments that are to be passed to the generic
%%     server.
%% Returns: Pid::pid(), the PID of the newly spawned server process.
%% -----------------------------------------------------------------------------
start_link(Mod, Args) ->
  spawn_link(?MODULE, init, [Mod, Args]).

%% -----------------------------------------------------------------------------
%% Stops the generic server.
%% stop(ServerRef) where:
%%   * To::identifier() is the PID or name of the recipient process.
%% Returns: Status:term(), the termination status that is returned by the
%%          Mod:terminate/1 callback if implemented, or {ok, stopped} if not.
%% -----------------------------------------------------------------------------
stop(ServerRef) ->
  rpc(ServerRef, stop).


%%% ------------------------------------------------------------------------ %%%
%%% Internal generic server functions.                                       %%%
%%% ------------------------------------------------------------------------ %%%

%% -----------------------------------------------------------------------------
%% Initializes the server state before it enters the main process loop.
%% init(Mod, Args) where:
%%   * Mod::term() is the name of the module that implements the callback.
%%   * Args::term() are any arguments that are to be passed to the generic
%%     server.
%% Returns: {ok, State::term()} where State is the initial state of the server.
%%          | {stop, Reason::term()} where Reason is the reason the server
%%          could not start.
%% -----------------------------------------------------------------------------
init(Mod, Args) ->
  case catch Mod:init(Args) of
    {ok, State} ->
      ?TRACE("Init successful with state: ~p.", [State]),
      loop(Mod, State);
    {stop, Reason} ->
      ?TRACE("Init failed with reason: ~p.", [Reason]),
      exit(Reason)
  end.

%% -----------------------------------------------------------------------------
%% Handles the generic server termination and cleanup procedure.
%% terminate(Mod, State) where:
%%   * Mod::term() is the name of the module that implements the callback.
%%   * State::term() is the current server state.
%% Returns: {ok, Reason::term()} where Reason is the termination reason.
%%
%% If Mod implements its own terminate/1 callback, then this is used to handle
%% the server cleanup, otherwise the default handler is used. The default
%% handler returns {ok, stopped}.
%% -----------------------------------------------------------------------------
terminate(Mod, State) ->
  case erlang:function_exported(Mod, terminate, 1) of
    false ->
      ?TRACE("~p:terminate/1 not implemented: using default behavior.", [Mod]),
      {ok, stopped};
    _ ->
      ?TRACE("Delegating termination to ~p:terminate/1.", [Mod]),
      Mod:terminate(State)
  end.

%% -----------------------------------------------------------------------------
%%  Handles generic aysnchronous messages.
%%  handle_async(Mod, Request, State) where:
%%   * Mod::term() is the name of the module that implements the callback.
%%   * Request::term() is the request sent by the requesting process.
%%   * State::term() is the current server state.
%% Returns: NewState::term(), the updated server state.
%%
%% If Mod implements its own handle_async/2 callback, then this is used to
%% handle the message, otherwise the default handler is used. The default
%% handler returns the current server state.
%%
%% Asynchronous messages are sent to the server using rpc_async/2.
%% -----------------------------------------------------------------------------
handle_async(Mod, Request, State) ->
  case erlang:function_exported(Mod, handle_async, 2) of
    false ->
      ?TRACE("~p:handle_async/2 not implemented: using default behavior.", [Mod]),
      State;
    _ ->
      ?TRACE("Delegating async message handling to ~p:handle_async/2.", [Mod]),
      Mod:handle_async(Request, State)
  end.

%% -----------------------------------------------------------------------------
%% Handles process exit messages from linked processes to the generic server.
%% handle_exit(Mod, From, Reason, State) where:
%%   * Mod::term() is the name of the module that implements the callback.
%%   * From::term() is the PID of the linked terminated process.
%%   * Reason::term() is the linked process termination reason.
%%   * State::term() is the current server state.
%% Returns: NewState::term(), the updated server state.
%%
%% If Mod implements its own handle_exit/2 callback, then this is used to
%% handle the message, otherwise the default handler is used. The default
%% handler returns the current server state. The request is handled
%% asynchronously.
%%
%% To receive process exit messages, the process_flag(trap_exit, true) should
%% be called in the init/1 callback of the implementor module.
%% -----------------------------------------------------------------------------
handle_exit(Mod, From, Reason, State) ->
  case erlang:function_exported(Mod, handle_exit, 3) of
    false ->
      ?TRACE("~p:handle_exit/3 not implemented: using default behavior.", [Mod]),
      State;
    _ ->
      ?TRACE("Delegating exit handling to ~p:handle_exit/3.", [Mod]),
      Mod:handle_exit(From, Reason, State)
  end.

%% -----------------------------------------------------------------------------
%% The main process loop.
%% loop(Mod, State) where:
%%   * Mod::term() is the name of the module that implements the callbacks.
%%   * State::term() is the current server state.
%% Does not return.
%%
%% This function is called by the init/1 initialisation function, passing to
%% it the State returned by the Mod:init/1 callback. It continues to loop
%% indefinitely until a 'stop' message is serviced, in which case the generic
%% server terminates, delegating any cleanup tasks to the terminate/1 callback.
%% -----------------------------------------------------------------------------
loop(Mod, State) ->
  receive

    % Handles stop messages, optionally applies the terminate/1 callback if
    % implemented by the client, and finally, stops the server.
    {From, Tag, stop} ->
      Status = terminate(Mod, State),
      ?TRACE("Stopping server ~p with status ~p.", [self(), Status]),
      From ! {Tag, Status};

    % Handles process exit messages. Custom logic is delegated to the
    % handle_exit/3 callback if implemented by the client, otherwise the
    % default handler is used.
    {'EXIT', From, Reason} ->
      ?TRACE("Handling EXIT from PID ~p with reason: ~p.", [From, Reason]),
      NewState = handle_exit(Mod, From, Reason, State),
      loop(Mod, NewState);

    % Handles generic synchronous messages. Custom logic is delegated to the
    % handle/2 callback, which *must* be implemented.
    {From, Tag, Request} ->
      ?TRACE("Handling sync request from PID ~p of type: ~p.", [From, Request]),
      {Reply, NewState} = Mod:handle(Request, State),
      From ! {Tag, Reply},
      loop(Mod, NewState);

    % Handles generic asynchronous messages. Custom logic is delegates to the
    % handle_async/2 callback if implemented by the client, otherwise the
    % default handler is used.
    {From, Request} ->
      ?TRACE("Handling async request from PID ~p of type: ~p.", [From, Request]),
      NewState = handle_async(Mod, Request, State),
      loop(Mod, NewState)
  end.


%%% ------------------------------------------------------------------------ %%%
%%% Client API.                                                              %%%
%%% ------------------------------------------------------------------------ %%%

%% -----------------------------------------------------------------------------
%% Sends a blocking request to the server and waits for a reply.
%% rpc(To, Request) where:
%%   * To::identifier() is the PID or name of the recipient process.
%%   * Request::term() is the request.
%% Returns: Reply::term(), the reply sent back by the server.
%% The function returns {error, timeout} if the timeout value of 2000ms is
%% exceeded.
%% -----------------------------------------------------------------------------
rpc(To, Request) ->
  Tag = make_ref(),
  To ! {self(), Tag, Request},
  receive
    {Tag, Reply} -> Reply
  after 2000 ->
    {error, timeout}
  end.

%% -----------------------------------------------------------------------------
%% Sends a non-blocking request to the server, not assuming any kind of reply.
%% rpc_async(To, Request) where:
%%   * To::identifier() is the PID or name of the recipient process.
%%   * Request::term() is the request.
%% Returns: Request::term(), the request sent to the server.
%% -----------------------------------------------------------------------------
rpc_async(To, Request) ->
  To ! {self(), Request}.
