%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%%
%%% Implements the behavior of a generic server that can handle both synchronous
%%% and asynchronous messages.
%%% ----------------------------------------------------------------------------
-module(server_gen).
-include("log.hrl").

%%% Generic server API.
-export([start/2, start_link/2, stop/1, rpc/2]).

%%% Internal exports.
-export([init/2, loop/2]).


%%% ------------------------------------------------------------------------ %%%
%%% Generic server API.                                                      %%%
%%% ------------------------------------------------------------------------ %%%

%% -----------------------------------------------------------------------------
%% Starts the server.
%%
%% Note: a new process is spawned, passing to the the module name 'Mod' and the
%% arguments that are to be used to initialise the server.
%% The module name 'Mod' is a variable that contains the name of the module that
%% implements all the callbacks required by this generic server. There are 3
%% callbacks to implement:
%% 1. the init/2 implementation should accept as parameters the module name
%%    'Mod' and the arguments 'Args'. It should return the 'State' that is used
%%    to bootstrap the server (see below).
%% 2. the terminate/1 implementation should accept a single parameter, the
%%    server state  'State', and should return the status that is to be sent to
%%    the client when the server is stopped.
%% 3. the handle/2 implementation should accept as parameters the module request
%%    data 'Request' sent by the client and the server state 'State'. The handle
%%    implementation will typically consist of multiple clauses, one per *each*
%%    different type of request sent by the client (see name_server for an
%%    example of this). Each handle clause must in turn return a pair:
%%    '{Reply, NewState}' with the computed 'Reply' to be sent to the client and
%%    the 'NewState' to be saved in the server. 'NewState' can either be equal
%%    to the old state (in nothing was changed by the handle clause) or the
%%    actual modifed state.
%% -----------------------------------------------------------------------------
start(Mod, Args) ->
  spawn(?MODULE, init, [Mod, Args]).

%% -----------------------------------------------------------------------------
%% Starts the server and links it to the process that called start_link/1.
%% -----------------------------------------------------------------------------
start_link(Mod, Args) ->
  spawn_link(?MODULE, init, [Mod, Args]).

%% -----------------------------------------------------------------------------
%% Stops the server.
%% -----------------------------------------------------------------------------
stop(ServerRef) -> rpc(ServerRef, stop).


%%% ------------------------------------------------------------------------ %%%
%%% Internal generic server functions.                                       %%%
%%% ------------------------------------------------------------------------ %%%

%% -----------------------------------------------------------------------------
%% Initialises the server.
%%
%% Note: this function starts the process (i.e. it is the one that is spawned).
%% It calls the Mod:init/1 function that should be located in a different module
%% that implements init/1 (e.g. name_server).
%% The argument 'Args' passed to the implementation of init/1 can choose to
%% return the same 'Args', or typically use it to return the 'State'. The
%% returned 'State' is used to call the process loop/2 below.
%% -----------------------------------------------------------------------------
init(Mod, Args) ->
  State = Mod:init(Args),
  loop(Mod, State).

%% -----------------------------------------------------------------------------
%% The main process loop.
%%
%% Note: this function is called by init/1 above to service client messages.
%% There are 2 types of messages the receive statement handles:
%% 1. 'stop' messages cause the server to stop (observe that in the 'stop'
%%    clause, the server does not call loop/2). The '{From, Tag, stop}' receive
%%    clause calls the  Mod:terminate/1 function to allow the implementor to
%%    send a custom stop status.
%% 2. Any other type of 'Request' that must be handled by the Mod:handle/2
%%    function. The '{From, Tag, Request}' receive clause calls the Mod:handle/2
%%    function to allow the implementor to handle the request.
%% -----------------------------------------------------------------------------
loop(Mod, State) ->
  receive
    {From, Tag, stop} ->
      Status = Mod:terminate(State),
      ?DEBUG("Stopping server ~p with status ~p.", [self(), Status]),
      From ! {Tag, Status};
    {From, Tag, Request} ->
      {Reply, NewState} = Mod:handle(Request, State),
      ?DEBUG("Handling request of type: ~p.", [Request]),
      From ! {Tag, Reply},
      loop(Mod, NewState)
  end.


%%% ------------------------------------------------------------------------ %%%
%%% Client API.                                                              %%%
%%% ------------------------------------------------------------------------ %%%

%% -----------------------------------------------------------------------------
%% Requests the server and waits for the reply.
%% -----------------------------------------------------------------------------
rpc(To, Request) ->
  Tag = make_ref(),
  To ! {self(), Tag, Request},
  receive
    {Tag, Reply} -> Reply
  after 1000 ->
    {error, timeout}
  end.
