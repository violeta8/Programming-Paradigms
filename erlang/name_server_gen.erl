-module(name_server_gen).
-export([start/1, stop/0]). % Server exports.
-export([resolve/1, register/2, unregister/1, status/0]). % Client exports.
-export ([init/1, terminate/1, handle/2]). % Callback exports.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Server functions.                                                            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Starts the server and initialises it with the specified list of Name-IP pairs.
start(NamesIps) ->

  % Note: the start/1 function is delegating the task of starting the server
  % loop to the server_gen:start/2 function.
  server_gen:start(?MODULE, NamesIps).

% Stops the server.
stop() ->

  % Note: the stop/0 function is delegating the task of stopping the server
  % loop to the server_gen:stop/1 function.
  server_gen:stop(?MODULE).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Callback functions.                                                          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Handles server initialisation.
% Note: the init/1 function is called automatically by server_gen when it
% starts to allow (actual call is done by server_gen:init/2, but that is
% invisible from us and we should not care) us to perform *custom*
% initialisation. The returned State is then used by server_gen to start its
% own process loop.
init(State) ->
  true = erlang:register(?MODULE, self()),
  State.

% Handles the server cleanup.
% Note: this function is called automatically by server_gen when it stops
% (actual call is done by the process loop when it matches a stop message sent
% by server_gen:stop/1, but that is invisible to us and we should not care) to
% allow us to perform cleanup. The returned value is sent back to the client
% that sent the stop message.
terminate(_) -> {ok, stopped}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Client API callback functions.                                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Note: The handle function is called back (hence the name 'callback') by the
% process loop in server_gen. This occurs when a client sends a message to
% the process and the receive clause in the loop in server_gen matches *any*
% request (apart from 'stop').
% The receive statement is generic, and to handle the behaviour specific to
% each particular message (e.g. a 'resolve' request), it *delegates* (or
% forwards) this request to the handle function by simply calling it (examine
% the code in server_gen and notice the Mod:handle(..) in its receive
% statement).
% Each clause of handle is pattern-matched to determine which one should be used
% to service the request. The implementation of each handle clause is simple:
% it just returns the *data* that should be returned to the client as a reply.
% Crucially however, it does not send it itself: this task is taken care of
% by the receive statement back in server_gen.
% Notice how we split the generic server behaviour (simply looping, reading a
% request and sending a reply), and the specific behaviour (each request that
% is sent has different input and output data).
% Notice the semicolon after each function: this is *one* function split into
% a number of clauses!

% Inquires the server status.
% Note: this clause is matched when a message of type 'status' is sent by the
% client. State contains the list of Name-IP pairs.
handle(status, State) ->
  {{ok, length(State)}, State};

% Fetches the registered name from the server's list of Name-IP pairs.
% If no such Name is found, and error is returned.
% Note: this clause is matched when a message of type '{resolve, Name}' is sent
% by the client.
handle({resolve, Name}, State) ->
  case lists:keyfind(Name, 1, State) of
    false ->

      % Requested name was not found - return error.
      {{error, not_found}, State};
    {Name, Ip} ->

      % Requested name was found - return OK.
      {{ok, Ip}, State}
  end;


% Adds the Name and corresponding IP to the server's list of Name-IP pairs.
% If a Name already exists, an error is returned.
% Note: this clause is matched when a message of type '{register, Name, Ip}' is
% sent by the client.
handle({register, Pair = {Name, Ip}}, State) ->
  case lists:keyfind(Name, 1, State) of
    false ->

      % Name to be added was not found - return OK and add the new Name-IP pair
      % to the current list of pairs in the server.
      % Note: we return the *updated* list.
      {{ok, Ip}, [Pair | State]};
    _ ->

      % Name to be added was found and we should not create duplicates -
      % return error.
      % Note: we return the same list as nothing was added.
      {{error, already_exists}, State}
  end;

% Removes the registered Name and its corresponding IP from the list of
% Name-IP pairs.
% If no such Name is found, and error is returned.
% Note: this clause is matched when a message of type '{unregister, Name}' is
% sent by the client.
handle({unregister, Name}, State) ->
  case lists:keyfind(Name, 1, State) of
    false ->

      % Name to be removed not found - reply error.
      % Note: we return the same list as nothing was removed.
      {{error, not_found}, State};

    {Name, Ip} ->

      % Name to be removed was found - return OK and remove the Name-IP pair
      % from the current list of pairs in the server.
      % Note: we return the *updated* list.
      {{ok, Ip}, lists:keydelete(Name, 1, State)}
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Client API.                                                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Resolves the specified name to an IP.
resolve(Name) -> server_gen:rpc(?MODULE, {resolve, Name}).

% Registers the specified name to the IP.
register(Name, Ip) -> server_gen:rpc(?MODULE, {register, {Name, Ip}}).

% Unregisters the speficied name.
unregister(Name) -> server_gen:rpc(?MODULE, {unregister, Name}).

% Inquires the server's status.
status() -> server_gen:rpc(?MODULE, status).
