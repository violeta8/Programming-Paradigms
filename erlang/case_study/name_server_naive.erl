%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%%
%%% Implements a namer server without using the server_gen.erl module. The
%%% implementation works as required, but in terms of its software design, can
%%% be greatly improved. See name_server.erl.
%%% ----------------------------------------------------------------------------
-module(name_server_naive).

%%% Name server API exports.
-export([start/1, loop/1, stop/0]).

%%% Client API exports.
-export([resolve/1, register/2, unregister/1, status/0]).


%%% ------------------------------------------------------------------------ %%%
%%% Name server API.                                                         %%%
%%% ------------------------------------------------------------------------ %%%

%% -----------------------------------------------------------------------------
%% Starts the server and initialises it with the specified list of Name-IP
%% pairs.
%% -----------------------------------------------------------------------------
start(NamesIps) ->

  % Note: Using register(..) would have been fine in other applications, but
  % since I have defined a register/2 function myself in this file, I *must*
  % use the fully-qualified form 'erlang:register(..)', otherwise the compiler
  % will complain.
  erlang:register(?MODULE, spawn(?MODULE, loop, [NamesIps])).

%% -----------------------------------------------------------------------------
%% The main server loop that goes on forever, unless the message consisting of
%% just the atom 'stop' is received.
%%
%% Note: the server loop has a single state variable containing a list of
%% Name-IP pairs.
%% -----------------------------------------------------------------------------
loop(NameIps) ->
  receive

    % Stops the server.
    {From, Tag, stop} ->
      From ! {Tag, {ok, stopped}};

    % Inquires the server status.
    {From, Tag, status} ->
      From ! {Tag, {ok, length(NameIps)}},
      loop(NameIps);

    % Fetches the registered name from the server's list of Name-IP pairs.
    % If no such 'Name' is found, and error is sent back to the client.
    {From, Tag, {resolve, Name}} ->
      case lists:keyfind(Name, 1, NameIps) of
        false ->

          % Requested name was not found - reply with error.
          From ! {Tag, {error, not_found}};
        {Name, Ip} ->

          % Requested name was found - reply with OK.
          From ! {Tag, {ok, Ip}}
      end,
      loop(NameIps);

    % Adds the 'Name' and corresponding 'Ip' to the server's list of Name-IP pairs.
    % If 'Name' already exists, an error is sent back to the client.
    {From, Tag, {register, Pair = {Name, Ip}}} ->
      case lists:keyfind(Name, 1, NameIps) of
        false ->

          % 'Name' to be added was not found - reply to client with OK and add
          % the new Name-IP pair to the current list of pairs in the server.
          % Note: we call loop/1 with the *updated* list.
          From ! {Tag, {ok, Ip}},
          loop([Pair | NameIps]);
        _ ->

          % 'Name' to be added was found and we should not create duplicates -
          % reply to client with error.
          % Note: we call loop/1 with the same list as nothing was added.
          From ! {Tag, {error, already_exists}},
          loop(NameIps)
      end;

    % Removes the registered 'Name' and its corresponding IP from the list of
    % Name-IP pairs.
    % If no such 'Name' is found, and error is sent back to the client.
    {From, Tag, {unregister, Name}} ->
      case lists:keyfind(Name, 1, NameIps) of
        false ->

          % 'Name' to be removed not found - reply with error.
          % Note: we call loop/1 with the same list as nothing was removed.
          From ! {Tag, {error, not_found}},
          loop(NameIps);

        {Name, Ip} ->

          % 'Name' to be removed was found - reply to client with OK and removee
          % the Name-IP pair from the current list of pairs in the server.
          % Note: we call loop/1 with the *updated* list.
          From ! {Tag, {ok, Ip}},
          loop(lists:keydelete(Name, 1, NameIps))
      end
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

%% -----------------------------------------------------------------------------
%% Stops the server.
%% -----------------------------------------------------------------------------
stop() -> rpc(?MODULE, stop).

%% -----------------------------------------------------------------------------
%% Inquires the server status.
%% -----------------------------------------------------------------------------
status() -> rpc(?MODULE, status).

%% -----------------------------------------------------------------------------
%% Resolves the specified 'Name' to an IP.
%% -----------------------------------------------------------------------------
resolve(Name) -> rpc(?MODULE, {resolve, Name}).

%% -----------------------------------------------------------------------------
%% Registers the specified 'Name' to the 'Ip'.
%% -----------------------------------------------------------------------------
register(Name, Ip) -> rpc(?MODULE, {register, {Name, Ip}}).

%% -----------------------------------------------------------------------------
%% Unregisters the speficied 'Name'.
%% -----------------------------------------------------------------------------
unregister(Name) -> rpc(?MODULE, {unregister, Name}).
