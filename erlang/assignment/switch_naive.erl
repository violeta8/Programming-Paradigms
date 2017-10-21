%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%%
%%% Implements a a mobile network switch.
%%%
%%% The switch provides rudimentary functionality, allowing it to subscribe
%%% new, or unsubscribe existing mobile numbers (called MSISDNs).
%%%
%%% The state of the switch is maintained as a list of mobile phone subscribers,
%%% each having a MSISDN, a unique identifier (conveniently taken to be the PID
%%% of the mobile process), and a list of pending messages (messages that have
%%% not been yet delivered) that is initally set to be [].
%%%
%%% The structure of the pending message list is as follows:
%%% pending() = [{FromMsisdn::string(), Msg::string()}] where:
%%%   * FromMsisdn::string() is the mobile number of the sender.
%%%   * Msg::string() is the message that was sent.
%%%
%%% The structure of the internal switch state is a list of mobile subscribers:
%%% subs() = [{Msisdn::string(), Pid::pid(), Pending::pending()}] where:
%%%   * Msisdn::string() is the mobile number of the owner.
%%%   * Pid::pid() | 0 is the unique ID associated with this mobile number. Pid
%%%     will be conveniently used to send messages (using the primitive !) to
%%%     other processes (next step of the assignment) that represent the mobile
%%%     phones of other users.
%%%     A user who is attached to the mobile network is represented by an actual
%%%     Pid value that is used to send messages with. When the user is not
%%%     attached to the mobile network, Pid is set to 0 to denote
%%%     non-attachment. For now, all the new subscribers that are created are
%%%     not attached to the mobile network, so set the Pid = 0. We will see how
%%%     this can be dynamically changed when we cover the attach and detach
%%%     operations.
%%%   * Pending::pending() is the list of messages that have not yet been
%%%     delivered because the recipient is not currently attached to the mobile
%%%     network (i.e. the tuple for a particular MSISDN has its Pid set to 0 and
%%%     cannot recieve process messages). Pending is reset to [] when the mobile
%%%     corresponding subscriber MSISDN attaches itself to the mobile network.
%%%
%%% This naive implementation does not use a generic server, but handles the
%%% processing of messages in its own process loop.
%%% ----------------------------------------------------------------------------
-module(switch_naive).
-include("log.hrl").

%%% Switch API exports.
-export([start/0, stop/0]).

%%% Internal exports.
-export([loop/1]).

%%% Client API exports.
-export([status/0, subscribe/1, unsubscribe/1]).


%%% ------------------------------------------------------------------------ %%%
%%% Switch API.                                                              %%%
%%% ------------------------------------------------------------------------ %%%

%% -----------------------------------------------------------------------------
%% Starts the switch.
%% start() where:
%%   no parameters are accepted.
%% Returns: Pid::pid(), the PID of the newly spawned switch process.
%% -----------------------------------------------------------------------------

start() ->
  erlang:register(?MODULE, Pid = spawn(?MODULE, loop, [[]])),
  Pid.

%% -----------------------------------------------------------------------------
%% Stops the server.
%% stop() where:
%%   no parameters are accepted.
%% Returns: Status::term(), the switch termination status.
%% -----------------------------------------------------------------------------
stop() -> rpc(?MODULE, stop).


%%% ------------------------------------------------------------------------ %%%
%%% Internal functions.                                                      %%%
%%% ------------------------------------------------------------------------ %%%

%% -----------------------------------------------------------------------------
%% The main process loop.
%% loop(State) where:
%%   * Store::subs() is the current list of subscribed MSISDNs managed by the
%%     switch.
%% Does not return.
%%
%% The loop handles 4 types of messages:
%%
%% 1. A 'stop server' request message with the following format:
%%      * stop atom that causes the main process loop to terminate.
%%    Returns: {ok, stopped} to the sending process as an exit status.
%%
%%    The message request is handled synchronously.
%%
%% 2. A 'return the internal switch state' request message with the following
%%    format:
%%      * status atom that instructs the main process loop to return the
%%        internal state of the switch.
%%    Returns: {ok, Store::subs()} where Store is the list of all subscribed
%%             MSISDNs.
%%
%%    Note: status is useful when debugging and wishing to know how the internal
%%    state of the switch is evolving.
%%    The message request is handled synchronously.
%%
%% 3. A 'subscribe new Msisdn' request message with the following format:
%%      * {subscribe, Msisdn::string()} is the message request containing the
%%        new Msisdn to be subscribed.
%%    Returns: {ok, subscribed} when the Msisdn to be subscribed does not exist
%%             in the list of currently subscribed MSISDNs and is added.
%%             | {error, already_subscribed} when Msisdn is already subscribed.
%%             Msisdn is not added, and the store of currently subscribed
%%             MSISDNs is left unmodified.
%%
%%    The message request is handled synchronously.
%%
%% 4. An 'unsubscribe existing Msisdn' request message with the following
%%    format:
%%      * {unsubscribe, Msisdn::string()} is the message request containing the
%%        existing Msisdn to be unsubscribed.
%%    Returns: {ok, unsubscribed} when the Msisdn to be unsubscribed is found in
%%             the list of currently subscribed MSISDNs and is removed.
%%             | {error, not_subscribed} when Msisdn is not subscribed. The
%%             store of currently subscribed MSISDNs is left unmodified.
%%
%%    The message request is handled synchronously.
%% -----------------------------------------------------------------------------
loop(Store) ->
  receive
    {From, Tag, stop} ->
      ?DEBUG("Stopping server."),

      From ! {Tag, {ok, stopped}};

    {From, Tag, status} ->
      % TODO: Add implementation.
      ok;

    {From, Tag, {subscribe, Msisdn}} ->
      % TODO: Add implementation.
      ok;

    {From, Tag, {unsubscribe, Msisdn}} ->
      % TODO: Add implementation.
      ok
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
  after 1000 ->
    {error, timeout}
  end.

%% -----------------------------------------------------------------------------
%% (Operator) Inquires the switch's status.
%% status() where:
%%   no parameters are accepted.
%% Returns: {ok, Store::subs()} where Store is the list of all subscribed
%%          MSISDNs.
%% -----------------------------------------------------------------------------
status() ->
  % TODO: Add implementation.
  ok.

%% -----------------------------------------------------------------------------
%% (Operator) Subscribes the specified Msisdn to the switch.
%% subscribe(Msisdn) where:
%%   * Msisdn::string() is the new Msisdn to be subscribed.
%% Returns: {ok, subscribed} when the Msisdn to be subscribed does not exist on
%%          the switch.
%%          | {error, already_subscribed} when Msisdn is already subscribed.
%% -----------------------------------------------------------------------------
subscribe(Msisdn) ->
  % TODO: Add implementation.
  ok.

%% -----------------------------------------------------------------------------
%% (Operator) Unsubscribes the specified Msisdn from the switch.
%% unsubscribe(Msisdn) where:
%%   * Msisdn::string() is the existing Msisdn to be unsubscribed.
%% Returns: {ok, unsubscribed} when the Msisdn to be unsubscribed is found on
%%          the switch.
%%          | {error, not_subscribed} when Msisdn is not subscribed.
%% -----------------------------------------------------------------------------
unsubscribe(Msisdn) ->
  % TODO: Add implementation.
  ok.
