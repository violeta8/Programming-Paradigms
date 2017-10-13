%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%%
%%% Extends the generic server and implements the behavior of a mobile phone.
%%%
%%% The mobile phone provides a number of API functions, allowing it to interact
%%% with the switch (switch.erl) process. In doing so, these functions
%%% manipulate the internal state of the mobile phone directly, and the internal
%%% state of the switch indirectly (see switch.erl).
%%%
%%% SMS messages are simply implemented using the standard messages sending
%%% functionality provided by Erlang (i.e. using !).
%%%
%%% The state of the mobile phone process, maintained as a pair, consists of the
%%% mobile phone number (called the MSISDN) and the flight mode status.
%%%
%%% The flight mode status can be one of two values:
%%% fmode() = flight_mode_on | flight_mode_off where:
%%%   * flight_mode_on means that the mobile phone is currently detached from
%%%     the mobile network and cannot receive any messages.
%%%   * flight_mode_off means that the mobile phone is currently attached to the
%%%     mobile network and can receive messages.
%%% A mobile phone process that starts should immediately attach itself to the
%%% mobile network, enabling it to receive text messages. Any pending messages
%%% that were sent when the mobile was not attached to the network should be
%%% processed at this time by the mobile phone (e.g. printing them to screen).
%%% Terminating mobile processes should (in their cleanup function) detached
%%% from the mobile network to inform the switch.
%%%
%%% The structure of the internal mobile phone state tuple is structured as
%%% follows:
%%% mob() = {Msisdn::string(), FlightMode::fmode()} where:
%%%   * Msisdn::string() is the number the mobile phone process is subscribed
%%%     with.
%%%   * FlightMode::fmode() is the current flight mode status.
%%% The MSISDN to be used by the mobile phone is set at startup, whereas the
%%% initial value of FlightMode is set to flight_mode_off, to denote the fact
%%% that the mobile phone is immediately attached to the mobile network.
%%% ----------------------------------------------------------------------------
-module(mobile).
-include("log.hrl").

%%% Mobile API exports.
-export([switch_on/1, switch_off/1]).

%%% Callback exports.
-export([init/1, terminate/1, handle/2, handle_async/2]).

%%% Client API exports.
-export([status/1, tog_flight_mode/1, send_msg/3]).


%%% ------------------------------------------------------------------------ %%%
%%% Mobile phone API.                                                        %%%
%%% ------------------------------------------------------------------------ %%%

%% -----------------------------------------------------------------------------
%% Starts the mobile phone.
%% switch_on(Msisdn) where:
%%   * Msisdn::string() is the number the mobile phone process is subscribed
%%     with.
%% Returns: Pid::pid(), the PID of the newly spawned mobile phone process.
%% -----------------------------------------------------------------------------
switch_on(Msisdn) ->
  server_gen:start(?MODULE, Msisdn).

%% -----------------------------------------------------------------------------
%% Stops the mobile phone.
%% stop() where:
%%   no parameters are accepted.
%% Returns: Status:term(), the switch termination status.
%% -----------------------------------------------------------------------------
switch_off(Pid) ->
  server_gen:stop(Pid).


%%% ------------------------------------------------------------------------ %%%
%%% Callbacks.                                                               %%%
%%% ------------------------------------------------------------------------ %%%

%% -----------------------------------------------------------------------------
%% Initializes the mobile phone state before it enters the main process loop,
%% attaching it to the mobile network.
%% init(Msisdn) where:
%%   * Msisdn::string() is the number the mobile phone process is subscribed
%%     with.
%% Returns: {ok, {Msisdn::string(), flight_mode_off} if the Msisdn was
%%          successfully attached to the mobile network.
%%          | {stop, {Reason::term(), Msisdn::string()}} when Msisdn could not
%%          be attached to the mobile network.
%%
%% Any messages that were pending are printed out on screen when the mobile
%% phone is successfully attached to the mobile network.
%% -----------------------------------------------------------------------------
init(Msisdn) ->

  % Check that Msisdn can be successfully attached to the network. If not, it
  % makes no sense to start the mobile phone process.
  case switch:attach(Msisdn) of
    {error, Reason} ->
      {stop, {Reason, Msisdn}};
    {ok, attached, Pending} ->

      % Print any messages that might have been pending while the mobile was
      % not attached to the mobile network.
      print_msgs(Pending),
      {ok, {Msisdn, flight_mode_off}}
  end.

%% -----------------------------------------------------------------------------
%% Performs cleanup before the mobile phone process is terminated by detaching
%% the internally managed Msisdn from the mobile network.
%% terminate(_State) where:
%%   * _State::mob() is the mobile MSISDN and its current flight mode status.
%% Returns: {ok, switched_off} when the internally managed Msisdn has been
%%          successfully detached from the mobile network.
%%          | {warning, Reason} when the internally managed Msisdn was removed
%%          from the mobile network while this mobile was still atached (and
%%          therefore, the Msisdn could then not be detached because it does
%%          not exist anymore). The atom warning indicates that despite failing
%%          to detach the internally managed Msisdn from the mobile network,
%%          both the switch and mobile will remain in a consistent state:
%%          Msisdn was already removed from the switch, and the mobile phone
%%          process will in any case terminate (since we are already in the
%%          cleanup phase). When next switched on, the mobile phone will behave
%%          normally (i.e. either return an error because it is not subscribed,
%%          or attach successfully if it has been resubscribed).
%% -----------------------------------------------------------------------------
terminate(_State) ->

  % Handles the case where the mobile is attached to the switch, but someone
  % mistakenly unsubscribed the msisdn. Detaching will return a
  % {error, not_attached} reply that must be handled correctly.
  case switch:detach() of
    {ok, detached} ->
      {ok, switched_off};
    {error, Reason} ->
      {warning, Reason}
  end.

%% -----------------------------------------------------------------------------
%% Returns the internal mobile phone state.
%% handle(status, State) where:
%%   * status is the message request.
%%   * State::mob() is the mobile MSISDN and its current flight mode status.
%% Returns: {{ok, State::mob()}, State::mob()} where State the mobile MSISDN and
%%           its current flight mode status.
%%
%% The message request is handled synchronously.
%% -----------------------------------------------------------------------------
handle(status, State) -> {{ok, State}, State};

%% -----------------------------------------------------------------------------
%% Handles a 'toggle flight mode status' request message.
%% handle(tog_flight_mode, State) where
%%   * tog_flight_mode is the message request.
%%   * State::mob() is the mobile MSISDN and its current flight mode status.
%% Returns: {{ok, flight_mode_on}, {Msisdn::string(), flight_mode_on}} if
%%          the previous flight mode status was equal to flight_mode_off, and
%%          the mobile was successfully detached from the mobile network.
%%          State is the unmodified mobile phone state.
%%          | {{ok, flight_mode_off}, {Msisdn::string(), flight_mode_off}} if
%%          the previous flight mode status was equal to flight_mode_on, and the
%%          mobile was successfully attached to the mobile network.
%%          State is the unmodified mobile phone state.
%%          | {{error, Reason}, State::mob()} if in any of the cases (i.e.
%%          mobile being attached to/detached from the mobile network), the
%%          operation was unsuccessful.
%%          State is the unmodified mobile phone state.
%%
%% The message request is handled synchronously.
%% -----------------------------------------------------------------------------
handle(tog_flight_mode, State = {Msisdn, Status}) ->
  % TODO: Add implementation.
  ok;

%% -----------------------------------------------------------------------------
%% Handles a 'send message text' request message.
%% handle({send_msg, ToMsisdn, Msg}, State) where:
%%   * {send_msg, ToMsisdn::string(), Msg::string()} is the message request
%%     containing the recipient MSISDN ToMsisdn, and the text Msg to be sent.
%%   * State::mob() is the mobile MSISDN and its current flight mode status.
%% Returns: {Reply::term(), State::mob()} where Reply is the forwarded 'send
%%          message' reply that was sent from the switch. State is the
%%          unmodified mobile phone state.
%%
%% The message request is handled synchronously.
%% -----------------------------------------------------------------------------
handle({send_msg, ToMsisdn, Msg}, State) ->
  % TODO: Add implementation.
  ok.

%% -----------------------------------------------------------------------------
%% Handles a 'receive new text message' request message.
%% handle_async({msg, FromMsisdn, Msg}, State) where:
%%   * {msg, FromMsisdn::string(), Msg::string()} is the new text message.
%%   * State::mob() is the mobile MSISDN and its current flight mode status.
%% Returns: State::mob(), the unmodified mobile phone state.
%%
%% The message request is handled asynchronously.
%% -----------------------------------------------------------------------------
handle_async({msg, FromMsisdn, Msg}, State) ->
  print_msgs([{FromMsisdn, Msg}]),
  State.


%%% ------------------------------------------------------------------------ %%%
%%% Helper functions.                                                        %%%
%%% ------------------------------------------------------------------------ %%%

%% -----------------------------------------------------------------------------
%% Prints the specified list of text messages.
%% print_msgs(MsgList) where:
%%   * [{FromMsisdn::string(), Msg::string()}] is the list of messages with
%%     FromMsisdn being the MSISDN of the sender, and Msg, the message text
%%     itself.
%% Returns: the no-op ok.
%% -----------------------------------------------------------------------------
print_msgs([]) ->
  ok;
print_msgs([{FromMsisdn, Msg} | T]) ->
  io:format("Received new text from ~p: ~p.~n", [FromMsisdn, Msg]),
  print_msgs(T).


%%% ------------------------------------------------------------------------ %%%
%%% Client API.                                                              %%%
%%% ------------------------------------------------------------------------ %%%

%% -----------------------------------------------------------------------------
%% Inquires the mobile phone status.
%% status(Pid) where:
%%   * Pid::pid() is the PID of the mobile phone process whose status is to be
%%     checked.
%% Returns: {ok, State::mob()} where State the mobile MSISDN and its current
%%          flight mode status.
%% -----------------------------------------------------------------------------
status(Pid) ->
  server_gen:rpc(Pid, status).

%% -----------------------------------------------------------------------------
%% Toggles the mobile phone's flight mode status flag.
%% tog_flight_mode(Pid) where:
%%   * Pid::pid() is the PID of the mobile phone process whose flight mode
%%     status is to be toggled.
%% Returns: {ok, flight_mode_on} if the previous flight mode status was equal to
%%          flight_mode_off, and the mobile was successfully detached from the
%%          mobile network.
%%          | {ok, flight_mode_off} if the previous flight mode status was equal
%%          to flight_mode_on, and the mobile was successfully attached to the
%%          mobile network.
%%          | if in any of the cases (i.e. mobile being attached to/detached
%%          from the mobile network), the operation was unsuccessful.
%% -----------------------------------------------------------------------------
tog_flight_mode(Pid) ->
  server_gen:rpc(Pid, tog_flight_mode).

%% -----------------------------------------------------------------------------
%% Sends a text message to the specified ToMsisdn.
%% send_msg(Pid, ToMsisdn, Msg) where:
%%   * Pid::pid() is the PID of the mobile phone process that is to send the
%%     text message.
%%   * ToMsisdn::string() is the MSISDN of the recipient mobile phone.
%%   * Msg::string() is the text message to be sent.
%% Returns: Reply::term() is the message sending status Reply from the switch.
%% -----------------------------------------------------------------------------
send_msg(Pid, ToMsisdn, Msg) ->
  server_gen:rpc(Pid, {send_msg, ToMsisdn, Msg}).
