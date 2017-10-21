%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%%
%%% Extends the generic server and implements the behavior of a mobile network
%%% switch.
%%%
%%% The switch provides a number of API functions, allowing other processes to
%%% interact with it. These functions indirectly manipulate the internal state
%%% of the switch by sending it messages.
%%%
%%% SMS messages are simply implemented using the standard messages sending
%%% functionality provided by Erlang (i.e. using !).
%%%
%%% The state of the switch is maintained as a list of mobile phone subscribers,
%%% each having a mobile number (called a MSISDN), a unique identifier
%%% (conveniently taken to be the PID of the mobile process), and a list of
%%% pending messages (messages that have not been yet delivered) that is
%%% initally set to be [].
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
%%%     is conveniently used to send messages (using the primitive !) to other
%%%     processes that represent the mobile phones of other users. A user who is
%%%     attached to the mobile network is represented by an actual Pid value
%%%     that is used to send messages with. When the user is not attached to the
%%%     mobile network, Pid is set to 0 to denote non-attachment.
%%%   * Pending::pending() is the list of messages that have not yet been
%%%     delivered because the recipient is not currently attached to the mobile
%%%     network (i.e. the tuple for a particular MSISDN has its Pid set to 0 and
%%%     cannot recieve process messages). Pending is reset to [] when the mobile
%%%     corresponding subscriber MSISDN attaches itself to the mobile network.
%%% ----------------------------------------------------------------------------
-module(switch).
-include("log.hrl").

%%% Switch API exports.
-export([start/0, stop/0]).

%%% Callback exports.
-export([init/1, handle/2, handle_exit/3]).

%%% Client API exports.
-export([status/0, subscribe/1, unsubscribe/1, attach/1, detach/0, send_msg/2]).


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
  server_gen:start(?MODULE, []).

%% -----------------------------------------------------------------------------
%% Stops the switch.
%% stop() where:
%%   no parameters are accepted.
%% Returns: Status:term(), the switch termination status.
%% -----------------------------------------------------------------------------
stop() ->
  server_gen:stop(?MODULE).


%%% ------------------------------------------------------------------------ %%%
%%% Callbacks.                                                               %%%
%%% ------------------------------------------------------------------------ %%%

%% -----------------------------------------------------------------------------
%% Initializes the switch state before it enters the main process loop.
%% init([]) where:
%%   * [] is the empty argument list.
%% Returns: {ok, []}, where the inital state of the server is the empty list.
%%
%% The switch process registers itself as a named process. The name used is that
%% of the switch module itself. The switch is started as a system process to
%% to allow itself to handle the termination of other processes linked to it.
%% -----------------------------------------------------------------------------
init([]) ->
  ?DEBUG("Starting switch process."),

  % The switch is a long-lived process; register it.
  register(?MODULE, self()),

  % Trap exits so that we are able to detect and handle cases where linked
  % mobile processes have terminated.
  process_flag(trap_exit, true),
  {ok, []}.

%% -----------------------------------------------------------------------------
%% Returns the internal switch state.
%% handle(status, Store) where:
%%   * status is the message request.
%%   * Store::subs() is the current list of subscribed MSISDNs managed by the
%%     switch.
%% Returns: {{ok, Store::subs()}, Store::subs()} where Store is the list of all
%%          subscribed MSISDNs.
%%
%% The message request is handled synchronously.
%% -----------------------------------------------------------------------------
handle(status, Store) ->
  {{ok, Store}, Store};

%% -----------------------------------------------------------------------------
%% Handles a 'subscribe new Msisdn' request message.
%% handle({subscribe, Msisdn}, Store) where:
%%   * {subscribe, Msisdn::string()} is the message request containing the new
%%     Msisdn to be subscribed.
%%   * Store::subs() is the current list of subscribed MSISDNs managed by the
%%     switch.
%% Returns: {{ok, subscribed}, NewStore::subs()} when the Msisdn to be
%%          subscribed does not exist in Store. NewStore contains all the
%%          subscribed MSISDNs previously in Store, together with the new
%%          Msisdn.
%%          | {{error, already_subscribed}, Store::subs()} when Msisdn is
%%          already subscribed. Store is the same unmodified list of MSISDNs.
%%
%% The message request is handled synchronously.
%% -----------------------------------------------------------------------------
handle({subscribe, Msisdn}, Store) ->
  % TODO: Add implementation.
  ok;

%% -----------------------------------------------------------------------------
%% Handles an 'unsubscribe existing Msisdn' request message.
%% handle({unsubscribe, Msisdn}, Store) where:
%%   * {unsubscribe, Msisdn::string()} is the message request containing the
%%     existing Msisdn to be unsubscribed.
%%   * Store::subs() is the current list of subscribed MSISDNs managed by the
%%     switch.
%% Returns: {{ok, unsubscribed}, NewStore::subs()} when the Msisdn to be
%%          unsubscribed is found in Store. NewStore contains all the
%%          subscribed MSISDNs previously in Store, less the removed Msisdn.
%%          | {{error, not_subscribed}, Store::subs()} when Msisdn is not
%%          subscribed. Store is the same unmodified list of MSISDNs.
%%
%% The message request is handled synchronously.
%% -----------------------------------------------------------------------------
handle({unsubscribe, Msisdn}, Store) ->
  % TODO: Add implementation.
  ok;

%% -----------------------------------------------------------------------------
%% Handles a 'attach Msisdn to mobile network' request messge.
%% handle({attach, Pid, Msisdn}, Store) where:
%%   * {attach, Pid::pid(), Msisdn::string()} is the message request containing
%%     an existing Msisdn to be attached to the mobile network using the Pid of
%%     its process.
%%   * Store::subs() is the current list of subscribed MSISDNs managed by the
%%     switch.
%% Returns: {{ok, attached, Pending::pending()}, NewStore::subs()} when the
%%          Msisdn to be attached with Pid is found in Store. NewStore contains
%%          all the subscribed MSISDNs previously in Store, with the entry for
%%          Msisdn modifed so that it is associated with the new Pid. Pending
%%          contains the list of pending text messages that were not delivered
%%          to the mobile because it was previously not attached to the mobile
%%          network.
%%          | {{error, already_attached}, Store::subs()} when Msisdn is already
%%          attached to the mobile network (i.e. the Pid it is associated to is
%%          not 0). Store is the same unmodified list of MSISDNs. This error is
%%          also returned when the specified Pid is already associated with
%%          a different MSISDN in Store. Pid can be associated with only one
%%          MSISDN.
%%          | {{error, not_subscribed}, Store::subs()} when Msisdn is not
%%          subscribed. Store is the same unmodified list of MSISDNs.
%%
%% The message request is handled synchronously.
%% -----------------------------------------------------------------------------
handle({attach, Pid, Msisdn}, Store) ->
  % TODO: Add implementation.
  ok;

%% -----------------------------------------------------------------------------
%% Handles a 'detach Msisdn from mobile network' request messge.
%% handle({detach, Pid}, Store) where:
%%   * {detach, Pid::pid()} is the message request containing the Pid to be
%%     set to 0, denoting the corresponding MSISDN entry in Store as being
%%     detached from the mobile network.
%%   * Store::subs() is the current list of subscribed MSISDNs managed by the
%%     switch.
%% Returns: {{ok, detached}, NewStore::subs()} when Pid is found in Store.
%%          NewStore contains all the subscribed MSISDNs previously in Store,
%%          with the entry for Pid modfied (Pid is set to 0) so that the
%%          corresponding MSISDN is now considered to be detached from the
%%          mobile network.
%%          | {{error, not_attached}, Store::subs()} when Pid is not found in
%%          Store. Store is the same unmodified list of MSISDNs.
%%
%% The message request is handled synchronously.
%% -----------------------------------------------------------------------------
handle({detach, Pid}, Store) ->
  % TODO: Add implementation.
  ok;

%% -----------------------------------------------------------------------------
%% Handles a 'send message text' request message.
%% handle({send_msg, Pid, ToMsisdn, Msg}, Store) where:
%%   * {send_msg, Pid::pid(), ToMsisdn::string(), Msg:String()} is the message
%%     request containing the Pid of the sending process, the recipient MSISDN
%%     ToMsisdn, and the text Msg to be sent.
%%   * Store::subs() is the current list of subscribed MSISDNs managed by the
%%     switch.
%% Returns: {{ok, msg_sent}, Store::subs()} when Pid is attached to the mobile
%%          network, ToMsisdn is subscribed and it is also attached to the
%%          network. Store is the same unmodified list of MSISDNs.
%%          | {{ok, msg_queued}, NewStore::subs()} when Pid is attached to the
%%          mobile network, ToMsisdn is subscribed but it is currently not
%%          attached to the network. Msg is not discarded, but appended to the
%%          list of messages that have yet to be delivered for ToMsisdn.
%%          NewStore contains all the subscribed MSISDNs previously in Store,
%%          with the entry for ToMsisdn modifed so that its list of pending
%%          messages contains also Msg.
%%          | {{error, to_not_subscribed}, Store::subs()} when ToMsisdn is not
%%          subscribed. Store is the same unmodified list of MSISDNs.
%%          {{error, not_attached}, Store::subs()} when Pid is not found in
%%          Store. Store is the same unmodified list of MSISDNs.
%%
%% The message request is handled synchronously.
%% -----------------------------------------------------------------------------
handle({send_msg, Pid, ToMsisdn, Msg}, Store) ->
  % TODO: Add implementation.
  ok.

%% -----------------------------------------------------------------------------
%% Handles the process termination messages.
%% handle_exit(Pid, _, Store) where:
%%   * Pid::pid() is the PID of the linked terminated process.
%%   * _Reason::term() is the linked process termination reason.
%%   * Store::subs() is the current list of subscribed MSISDNs managed by the
%%     switch.
%% Returns: NewStore::subs() when the terminated Pid is found in Store.
%%          NewStore contains all the subscribed MSISDNs previously in Store,
%%          with the entry for Pid modfied (Pid is set to 0) so that the
%%          corresponding MSISDN is now considered to be detached from the
%%          mobile network.
%%          | Store::subs() when Pid is not found in Store. Store is the same
%%          unmodified list of MSISDNs.
%%
%% The message request is handled asynchronously.
%% -----------------------------------------------------------------------------
handle_exit(Pid, _Reason, Store) ->
  % TODO: Add implementation.
  ok.


%%% ------------------------------------------------------------------------ %%%
%%% Client API.                                                              %%%
%%% ------------------------------------------------------------------------ %%%

%% -----------------------------------------------------------------------------
%% (Operator) Inquires the switch's status.
%% status() where:
%%   no parameters are accepted.
%% Returns: {ok, Store::subs()} where Store is the list of all subscribed
%%          MSISDNs.
%% -----------------------------------------------------------------------------
status() ->
   server_gen:rpc(?MODULE, status).

%% -----------------------------------------------------------------------------
%% (Operator) Subscribes the specified Msisdn to the switch.
%% subscribe(Msisdn) where:
%%   * Msisdn::string() is the new Msisdn to be subscribed.
%% Returns: {ok, subscribed} when the Msisdn to be subscribed does not exist on
%%          the switch.
%%          | {error, already_subscribed} when Msisdn is already subscribed.
%% -----------------------------------------------------------------------------
subscribe(Msisdn) ->
  server_gen:rpc(?MODULE, {subscribe, Msisdn}).

%% -----------------------------------------------------------------------------
%% (Operator) Unsubscribes the specified Msisdn from the switch.
%% unsubscribe(Msisdn) where:
%%   * Msisdn::string() is the existing Msisdn to be unsubscribed.
%% Returns: {ok, unsubscribed} when the Msisdn to be unsubscribed is found on
%%          the switch.
%%          | {error, not_subscribed} when Msisdn is not subscribed.
%% -----------------------------------------------------------------------------
unsubscribe(Msisdn) ->
  server_gen:rpc(?MODULE, {unsubscribe, Msisdn}).

%% -----------------------------------------------------------------------------
%% (Mobile) Attaches the specified Msisdn to the mobile network.
%% attach(Msisdn) where:
%%   * Msisdn::string() is the existing Msisdn to be attached to the mobile
%%     network.
%% Returns: {ok, attached, Pending::pending()} when the Msisdn to be attached is
%%          found on the switch. Pending contains the list of pending text
%%          messages that were not delivered to the mobile because it was
%%          previously not attached to the mobile network.
%%          | {error, already_attached} when Msisdn is already attached to the
%%          mobile network.
%%          | {error, not_subscribed} when Msisdn is not subscribed.
%% -----------------------------------------------------------------------------
attach(Msisdn) ->
  server_gen:rpc(?MODULE, {attach, self(), Msisdn}).

%% -----------------------------------------------------------------------------
%% (Mobile) Detaches the specified Msisdn from the mobile network.
%% detach() where:
%%   no parameters are accepted.
%% Returns: {ok, detached} when successfully detached from the mobile network.
%%          | {error, not_attached} when the current process is not attached.
%% -----------------------------------------------------------------------------
detach() ->
  server_gen:rpc(?MODULE, {detach, self()}).

%% -----------------------------------------------------------------------------
%% (Mobile) Sends a text Msg to the subscriber To.
%% send_msg(To, Msg) where:
%%   * ToMsisdn::string() is the recipient.
%%   * Msg::string() is the text messges to be sent.
%% Returns: {ok, msg_sent} when the sending process is attached to the mobile
%%          network, ToMsisdn is subscribed and it is also attached to the
%%          network.
%%          | {ok, msg_queued} when the sending process is attached to the
%%          mobile network, ToMsisdn is subscribed but it is currently not
%%          attached to the network.
%%          | {error, to_not_subscribed} when ToMsisdn is not subscribed.
%%          | {error, not_attached} when the sending process is not attached.
%% -----------------------------------------------------------------------------
send_msg(ToMsisdn, Msg) ->
  server_gen:rpc(?MODULE, {send_msg, self(), ToMsisdn, Msg}).
