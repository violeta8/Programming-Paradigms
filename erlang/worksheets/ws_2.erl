%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%%
%%% Model answers for worksheet 2 questions.
%%% ----------------------------------------------------------------------------
-module(ws_2).
-export([log_msg_once/0, log_msg/0, counter/0, counter_named/0]).
-export([counter_loop/1]). % Internal exports.

%% -----------------------------------------------------------------------------
%% Spawns a process which receives a message, displays it and then dies.
%% log_msg_once() where:
%%   no parameters are accepted.
%% Returns: Pid::pid(), the PID of the newly spawned message process.
%%
%% This handles 1 type of message:
%%
%% 1. A message request that is output to the screen.
%% -----------------------------------------------------------------------------
log_msg_once() ->
  % TODO: Add implementation.
  ok.

%% -----------------------------------------------------------------------------
%% Spawns a process which loops indefinitely, receiving messages and displaying
%% them.
%% log_msg_once() where:
%%   no parameters are accepted.
%% Returns: Pid::pid(), the PID of the newly spawned message process.
%% -----------------------------------------------------------------------------
log_msg() ->
  spawn(fun log_msg_loop/0).

%% -----------------------------------------------------------------------------
%% The log message process loop.
%% log_msg_loop() where:
%%   no parameters are accepted.
%% Does not return.
%%
%% The loop handles 1 type of message:
%%
%% 1. A message request that is output to the screen.
%% -----------------------------------------------------------------------------
log_msg_loop() ->
  % TODO: Add implementation.
  ok.

%% -----------------------------------------------------------------------------
%% Spawns a process which loops indefinitely until stopped, acting as a counter
%% that keeps track of a single integer.
%% counter() where:
%%   no parameters are accepted.
%% Returns: Pid::pid(), the PID of the newly spawned counter process.
%% -----------------------------------------------------------------------------
counter() ->
  spawn(?MODULE, counter_loop, [0]).

%% -----------------------------------------------------------------------------
%% The counter process loop.
%% counter_loop(Count) where:
%%   * Count::integer() is the starting count value.
%% Does not return.
%%
%% The loop handles 6 type of messages:
%%
%% 1. A 'stop' request message request with the following format:
%%      * stop atom that causes the process loop to terminate.
%%    Does not return anything to the sending process.
%%
%% 2. A 'get' request message with the following format:
%%      * {get, From::pid()} is the message request containing the reply-to
%%        address From of the sending process.
%%    Returns: {ok, Count::integer()} where Count is the current value stored
%%             in the server.
%%
%% 3. A 'put' request message with the following format:
%%      * {put, From::pid(), Amt::integer()} is the message request containing
%%        the amount to be added to the current count maintained in the server
%%        state Count variable. Amt can be both positive or negative.
%%    Returns: {ok, put, Old::integer()} where Old is the value of count stored
%%             in the server before it was updated.
%%
%% 4. An 'increment' request message request with the following format:
%%      * {inc, From::pid()} is the message request containing the reply-to
%%        address From of the sending process.
%%    Returns: {ok, inc, Old::integer()} to inform the sending process that
%%             the increment was performed successfully. Old is the old count
%%             value before it was incremented.
%%
%% 5. A 'decrement' request message with the following format:
%%      * {dec, From::pid()} is the message request containing the reply-to
%%        address From of the sending process.
%%    Returns: {ok, dec, Old::integer()} to inform the sending process that
%%             the decrement was performed successfully. Old is the old count
%%             value before it was decremented.
%%
%% 6. Any other message that does not conform to the protocol described in 1 to
%%    5 above. The message is simply output to the screen and discarded.
%% -----------------------------------------------------------------------------
counter_loop(Count) ->
  % TODO: Add implementation.
  ok.

%% -----------------------------------------------------------------------------
%% Spawns a process which loops indefinitely until stopped, acting as a counter
%% that keeps track of a single integer.
%% counter_named() where:
%%   no parameters are accepted.
%% Returns: Pid::pid(), the PID of the newly spawned counter process.
%%
%% The process is registered against the name 'counter'.
%% -----------------------------------------------------------------------------
counter_named() ->
  % TODO: Add implementation.
  ok.
