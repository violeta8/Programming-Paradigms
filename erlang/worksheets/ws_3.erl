%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%%
%%% Model answers for worksheet 4 questions.
%%% ----------------------------------------------------------------------------
-module(ws_3).
-export([calc/0, calc_link/0, calc_super/0, rpc/2]).
-export([calc_loop/0, calc_super_loop/3]). % Internal exports.

%% -----------------------------------------------------------------------------
%% Spawns a process which loops indefinitely until stopped, acting as a simple
%% calculator.
%% calc() where:
%%   no parameters are accepted.
%% Returns: Pid::pid(), the PID of the newly spawned calculator process.
%% -----------------------------------------------------------------------------
calc() ->
  spawn(?MODULE, calc_loop, []).

% This introduces the RPC which would have been explained in the tutorial.

%% -----------------------------------------------------------------------------
%% The calculator process loop.
%% calc_loop() where:
%%   no parameters are accepted.
%% Does not return.
%%
%% The loop handles 4 type of messages:
%%
%% 1. A 'stop' request message request with the following format:
%%      * stop atom that causes the process loop to terminate.
%%    Returns: {ok, stopped} to the sending process as an exit status.
%%
%% 2. An 'add two numbers' request message with the following format:
%%      * {add, X::number(), Y::number()} where X and Y are the numbers to add
%%        together.
%%    Returns {ok, Sum::number()} where Sum is the addition of X and Y.
%%
%% 3. A 'subtract two numbers' request message with the following format:
%%      * {sub, X::number(), Y::number()} where Y is the number to subtract from
%%        X.
%%    Returns {ok, Difference::number()} where Difference is the subtraction of
%%    Y from X.
%%
%% 4. Any other message that does not conform to the protocol described in 1 to
%%    3 above. The message is simply output to the screen and discarded.
%% -----------------------------------------------------------------------------
calc_loop() ->
  % TODO: Add implementation.
  ok.

%% -----------------------------------------------------------------------------
%% Spawns a process which loops indefinitely until stopped, acting as a simple
%% calculator. The spawned process is linked to the spawning process.
%% calc_link() where:
%%   no parameters are accepted.
%% Returns: Pid::pid(), the PID of the newly spawned calculator process.
%% -----------------------------------------------------------------------------
calc_link() ->
  % TODO: Add implementation.
  ok.

% To reverse the effect of linking unlink/1 from the shell.
% Run link/1 and unlink/1 and test manually on shell.

%% -----------------------------------------------------------------------------
%% Spawns a process that acts as a supervisor for the calculator process.
%% calc_super() where:
%%   no parameters are accepted.
%% Returns: Pid::pid(), the PID of the newly spawned calculator supervisor
%%          process.
%%
%% The calculator process is linked to the supervisor and restarted indefinitely
%% whenever it dies. The supervisor takes case to set itself as a system process
%% so as to trap and handle any exit messages that may be emitted by the
%% calculator. The supervisor also keeps count of the number of restarts: this
%% can be queried by sending it the appropriate message (see below).
%%
%% Note, in order to also permit the calling process to get a handle on the PID
%% of the newly spawned (or restarted) calculator process, its PID is sent to
%% in the form of a message to the calling process.
%% -----------------------------------------------------------------------------
calc_super() ->

  % Save the PID of the caller process in order to send it back the PID of
  % the calculator process about to be spawned.
  CallerPid = self(),

  % Spawn a convenience init function that sets the process as a system
  % process and launches the calculator.
  spawn(fun() ->
    process_flag(trap_exit, true),
    Pid = calc_link(),

    io:fwrite(user, "Started calc with PID: ~p.~n", [Pid]),

    % Send calculator PID to the process that initially called the supervisor.
    % This we do for reference purposes.
    CallerPid ! Pid,

    calc_super_loop(0, Pid, CallerPid)
  end).

%% -----------------------------------------------------------------------------
%% The calculator supervisor process loop.
%% calc_super_loop(Restarts, Pid, CallerPid) where:
%%   * Restarts::integer() is the number of times the calculator process has
%%     been restarted.
%%   * Pid::pid() is the PID of the newly spawned calculator process that will
%%     be used to check that 'EXIT' messages are indeed being sent from the
%%     calculator process, and not some other process. This prevents the
%%     supervisor from spawning a new calculator process each time an 'EXIT'
%%     message is received; instead it forces it to spawn a new calculator
%%     process only when the 'EXIT' message was sent by the calculator process
%%     whose PID we kept.
%%   * CallerPid::pid() is the PID of the process that called the supervisor.
%% Does not return.
%%
%% The loop handles 2 type of messages:
%%
%% 1. An 'exit' request message with the following format:
%%      * {'EXIT', From::pid(), Reason::any()} where From is the terminated
%%        process, and Reason, the termination reason.
%%    Returns: The new PID of the calculator process to the internally saved
%%             value of the process that called the supervisor (i.e. CallerPid).
%%
%% 2. A 'return the number of restarts' request message with the following
%%    format:
%%      * status atom that instructs the process loop to return the its state.
%%    Returns: {ok, Restarts::integer()} where Restarts is the number of times
%%             the calculator process has been restarted.
%% -----------------------------------------------------------------------------
calc_super_loop(Restarts, Pid, CallerPid) ->
  % TODO: Add implementation.
  ok.

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
