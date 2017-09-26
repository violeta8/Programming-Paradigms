%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%%
%%% Implements the behaviour of a generic supervisor that can monitor and
%%% restart processes when they crash.
%%% ----------------------------------------------------------------------------
-module(super_gen).
-include("log.hrl").

%%% Generic supervisor API exports.
-export([start/2, start_link/2, init/2, loop/1]).


%%% ------------------------------------------------------------------------ %%%
%%% Generic supervisor API.                                                  %%%
%%% ------------------------------------------------------------------------ %%%


%% -----------------------------------------------------------------------------
%% Starts the supervisor.
%%
%% Note: a new process is spawned, passing to the the module name 'Mod' and the
%% arguments that are to be used to initialise the supervisor.
%% The module name 'Mod' is a variable that contains the name of the module that
%% implements all the callbacks required by this generic supervisor. There is 1
%% callback to implement:
%% 1. the init/2 implementation should accept as parameters the module name
%%    'Mod' and the arguments 'Args'. It should return the pair
%%    '{Strategy, Mfa}' that is used to bootstrap the supervisor (see below).
%% -----------------------------------------------------------------------------
start(Mod, Args) ->
  spawn(?MODULE, init, [Mod, Args]).

%% -----------------------------------------------------------------------------
%% Starts the supervisor and links it to the process that called start_link/1.
%% -----------------------------------------------------------------------------
start_link(Mod, Args) ->
  spawn_link(?MODULE, init, [Mod, Args]).


%%% ------------------------------------------------------------------------ %%%
%%% Internal generic supervisor functions.                                   %%%
%%% ------------------------------------------------------------------------ %%%

%% -----------------------------------------------------------------------------
%% Initialises the supervisor.
%%
%% Note: this function starts the process (i.e. it is the one that is spawned).
%% It calls the Mod:init/1 function that should be located in a different module
%% that implements init/1 (e.g. name_server_super).
%% The argument 'Args' passed to the implementation of init/1 can be used to
%% perform additional initialisation. The implementation of init/1 should return
%% a pair '{Strategy, Mfa}'. The 'Mfa' is the actual function that we wish the
%% supervisor to invoke (e.g. {name_server, start_link, []}), whereas the
%% restart 'Strategy' must be one of the following:
%% 1. 'always': this means that the child process can be restarted by the
%%    supervisor an infinite number of times when it crashes.
%% 2. '{max, Max}': this instructs the supervisor to restart a crashed child
%%    process a fixed number of times, as specified by 'Max'. When 'Max' is
%%    reached, the child process is not restarted and the supervisor itself
%%    crashes.
%% The returned pair '{Strategy, Mfa}' is used to call the process loop/1 below.
%% -----------------------------------------------------------------------------
init(Mod, Args) ->
  {Strategy, Mfa = {M, F, A}} = Mod:init(Args),

  % Transform the process into a system process. Recall that a system process
  % traps exit signals that come from other processes: these are deposited to
  % our own mailbox in the form of messages which we can handle as seen below
  % in loop/2.
  process_flag(trap_exit, true),

  % Call the MFA that launches the child process. Note that we called it using
  % the apply/3 BIF.
  % Note: we are calling the process MFA, *not* spawning it. Spawning the
  % process is taken care of by the MFA (e.g. name_server:start_link/1). Had we
  % done it with spawn_link/3 (e.g. spawn_link(name_server, start_link, [])),
  % another process would have been spawned and linked in this example,
  % name_server:start_link/1) to this supervior. The process
  % (e.g. name_server:start_link/1) would then itself spawn the real process
  % (the one which we were supposed to supervise) and then after doing so,
  % terminates normally. Because of the incorrect link that would exist between
  % this supervior and the launcher process (in this example,
  % name_server:start_link/1), the supervior would receive a {'EXIT', From,
  % normal} message.
  %
  % By applying (i.e. we are literally calling the function from here) the
  % function, we are calling the launcher function (for this example,
  % name_server:start_link/1) directly. This then spawns and links the process
  % to this supervisor, because the supervisor is the caller of this function.
  ChildPid = apply(M, F, [A]),
  loop({init_strategy(Strategy), ChildPid, Mfa}).

%% -----------------------------------------------------------------------------
%% Sets up the data that is needed by the restart strategy specified by the
%% caller.
%%
%% Note: this first clause simply matches the strategy 'max', and returns a
%% triple '{max, 1, Max}'. This is fed to the process loop below. The number 1
%% will be incremented upon each child crash until 'Max' is reached.
%% -----------------------------------------------------------------------------
init_strategy({max, Max}) -> {max, 1, Max};

%% -----------------------------------------------------------------------------
%% Note: this second clause matches the 'always' atom, and since no special
%% variables or counts are maintained for this type of restart strategy, the
%% same 'always' atom is retruned instead. See the body of loop/1 below for how
%% these two restart strategies are actually implemented.
%% -----------------------------------------------------------------------------
init_strategy(always) -> always.

%% -----------------------------------------------------------------------------
%% The main process loop.
%%
%% Note: the state for the process loop consists of a tuple containing 3 items
%% (remember that the state can be anything we choose, and for this supervisor
%% implementation, we chose this format):
%% 1. The 'Strategy' component of the state triple keeps the restart strategy
%%    that is in used by the supervisor (recall, these can be the atom always or
%%    the triple '{max, N, Max}').
%% 2. The 'ChildPid' component keeps the PID of the current child the supervisor
%%    is monitoring. This we use to match messages that are sent to this
%%    supervisor so that we only process those that originate from our child
%%    process.
%% 3. The 'Mfa' component is the function that allows us to spawn the child
%%    process. We need it in this loop so that when the child crashed, we have
%%    the MFA that allows us to restart it afresh.
%% There are 3 types of messages the receive statement handles:
%% 1. '{'EXIT', ChildPid, normal}' messages match the case where the child has
%%    terminated normally (e.g. in the case of name_server, this can be done
%%    by calling stop/0). In this case, no restart is required.
%% 2. '{'EXIT', ChildPid, Reason}' messages match the case where the child has
%%    terminated abnormally (e.g. by invoking the exit/2 BIF on the process).
%%    In order to determine what to do for this case, we use the restart
%%    strategy. If the strategy is 'max', then we check whether the maximum
%%    number or child process restarts has been reached. If it has, the child
%%    is not restarted, and the supervisor crashes (note the exit(kill) call
%%    below in the case..of expression). If the startegy is 'always', then we
%%    simply restart the child process.
%% -----------------------------------------------------------------------------
loop(State = {Strategy, ChildPid, Mfa = {M, F, A}}) ->
  receive
    {'EXIT', ChildPid, normal} ->
      ?DEBUG("Process ~p exited normally.", [ChildPid]),
      loop(State);

    {'EXIT', ChildPid, Reason} ->
      ?TRACE("Process ~p exited with reason: ~p.", [ChildPid, Reason]),

      case Strategy of
        {max, Max, Max} ->
          ?INFO("Limit reached (~p) and won't restart child again!", [Max]),
          exit(kill);
        {max, N, Max} ->
          ?DEBUG("Limit not reached (~p/~p); restarting child.", [N, Max]),
          NewChildPid = apply(M, F, [A]),
          loop({{max, N + 1, Max}, NewChildPid, Mfa});
        always ->
          ?DEBUG("No limits imposed; restarting child."),
          NewChildPid = apply(M, F, [A]),
          loop({Strategy, NewChildPid, Mfa})
      end
  end.
