%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%%
%%% Provides a batch of unit tests for worksheet 4.
%%% ----------------------------------------------------------------------------
-module(ws_3_test).
-include_lib("eunit/include/eunit.hrl").


%%% ------------------------------------------------------------------------ %%%
%%% Unit tests.                                                              %%%
%%% ------------------------------------------------------------------------ %%%

%% -----------------------------------------------------------------------------
%% Tests the calculator functionality.
%% -----------------------------------------------------------------------------
calc_test_() -> {foreach,
  fun() ->

    % Set up process.
    ws_3:calc()
  end,
  fun(Pid) ->

    % Teardown process.
    exit(Pid, kill)
  end,
  [
    fun(Pid) ->
      {"Stop calculator", ?_test(begin

        % Stop calculator, which should terminate.
        Pid ! {self(), tag, stop},
        ?assertEqual({tag, {ok, stopped}}, receive X -> X end),
        ?assertEqual(undefined, process_info(Pid))
      end)}
    end,
    fun(Pid) ->
      {"Add two numbers", ?_test(begin

        % Add 5 and 1 which shoud return the result 6.
        Pid ! {self(), tag, {add, 5, 1}},
        ?assertEqual({tag, {ok, 6}}, receive X -> X end),
        ?assertNotEqual(undefined, process_info(Pid))
      end)}
    end,
    fun(Pid) ->
      {"Subtract two numbers", ?_test(begin

        % Subtract 1 from 5 which should return -4.
        Pid ! {self(), tag, {sub, 5, 1}},
        ?assertEqual({tag, {ok, 4}}, receive X -> X end),
        ?assertNotEqual(undefined, process_info(Pid))
      end)}
    end
  ]
}.

%% -----------------------------------------------------------------------------
%% Tests the calculator functionality with process linking.
%% -----------------------------------------------------------------------------
calc_link_test_() -> [
  {"Kill spawning process to trigger exit signal to calculator", ?_test(begin

    % Set up caclulator process in another spawner and return its PID to the
    % test process. It is essential that we spawn the calculator process from
    % another process, and not from the test (i.e. this) process, since the
    % test that we need to perform is to kill the spawner process (if we kill
    % the test (i.e. this) process we would not be able to perform the
    % assertions!
    TestPid = self(),
    SpawnerPid = spawn(
      fun() ->
        CalculatorPid = ws_3:calc_link(),

        % Since the is another process, there is not way we can send the
        % calculator PID to the test process directly using a function return,
        % so instead we send it as a message.
        TestPid ! CalculatorPid,

        % To prevent the process from dying normally (as this would not give us
        % enough time to actually kill this spawner process, we block. We do
        % this using an empty receive clause with a timeout of infinity.
        receive
          after
            infinity ->
              ok
        end
      end),

    % Receive the calculator PID that was sent to us by the spawner process
    % (i.e. the process above labelled by SpawnerPid). Note that this variable
    % CalculatorPid is *different* from the one created in the process labelled
    % by SpawnerPid - remember scopes, and that the scope of CalculatorPid in
    % the process SpawnerPid is local to that process and is not visible from
    % outside it, and that is the reason we sent it via a message
    % (i.e. TestPid ! CalculatorPid) in the first place to the test process
    % (i.e. the one labelled by TestPid, which is, in fact self()).
    CalculatorPid = receive X -> X end,

    % Kill the spawner process (labelled by SpawnerPid) and make sure that
    % the calculator process (labelled by CalculatorPid) dies as well. Recall
    % that SpawnerPid is linked to CalculatorPid.
    % pause_after(fun() -> exit(SpawnerPid, kill) end, ?PAUSE_MILLIS),
    exit(SpawnerPid, kill),
    pause_until_dead(SpawnerPid, 1000),
    pause_until_dead(CalculatorPid, 1000),
    ?assertEqual(undefined, process_info(SpawnerPid)),
    ?assertEqual(undefined, process_info(CalculatorPid))
  end)}
].

%% -----------------------------------------------------------------------------
%% Tests the calculator supervisor functionality.
%% -----------------------------------------------------------------------------
calc_super_test_() -> [
  {"Start supervisor and kill the calculator process multiple times", ?_test(begin

    % Spawn the supervisor process which in turn, spawns the calculator process.
    % The supervisor sends back the calculator PID in the form of a message, so
    % that we have a handle on it.
    SupervisorPid = ws_3:calc_super(),
    CalculatorPid = receive X -> X end,

    % Kill the calculator process, which should be immediately restarted by
    % the supervisor. The new PID should be sent by the supervisor to us.
    exit(CalculatorPid, kill),
    RestartedCalculatorPid1 = receive Y -> Y end,
    ?assertNotEqual(CalculatorPid, RestartedCalculatorPid1),

    % Kill the calculator process a second time, and receive its new PID.
    exit(RestartedCalculatorPid1, kill),
    RestartedCalculatorPid2 = receive Z -> Z end,
    ?assertNotEqual(RestartedCalculatorPid1, RestartedCalculatorPid2),

    % Confirm that the supervisor restart count is equal to 2.
    SupervisorPid ! {self(), tag, status},
    ?assertEqual({tag, {ok, 2}}, receive S -> S end)
  end)}
].


%%% ------------------------------------------------------------------------ %%%
%%% Helper functions.                                                        %%%
%%% ------------------------------------------------------------------------ %%%

%% -----------------------------------------------------------------------------
%% Blocks the caller until the specified process PID is dead.
%% pause_until_dead(Pid) where:
%%   * Pid:pid() is the process PID to wait for.
%%   * Millis:number() is the number of milliseconds to wait before giving up.
%% Returns: ok once the process PID is dead.
%% -----------------------------------------------------------------------------
pause_until_dead(_, Millis) when is_number(Millis), Millis =< 0 ->
  throw(wait_timeout);
pause_until_dead(Pid, Millis) when is_pid(Pid), is_number(Millis)->
  case process_info(Pid) of
    undefined ->
      ok;
    _ ->
      timer:sleep(100),
      pause_until_dead(Pid, Millis - 100)
  end.
