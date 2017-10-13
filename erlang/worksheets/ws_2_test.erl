%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%%
%%% Provides a batch of unit tests for worksheet 3.
%%% ----------------------------------------------------------------------------
-module(ws_2_test).
-include_lib("eunit/include/eunit.hrl").


%%% ------------------------------------------------------------------------ %%%
%%% Unit tests.                                                              %%%
%%% ------------------------------------------------------------------------ %%%

%% -----------------------------------------------------------------------------
%% Tests the log message once functionality.
%% -----------------------------------------------------------------------------
log_msg_once_test_() -> {foreach,
  fun() ->

    % Set up process.
    ws_2:log_msg_once()
  end,
  fun(_) ->
    ok
  end,
  [
    fun(Pid) ->
      {"Send a single message", ?_test(begin

        % Send a single message, which should terminate the switch.
        Pid ! hello,
        pause_until_dead(Pid, 1000),
        ?assertEqual(undefined, process_info(Pid))
      end)}
    end
  ]
}.

%% -----------------------------------------------------------------------------
%% Tests the log message functionality.
%% -----------------------------------------------------------------------------
log_msg_test_() -> {foreach,
  fun() ->

    % Set up process.
    ws_2:log_msg()
  end,
  fun(Pid) ->

    % Teardown process.
    exit(Pid, kill)
  end,
  [
    fun(Pid) ->
      {"Send a single message", ?_test(begin

        % Send a number of messages; the switch should not terminate.
        [Pid ! X || X <- lists:seq(1, 5)],
        ?assertNotEqual(undefined, process_info(Pid))
      end)}
    end
  ]
}.

%% -----------------------------------------------------------------------------
%% Tests the counter functionality.
%% -----------------------------------------------------------------------------
counter_test_() -> {foreach,
  fun() ->

    % Set up process.
    ws_2:counter()
  end,
  fun(Pid) ->

    % Teardown process.
    exit(Pid, kill)
  end,
  [
    fun(Pid) ->
      {"Stop counter", ?_test(begin

        % Stop counter, which should terminate.
        Pid ! stop,
        pause_until_dead(Pid, 1000),
        ?assertEqual(undefined, process_info(Pid))
      end)}
    end,
    fun(Pid) ->
      {"Get counter state", ?_test(begin

        % Get the internal counter state which should be 0.
        Pid ! {get, self()},
        ?assertEqual({ok, 0}, receive X -> X end),
        ?assertNotEqual(undefined, process_info(Pid))
      end)}
    end,
    fun(Pid) ->
      {"Put in counter", ?_test(begin

        % Put 500 into the counter, which should return 0 as the old value.
        Pid ! {put, self(), 500},
        ?assertEqual({ok, put, 0}, receive X -> X end),
        ?assertNotEqual(undefined, process_info(Pid)),

        % Put 20 into the counter, which now should return 500, reflecting the
        % old value that was put in earlier.
        Pid ! {put, self(), 20},
        ?assertEqual({ok, put, 500}, receive X -> X end),
        ?assertNotEqual(undefined, process_info(Pid))
      end)}
    end,
    fun(Pid) ->
      {"Increment counter", ?_test(begin

        % Increment the counter which should return 0 as the old value.
        Pid ! {inc, self()},
        ?assertEqual({ok, inc, 0}, receive X -> X end),
        ?assertNotEqual(undefined, process_info(Pid)),

        % Increment the counter again, which now should return 1 as the old
        % value.
        Pid ! {inc, self()},
        ?assertEqual({ok, inc, 1}, receive X -> X end),
        ?assertNotEqual(undefined, process_info(Pid))
      end)}
    end,
    fun(Pid) ->
      {"Decrement counter", ?_test(begin

        % Decrement the counter which should return 0 as the old value.
        Pid ! {dec, self()},
        ?assertEqual({ok, dec, 0}, receive X -> X end),
        ?assertNotEqual(undefined, process_info(Pid)),

        % Decrement the counter again, which now should return -1 as the old
        % value.
        Pid ! {dec, self()},
        ?assertEqual({ok, dec, -1}, receive X -> X end),
        ?assertNotEqual(undefined, process_info(Pid))
      end)}
    end
  ]
}.

%% -----------------------------------------------------------------------------
%% Tests the counter named functionality.
%% -----------------------------------------------------------------------------
counter_named_test_() -> {foreach,
  fun() ->

    % Set up process.
    ws_2:counter_named()
  end,
  fun(Pid) ->

    % Teardown process.
    exit(Pid, kill)
  end,
  [
    fun(Pid) ->
      {"Check that counter name is registered", ?_test(begin

        % Stop counter, which should terminate.
        ?assertEqual(Pid, whereis(counter))
      end)}
    end
  ]
}.


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
