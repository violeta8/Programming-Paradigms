%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%%
%%% Provides a batch of unit and intergration tests that are used as part of
%%% the assignment corrections.
%%% ----------------------------------------------------------------------------
-module(switch_test).
-include("log.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(mob1, "79204009").
-define(mob2, "79797979").
-define(msg1, "text1").
-define(msg2, "text2").
-define(msg3, "text3").


%%% ------------------------------------------------------------------------ %%%
%%% Unit tests.                                                              %%%
%%% ------------------------------------------------------------------------ %%%

%% -----------------------------------------------------------------------------
%% Tests the switch subscribe and unsubscribe MSISDN functionality.
%% -----------------------------------------------------------------------------
subscribe_unsubscribe_test_() -> {foreach,
  fun() ->

    % Set up switch process.
    switch:start(),
    pause_until_registered(switch)
  end,
  fun(_) ->

    % Teardown switch process.
    switch:stop()
  end,
  [
    fun(_) ->
      {"Subscribe MSISDN to switch", ?_test(begin

        % Subscribe MSISDN.
        ?assertEqual({ok, subscribed}, switch:subscribe(?mob1)),
        ?assertEqual({ok, [{?mob1, 0, []}]}, switch:status())
      end)}
    end,
    fun(_) ->
      {"Subscribe MSISDN to switch twice", ?_test(begin

        % Subscribe MSISDN.
        ?assertEqual({ok, subscribed}, switch:subscribe(?mob1)),
        ?assertEqual({ok, [{?mob1, 0, []}]}, switch:status()),

        % Resubscribe MSISDN.
        ?assertEqual({error, already_subscribed}, switch:subscribe(?mob1)),
        ?assertEqual({ok, [{?mob1, 0, []}]}, switch:status())
      end)}
    end,
    fun(_) ->
      {"Unsubscribe MSISDN from switch", ?_test(begin

        % Subscribe MSISDN.
        ?assertEqual({ok, subscribed}, switch:subscribe(?mob1)),
        ?assertEqual({ok, [{?mob1, 0, []}]}, switch:status()),

        % Unsubscribe MSISDN.
        ?assertEqual({ok, unsubscribed}, switch:unsubscribe(?mob1)),
        ?assertEqual({ok, []}, switch:status())
      end)}
    end,
    fun(_) ->
      {"Unsubscribe MSISDN from switch twice", ?_test(begin

        % Subscribe MSISDN.
        ?assertEqual({ok, subscribed}, switch:subscribe(?mob1)),
        ?assertEqual({ok, [{?mob1, 0, []}]}, switch:status()),

        % Unsubscribe MSISDN.
        ?assertEqual({ok, unsubscribed}, switch:unsubscribe(?mob1)),
        ?assertEqual({ok, []}, switch:status()),

        % Unsubscribe MSISDN.
        ?assertEqual({error, not_subscribed}, switch:unsubscribe(?mob1)),
        ?assertEqual({ok, []}, switch:status())
      end)}
    end
  ]
}.

%% -----------------------------------------------------------------------------
%% Tests the switch attach and detach functionality.
%% -----------------------------------------------------------------------------
attach_detach_test_() -> {foreach,
  fun() ->

    % Set up switch process.
    switch:start(),
    pause_until_registered(switch)
  end,
  fun(_) ->

    % Teardown switch process.
    switch:stop()
  end,
  [
    fun(_) ->
      {"Attach subscribed MSISDN with no pending messages", ?_test(begin

        % Subscribe MSISDN.
        ?assertEqual({ok, subscribed}, switch:subscribe(?mob1)),
        ?assertEqual({ok, [{?mob1, 0, []}]}, switch:status()),

        % Attach MSISDN.
        ?assertEqual({ok, attached, []}, switch:attach(?mob1)),
        ?assertEqual({ok, [{?mob1, self(), []}]}, switch:status())
      end)}
    end,
    fun(_) ->
      {"Attach subscribed MSISDN with pending messages", ?_test(begin

        % Subscribe recipient MSISDN.
        ?assertEqual({ok, subscribed}, switch:subscribe(?mob1)),
        ?assertEqual({ok, [{?mob1, 0, []}]}, switch:status()),

        % Subscribe sender MSISDN.
        ?assertEqual({ok, subscribed}, switch:subscribe(?mob2)),
        ?assertEqual({ok, [{?mob1, 0, []}, {?mob2, 0, []}]}, switch:status()),

        % Attach sender MSISDN.
        ?assertEqual({ok, attached, []}, switch:attach(?mob2)),
        ?assertEqual({ok, [{?mob1, 0, []}, {?mob2, self(), []}]}, switch:status()),

        % Send message.
        ?assertEqual({ok, msg_queued}, switch:send_msg(?mob1, ?msg1)),
        ?assertEqual({ok, [{?mob1, 0, [{?mob2, ?msg1}]}, {?mob2, self(), []}]}, switch:status()),

        % Detach sender MSISDN.
        ?assertEqual({ok, detached}, switch:detach()),
        ?assertEqual({ok, [{?mob1, 0, [{?mob2, ?msg1}]}, {?mob2, 0, []}]}, switch:status()),

        % Attach recipient MSISDN.
        ?assertEqual({ok, attached, [{?mob2, ?msg1}]}, switch:attach(?mob1)),
        ?assertEqual({ok, [{?mob1, self(), []}, {?mob2, 0, []}]}, switch:status())
      end)}
    end,
    fun(_) ->
      {"Attach unsubscribed MSISDN", ?_test(begin

        % Attach MSISDN.
        ?assertEqual({error, not_subscribed}, switch:attach(?mob1)),
        ?assertEqual({ok, []}, switch:status())
      end)}
    end,
    fun(_) ->
      {"Attach subscribed MSISDN twice", ?_test(begin

        % Subscribe MSISDN.
        ?assertEqual({ok, subscribed}, switch:subscribe(?mob1)),
        ?assertEqual({ok, [{?mob1, 0, []}]}, switch:status()),

        % Attach MSISDN.
        ?assertEqual({ok, attached, []}, switch:attach(?mob1)),
        ?assertEqual({ok, [{?mob1, self(), []}]}, switch:status()),

        % Attach MSISDN.
        ?assertEqual({error, already_attached}, switch:attach(?mob1)),
        ?assertEqual({ok, [{?mob1, self(), []}]}, switch:status())
      end)}
    end,
    fun(_) ->
      {"Attach subscribed MSISDN when PID is already taken", ?_test(begin

        % Subscribe MSISDN.
        ?assertEqual({ok, subscribed}, switch:subscribe(?mob1)),
        ?assertEqual({ok, [{?mob1, 0, []}]}, switch:status()),

        % Subscribe second MSISDN.
        ?assertEqual({ok, subscribed}, switch:subscribe(?mob2)),
        ?assertEqual({ok, [{?mob1, 0, []}, {?mob2, 0, []}]}, switch:status()),

        % Attach MSISDN.
        ?assertEqual({ok, attached, []}, switch:attach(?mob1)),
        ?assertEqual({ok, [{?mob1, self(), []}, {?mob2, 0, []}]}, switch:status()),

        % Attach MSISDN.
        ?assertEqual({error, already_attached}, switch:attach(?mob2)),
        ?assertEqual({ok, [{?mob1, self(), []}, {?mob2, 0, []}]}, switch:status())
      end)}
    end,
    fun(_) ->
      {"Detach attached PID", ?_test(begin

        % Subscribe MSISDN.
        ?assertEqual({ok, subscribed}, switch:subscribe(?mob1)),
        ?assertEqual({ok, [{?mob1, 0, []}]}, switch:status()),

        % Attach MSISDN.
        ?assertEqual({ok, attached, []}, switch:attach(?mob1)),
        ?assertEqual({ok, [{?mob1, self(), []}]}, switch:status()),

        % Detach PID.
        ?assertEqual({ok, detached}, switch:detach()),
        ?assertEqual({ok, [{?mob1, 0, []}]}, switch:status())
      end)}
    end,
    fun(_) ->
      {"Detach unattached PID", ?_test(begin

        % Subscribe MSISDN.
        ?assertEqual({ok, subscribed}, switch:subscribe(?mob1)),
        ?assertEqual({ok, [{?mob1, 0, []}]}, switch:status()),

        % Detach PID.
        ?assertEqual({error, not_attached}, switch:detach()),
        ?assertEqual({ok, [{?mob1, 0, []}]}, switch:status())
      end)}
    end
  ]
}.

%% -----------------------------------------------------------------------------
%% Tests the switch message sending functionality.
%% -----------------------------------------------------------------------------
send_msg_test_() -> {foreach,
  fun() ->

    % Set up switch process.
    switch:start(),
    pause_until_registered(switch)
  end,
  fun(_) ->

    % Teardown switch process.
    switch:stop()
  end,
  [
    fun(_) ->
      {"Send message to attached MSISDN", ?_test(begin

        % Subscribe recipient MSISDN.
        ?assertEqual({ok, subscribed}, switch:subscribe(?mob1)),
        ?assertEqual({ok, [{?mob1, 0, []}]}, switch:status()),

        % Subscribe sender MSISDN.
        ?assertEqual({ok, subscribed}, switch:subscribe(?mob2)),
        ?assertEqual({ok, [{?mob1, 0, []}, {?mob2, 0, []}]}, switch:status()),

        % Attach recipient MSISDN.
        ?assertEqual({ok, attached, []}, switch:attach(?mob1)),
        ?assertEqual({ok, [{?mob1, self(), []}, {?mob2, 0, []}]}, switch:status()),

        % Attach sender MSISDN in another process, otherwise we would get
        % an error that the PID is already associated with another MSISDN.
        process_flag(trap_exit, true),
        Pid = self(),
        CPid = spawn_link(
          fun() ->
            ?assertEqual({ok, attached, []}, switch:attach(?mob2)),
            ?assertEqual({ok, msg_sent}, switch:send_msg(?mob1, ?msg1)),
            ?assertEqual({ok, [{?mob1, Pid, []}, {?mob2, self(), []}]}, switch:status())
          end),

        % Assert that the spawned process CPid died with a reason of normal.
        ?assertEqual(normal, receive {'EXIT', CPid, Reason} -> Reason end)
      end)}
    end,
    fun(_) ->
      {"Send message to detached MSISDN", ?_test(begin

        % Subscribe recipient MSISDN.
        ?assertEqual({ok, subscribed}, switch:subscribe(?mob1)),
        ?assertEqual({ok, [{?mob1, 0, []}]}, switch:status()),

        % Subscribe sender MSISDN.
        ?assertEqual({ok, subscribed}, switch:subscribe(?mob2)),
        ?assertEqual({ok, [{?mob1, 0, []}, {?mob2, 0, []}]}, switch:status()),

        % Attach sender MSISDN.
        ?assertEqual({ok, attached, []}, switch:attach(?mob2)),
        ?assertEqual({ok, [{?mob1, 0, []}, {?mob2, self(), []}]}, switch:status()),

        % Send message.
        ?assertEqual({ok, msg_queued}, switch:send_msg(?mob1, ?msg1)),
        ?assertEqual({ok, [{?mob1, 0, [{?mob2, ?msg1}]}, {?mob2, self(), []}]}, switch:status())
      end)}
    end,
    fun(_) ->
      {"Send a number of messages to detached MSISDN", ?_test(begin

        % Subscribe recipient MSISDN.
        ?assertEqual({ok, subscribed}, switch:subscribe(?mob1)),
        ?assertEqual({ok, [{?mob1, 0, []}]}, switch:status()),

        % Subscribe sender MSISDN.
        ?assertEqual({ok, subscribed}, switch:subscribe(?mob2)),
        ?assertEqual({ok, [{?mob1, 0, []}, {?mob2, 0, []}]}, switch:status()),

        % Attach sender MSISDN.
        ?assertEqual({ok, attached, []}, switch:attach(?mob2)),
        ?assertEqual({ok, [{?mob1, 0, []}, {?mob2, self(), []}]}, switch:status()),

        % Send message 1.
        ?assertEqual({ok, msg_queued}, switch:send_msg(?mob1, ?msg1)),
        ?assertEqual({ok, [{?mob1, 0, [{?mob2, ?msg1}]}, {?mob2, self(), []}]}, switch:status()),

        % Send message 2.
        ?assertEqual({ok, msg_queued}, switch:send_msg(?mob1, ?msg2)),
        ?assertEqual({ok, [{?mob1, 0, [{?mob2, ?msg1}, {?mob2, ?msg2}]}, {?mob2, self(), []}]}, switch:status()),

        % Send message 3.
        ?assertEqual({ok, msg_queued}, switch:send_msg(?mob1, ?msg3)),
        ?assertEqual({ok, [{?mob1, 0, [{?mob2, ?msg1}, {?mob2, ?msg2}, {?mob2, ?msg3}]}, {?mob2, self(), []}]}, switch:status())
      end)}
    end,
    fun(_) ->
      {"Send message to non-existing MSISDN", ?_test(begin

        % Subscribe sender MSISDN.
        ?assertEqual({ok, subscribed}, switch:subscribe(?mob1)),
        ?assertEqual({ok, [{?mob1, 0, []}]}, switch:status()),

        % Attach sender MSISDN.
        ?assertEqual({ok, attached, []}, switch:attach(?mob1)),
        ?assertEqual({ok, [{?mob1, self(), []}]}, switch:status()),

        % Send message.
        ?assertEqual({error, to_not_subscribed}, switch:send_msg("", ?msg1)),
        ?assertEqual({ok, [{?mob1, self(), []}]}, switch:status())
      end)}
    end,
    fun(_) ->
      {"Send message when not attached", ?_test(begin

        % Subscribe sender MSISDN.
        ?assertEqual({ok, subscribed}, switch:subscribe(?mob1)),
        ?assertEqual({ok, [{?mob1, 0, []}]}, switch:status()),

        % Send message.
        ?assertEqual({error, not_attached}, switch:send_msg(?mob1, ?msg1)),
        ?assertEqual({ok, [{?mob1, 0, []}]}, switch:status())
      end)}
    end
  ]
}.


%%% ------------------------------------------------------------------------ %%%
%%% Helper functions.                                                        %%%
%%% ------------------------------------------------------------------------ %%%

%% -----------------------------------------------------------------------------
%% Blocks the caller until the specified process Name has been registered.
%% pause_until_registered(Name) where:
%%   * Name::atom() is the process name to wait for.
%% Returns: ok once the process Name is registered.
%% -----------------------------------------------------------------------------
pause_until_registered(Name) when is_atom(Name) ->
  case whereis(Name) of
    undefined ->
      pause_until_registered(Name);
    _ -> ok
  end.
