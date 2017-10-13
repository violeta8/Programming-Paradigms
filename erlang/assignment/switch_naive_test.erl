%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%%
%%% Provides a batch of unit and intergration tests that are used as part of
%%% the assignment corrections.
%%% ----------------------------------------------------------------------------
-module(switch_naive_test).
-include("log.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(mob1, "79204009").
-define(mob2, "79797979").


%%% ------------------------------------------------------------------------ %%%
%%% Unit tests.                                                              %%%
%%% ------------------------------------------------------------------------ %%%

%% -----------------------------------------------------------------------------
%% Tests the switch_naive subscribe and unsubscribe MSISDN functionality.
%% -----------------------------------------------------------------------------
subscribe_unsubscribe_test_() -> {foreach,
  fun() ->

    % Set up switch process.
    switch_naive:start(),
    pause_until_registered(switch_naive)
  end,
  fun(_) ->

    % Teardown switch process.
    switch_naive:stop()
  end,
  [
    fun(_) ->
      {"Subscribe MSISDN to switch", ?_test(begin

        % ?INFO("Inside test"),
        % Subscribe MSISDN.
        ?assertEqual({ok, subscribed}, switch_naive:subscribe(?mob1)),
        ?assertEqual({ok, [{?mob1, 0, []}]}, switch_naive:status())
      end)}
    end,
    fun(_) ->
      {"Subscribe MSISDN to switch twice", ?_test(begin

        % Subscribe MSISDN.
        ?assertEqual({ok, subscribed}, switch_naive:subscribe(?mob1)),
        ?assertEqual({ok, [{?mob1, 0, []}]}, switch_naive:status()),

        % Resubscribe MSISDN.
        ?assertEqual({error, already_subscribed}, switch_naive:subscribe(?mob1)),
        ?assertEqual({ok, [{?mob1, 0, []}]}, switch_naive:status())
      end)}
    end,
    fun(_) ->
      {"Unsubscribe MSISDN from switch", ?_test(begin

        % Subscribe MSISDN.
        ?assertEqual({ok, subscribed}, switch_naive:subscribe(?mob1)),
        ?assertEqual({ok, [{?mob1, 0, []}]}, switch_naive:status()),

        % Unsubscribe MSISDN.
        ?assertEqual({ok, unsubscribed}, switch_naive:unsubscribe(?mob1)),
        ?assertEqual({ok, []}, switch_naive:status())
      end)}
    end,
    fun(_) ->
      {"Unsubscribe MSISDN from switch twice", ?_test(begin

        % Subscribe MSISDN.
        ?assertEqual({ok, subscribed}, switch_naive:subscribe(?mob1)),
        ?assertEqual({ok, [{?mob1, 0, []}]}, switch_naive:status()),

        % Unsubscribe MSISDN.
        ?assertEqual({ok, unsubscribed}, switch_naive:unsubscribe(?mob1)),
        ?assertEqual({ok, []}, switch_naive:status()),

        % Unsubscribe MSISDN.
        ?assertEqual({error, not_subscribed}, switch_naive:unsubscribe(?mob1)),
        ?assertEqual({ok, []}, switch_naive:status())
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
