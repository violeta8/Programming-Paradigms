%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%%
%%% Provides a batch of unit and intergration tests that are used as part of
%%% the assignment corrections.
%%% ----------------------------------------------------------------------------
-module(util_test).
-include("log.hrl").
-include_lib("eunit/include/eunit.hrl").


%%% ------------------------------------------------------------------------ %%%
%%% Unit tests.                                                              %%%
%%% ------------------------------------------------------------------------ %%%

%% -----------------------------------------------------------------------------
%% Tests the fetch from tuple list functionality.
%% -----------------------------------------------------------------------------
fetch_test_() -> [
  {"Fetch from an empty tuple list", ?_test(begin

    % Fetch from empty list.
    ?assertEqual(false, util:fetch(a, 1, []))
  end)},
  {"Fetch when an element exists in tuple list", ?_test(begin

    % Fetch non-empty list.
    ?assertEqual({b, 2}, util:fetch(b, 1, [{a, 1}, {b, 2}]))
  end)},
  {"Fetch when an element does not exist in tuple list", ?_test(begin

    % Fetch non-empty list.
    ?assertEqual(false, util:fetch(a, 4, [{a, 1}, {b, 2}]))
  end)},
  {"Fetch when duplicate elements exist in tuple list", ?_test(begin

    % Fetch non-empty list.
    ?assertEqual({a, 1}, util:fetch(a, 1, [{a, 1}, {a, 2}]))
  end)}
].

%% -----------------------------------------------------------------------------
%% Tests the remove from tuple list functionality.
%% -----------------------------------------------------------------------------
remove_test_() -> [
  {"Remove from an empty tuple list", ?_test(begin

    % Remove from empty list.
    ?assertEqual([], util:remove(a, 1, []))
  end)},
  {"Remove when a single element exists in tuple list", ?_test(begin

    % Remove from non-empty list.
    ?assertEqual([{b, 2}], util:remove(a, 1, [{a, 1}, {b, 2}]))
  end)},
  {"Remove when an element does not exist in tuple list", ?_test(begin

    % Remove from non-empty list.
    ?assertEqual([{a, 1}, {b, 2}], util:remove(a, 4, [{a, 1}, {b, 2}]))
  end)},
  {"Remove when multiple elements exist in tuple list", ?_test(begin

    % Remove from non-empty list with duplicates.
    ?assertEqual([{b, 2}], util:remove(a, 1, [{a, 1}, {b, 2}, {a, 3}]))
  end)}
].

%% -----------------------------------------------------------------------------
%% Tests the store in tuple list functionality.
%% -----------------------------------------------------------------------------
store_test_() -> [
  {"Store into an empty tuple list", ?_test(begin

    % Store in empty list.
    ?assertEqual([{a, 1}], util:store(a, 1, [], {a, 1}))
  end)},
  {"Store into a non empty tuple list when key matches", ?_test(begin

    % Store in non-empty list.
    ?assertEqual([{z, 9}, {b, 2}], util:store(a, 1, [{a, 1}, {b, 2}], {z, 9}))
  end)},
  {"Store into a non empty tuple list when key matches multiple elements", ?_test(begin

    % Store in non-empty list.
    ?assertEqual([{z, 9}, {b, 2}, {z, 9}], util:store(a, 1, [{a, 1}, {b, 2}, {a, 2}], {z, 9}))
  end)},
  {"Store into a non empty tuple list when key does not match", ?_test(begin

    % Store in non-empty list.
    ?assertEqual([{a, 1}, {b, 2}, {z, 9}], util:store(a, 4, [{a, 1}, {b, 2}], {z, 9}))
  end)}
].

%% -----------------------------------------------------------------------------
%% Tests the reverse list functionality.
%% -----------------------------------------------------------------------------
reverse_test_() -> [
  {"Reverse empty list", ?_test(begin

    % Reverse empty list.
    ?assertEqual([], util:reverse([]))
  end)},
  {"Reverse non-empty list", ?_test(begin

    % Reverse non-empty list.
    ?assertEqual([5, 4, 3, 2, 1], util:reverse([1, 2, 3, 4, 5]))
  end)}
].
