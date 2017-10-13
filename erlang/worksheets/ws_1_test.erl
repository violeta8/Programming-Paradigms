%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%%
%%% Provides a batch of unit tests for worksheet 1.
%%% ----------------------------------------------------------------------------
-module(ws_1_test).
-include_lib("eunit/include/eunit.hrl").


%%% ------------------------------------------------------------------------ %%%
%%% Unit tests.                                                              %%%
%%% ------------------------------------------------------------------------ %%%

%% -----------------------------------------------------------------------------
%% Tests the reverse list functionality.
%% -----------------------------------------------------------------------------
reverse_test_() -> [
  {"Reverse empty list", ?_test(begin

    % Reverse empty list.
    ?assertEqual([], ws_1:reverse([]))
  end)},
  {"Reverse non-empty list", ?_test(begin

    % Reverse non-empty list.
    ?assertEqual([5, 4, 3, 2, 1], ws_1:reverse([1, 2, 3, 4, 5]))
  end)}
].

%% -----------------------------------------------------------------------------
%% Tests the fibonacci numbers functionality (direct recursion).
%% -----------------------------------------------------------------------------
fib1_test_() -> [
  {"Fibonacci number of 0", ?_test(begin

    % Compute fibonacci number of 0 which should return 1.
    ?assertEqual(1, ws_1:fib1(0))
  end)},
  {"Fibonacci number of 1", ?_test(begin

    % Compute fibonacci number of 1 which should return 1.
    ?assertEqual(1, ws_1:fib1(1))
  end)},
  {"Fibonacci number of a positive number", ?_test(begin

    % Compute fibonacci number of 10 which should return 89.
    ?assertEqual(89, ws_1:fib1(10))
  end)},
  {"Fibonacci number of a negative number", ?_test(begin

    % Try to compute fibonacci number of -1 which should cause a badmatch error.
    ?assertError(function_clause, ws_1:fib1(-1))
  end)}
].

%% -----------------------------------------------------------------------------
%% Tests the fibonacci numbers functionality (tail recursion).
%% -----------------------------------------------------------------------------
fib2_test_() -> [
  {"Fibonacci number of 0", ?_test(begin

    % Compute fibonacci number of 0 which should return 1.
    ?assertEqual(1, ws_1:fib2(0))
  end)},
  {"Fibonacci number of 1", ?_test(begin

    % Compute fibonacci number of 1 which should return 1.
    ?assertEqual(1, ws_1:fib2(1))
  end)},
  {"Fibonacci number of a positive number", ?_test(begin

    % Compute fibonacci number of 10 which should return 89.
    ?assertEqual(89, ws_1:fib2(10))
  end)},
  {"Fibonacci number of a negative number", ?_test(begin

    % Try to compute fibonacci number of -1 which should cause a badmatch error.
    ?assertError(function_clause, ws_1:fib2(-1))
  end)}
].

%% -----------------------------------------------------------------------------
%% Tests both fibonacci implementations.
%% -----------------------------------------------------------------------------
fib1_fib2_test_() -> [
  {"Comparing lists of fibonacci series", ?_test(begin

    % Try to compute fibonacci number of -1 which should cause a badmatch error.
    ?assertEqual([ws_1:fib1(X) || X <- lists:seq(1, 10)],
      [ws_1:fib2(X) || X <- lists:seq(1, 10)])
  end)}
].

%% -----------------------------------------------------------------------------
%% Tests the tuple to list conversion functionality.
%% -----------------------------------------------------------------------------
tuple_to_list_test_() -> [
  {"Convert empty tuple", ?_test(begin

    % Convert empty tuple.
    ?assertEqual([], ws_1:tuple_to_list({}))
  end)},
  {"Convert non-empty homogeneous tuple", ?_test(begin

    % Convert homogeneous tuple.
    ?assertEqual([1, 2, 3, 4, 5], ws_1:tuple_to_list({1, 2, 3, 4, 5}))
  end)},
  {"Convert non-empty heterogeneous tuple", ?_test(begin

    % Convert heterogeneous tuple.
    ?assertEqual([1, 2, hello, world, "test"], ws_1:tuple_to_list({1, 2, hello, world, "test"}))
  end)}
].

%% -----------------------------------------------------------------------------
%% Tests the for loop abstraction functionality.
%% -----------------------------------------------------------------------------
for_test_() -> [
  {"Loop over empty range", ?_test(begin

    % Loop using 0.
    ?assertEqual([0], ws_1:for(0, 0, fun id/1))
  end)},
  {"Loop over positive range", ?_test(begin

    % Loop from 1 to 5.
    ?assertEqual([1, 2, 3, 4, 5], ws_1:for(1, 5, fun id/1))
  end)},
  {"Loop over negative range", ?_test(begin

    % Loop from -5 to 0.
    ?assertEqual([-5, -4, -3, -2, -1, 0], ws_1:for(-5, 0, fun id/1))
  end)},
  {"Loop over negative to positive range", ?_test(begin

    % Loop from -5 to 5.
    ?assertEqual([-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5],
      ws_1:for(-5, 5, fun id/1))
  end)},
  {"Loop over invalid range", ?_test(begin

    % Loop from -5 to -6.
    ?assertError(function_clause, ws_1:for(-5, -6, fun id/1))
  end)}
].

%% -----------------------------------------------------------------------------
%% Tests the reduce abstraction functionality.
%% -----------------------------------------------------------------------------
reduce_test_() -> [
  {"Reduce an empty list", ?_test(begin

    % Reduce an empty list: this should yield the value of the seed.
    ?assertEqual(0, ws_1:reduce(fun add/2, 0, []))
  end)},
  {"Reduce a non-empty list", ?_test(begin

    % Reduce a non-empty list which should reduce the addition of all numbers.
    ?assertEqual(10, ws_1:reduce(fun add/2, 0, [1, 2, 3, 4]))
  end)}
].

%% -----------------------------------------------------------------------------
%% Tests the map abstraction functionality.
%% -----------------------------------------------------------------------------
map_test_() -> [
  {"Map an empty list", ?_test(begin

    % Map an empty list: this should yield an empty list.
    ?assertEqual([], ws_1:map(fun double/1, []))
  end)},
  {"Reduce a non-empty list", ?_test(begin

    % Map a non-empty list which should yield a list of the same length of
    % doubled numbers.
    ?assertEqual([2, 4, 6, 8], ws_1:map(fun double/1, [1, 2, 3, 4]))
  end)}
].

%% -----------------------------------------------------------------------------
%% Tests the filter abstraction functionality.
%% -----------------------------------------------------------------------------
filter_test_() -> [
  {"Filter an empty list", ?_test(begin

    % Filter an empty list: this should yield an empty list.
    ?assertEqual([], ws_1:filter(fun is_natural/1, []))
  end)},
  {"Filter a non-empty list", ?_test(begin

    % Filter a non-empty list which should yield a list of a smaller length
    % containing only the natural numbers (see is_natural/1 predicate below).
    ?assertEqual([1, 2, 3, 0, 4],
      ws_1:filter(fun is_natural/1, [-5, 1, 2, -6, 3, 0, 4]))
  end)}
].


%%% ------------------------------------------------------------------------ %%%
%%% Helper functions.                                                        %%%
%%% ------------------------------------------------------------------------ %%%

%% -----------------------------------------------------------------------------
%% Maps the specified argument X to the same value.
%% id(X) where:
%%   * X::any() is the argument to map to itself.
%% Returns: The same argument X.
%% -----------------------------------------------------------------------------
id(X) ->
  X.

%% -----------------------------------------------------------------------------
%% Adds two numbers together.
%% add(X, Y) where:
%%   * X::number() is the first number to add.
%%   * Y::number() is the second number to add.
%% Returns: The addition of X and Y.
%% -----------------------------------------------------------------------------
add(X, Y) when is_number(X), is_number(Y) ->
  X + Y.

%% -----------------------------------------------------------------------------
%% Doubles the specified number.
%% double(X) where:
%%   * X::number() is the number to double.
%% Returns: The number X * 2.
%% -----------------------------------------------------------------------------
double(X) when is_number(X) ->
  X * 2.

%% -----------------------------------------------------------------------------
%% Determines whether the specified number is a natural number.
%% is_natural(X) where:
%%   * X::integer() is the number to test.
%% Returns: true if the number is a natural number, otherwise false is returned.
%% -----------------------------------------------------------------------------
is_natural(X) when is_integer(X) ->
  X >= 0.
