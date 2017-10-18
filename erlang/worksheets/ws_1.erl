%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%%
%%% Model answers for worksheet 1 questions.
%%% ----------------------------------------------------------------------------
-module(ws_1).
-export([reverse/1, fib1/1, fib2/1, tuple_to_list/1]).
-export([for/3, reduce/3, map/2, filter/2]).

%% -----------------------------------------------------------------------------
%% Reverses the specified list.
%% reverse(List) where:
%%   * List:list() is the list to be reversed.
%% Returns: A new List with the order of its elements reversed.
%% -----------------------------------------------------------------------------
reverse(List) ->
  reverse2(List, []).

reverse2(_, _) ->
  % TODO: Add implementation.
  ok.

%% -----------------------------------------------------------------------------
%% Computes fibonacci numbers using direct recursion.
%% fib1(N) where:
%%   * N::number() is the number whose fibonacci number is to be computed.
%% Returns: The fibonacci numbers for index N.
%%
%% Note that we need both clauses for fib(0) and fib(1) because in the recursive
%% step we are invoking fib/1 with both N - 1 and N - 2.
%% -----------------------------------------------------------------------------
fib1(_) ->
  % TODO: Add implementation.
  ok.

%% -----------------------------------------------------------------------------
%% Computes fibonacci numbers using tail recursion.
%% fib2(N) where:
%%   * N::number() is the number whose fibonacci number is to be computed.
%% Returns: The fibonacci number for index N.
%%
%% To make sense of this, it is best if we list the first few numbers of the
%% fibonacci sequence and their corresponding indices:
%% Number index    : [0, 1, 2, 3, 4, 5]
%% Fibonacci number: [1, 1, 2, 3, 5, 8]
%% fib(0) = 1
%% fib(1) = 1
%% fib(2) = fib(1) which we have already computed in the previous step
%%          + fib(0) which have already computed in the previous, previous step.
%%
%% So, we note that there is a 'window' of length 2 before the current number N
%% (2 in our example) [1, 1] so that their addition gives 2.
%% For fib(3), this 'window' is [1, 2], the addition of which gives 3.
%% Thus, in the general case, the window can be seen as [Nminus2, NMinus1]
%% where the additon of which (i.e. NMinus2 + Nminus1) gives fib(N).
%%
%% Further, we can see that for fib(2), the 'window' is [1, 1]; for fib(3), the
%% 'window' is [1, 2]; for fib(4), the 'window' is [2, 3], and so on (look at
%% the two lists above see the pattern). Thus, each 'window' overlaps by 1
%% place, such that the NMinus1 of the first 'window' becomes the NMinus2 of
%% the second window, and in this second window, the NMinus1 is a new value:
%% the current fibonacci number NMinus1 + Nminus2.
%%
%% As for the base cases of the fibonacci numbers, we only return the current
%% fibonacci number which is contained in NMinus1. Note that we might be
%% tempted to specify fib3(0, ..) and fib3(1, ..), but this is not correct.
%% Observe that the function fib2/1 calls fib3/3 with N, Nminus1 = 1 and
%% NMinus2 = 0. This handles the two base cases:
%%   * when N = 0, NMinus1 is immediately returned, thus a result of 1;
%%   * when N = 1, the recursive clause adds NMinus1 (which is initially set to
%%     1 in the call at fib2/1) and NMinus2 (which is initially set to 0 in the
%%     call to fib2/1), resulting in NMinus1 + NMinus2 = 1. The next recursive
%%     call then hits the base case fib3(0) and NMinus1 = 1 is returned.
%% -----------------------------------------------------------------------------
fib2(N) when is_integer(N), N >= 0 ->
  fib3(N, 0, 1).

fib3(_, _, _) ->
  % TODO: Add implementation.
  ok.

%% -----------------------------------------------------------------------------
%% Converts a tuple to list.
%% tuple_to_list(Tuple) where:
%%   * Tuple::tuple() is the tuple to be convereted to a list.
%% Returns: A list corresponding to the elements of Tuple.
%% -----------------------------------------------------------------------------
tuple_to_list(Tuple) when is_tuple(Tuple) ->
  tuple_to_list2(Tuple, tuple_size(Tuple), []).

tuple_to_list2(_, _, _) ->
  % TODO: Add implementation.
  ok.

%% -----------------------------------------------------------------------------
%% Loops and applies the specified function from I to Max.
%% for(Start, Max, F) where:
%%   * Start::integer() is the from iteration number.
%%   * Max::integer() is the maximum iteration number.
%%   * F::fun(I::integer()) is the function to apply on the current index I.
%% Returns: A list of (Max - I) elements which are the result of fun F.
%%
%% Note: the function assumes 'smaller or equal to' semantics, in which case
%% for(0, 0, ..) would still yield a list with a value 0 (i.e. [0]), *not* the
%% empty list.
%% -----------------------------------------------------------------------------
for(_, _, _) ->
  % TODO: Add implementation.
  ok.

%% -----------------------------------------------------------------------------
%% Reduces a list of elements using the specified combining function F, starting
%% with the seed value.
%% reduce(F, Seed, List) where:
%%   * F::fun(X::any(), Acc::any()) -> any() is a function that combines the
%%     current element X in the list with the accumulator value Acc.
%%   * Seed::any() is the seed that the reduction starts with.
%%   * List::list() is the list to reduce using the combining function F.
%% Returns: A single value that reflects the reduction of List.
%% -----------------------------------------------------------------------------
reduce(_, Acc, []) ->
  % TODO: Add implementation.
  ok.

%% -----------------------------------------------------------------------------
%% Maps a list of elements into another list of elements having equal length.
%% map(F, List) where:
%%   * F::fun(X::any()) -> any() is a function that maps the current element
%%     X in the list to a new value.
%%   * List::list() is the list to map.
%% Returns: A new list with F applied on each element of the old List.
%% -----------------------------------------------------------------------------
map(F, List) ->
  % TODO: Add implementation.
  ok.

%% -----------------------------------------------------------------------------
%% Filters a list of elements according to the specifed predicate F.
%% filter(F, List) where:
%%   * F::fun(X::any()) -> boolean() is a function that tests the current
%%     element X in the list and returns a boolean, indicating whether
%%     X is to be retained.
%%   * List::list() is the list to filter.
%% Returns: A new list filtered according to F, possibly containing fewer
%%          elements than the original List.
%% -----------------------------------------------------------------------------
filter(F, List) ->
  % TODO: Add implementation.
  ok.
