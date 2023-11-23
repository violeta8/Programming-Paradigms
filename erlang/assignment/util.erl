%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%%
%%% Provides generic utility functions.
%%% ----------------------------------------------------------------------------
-module(util).
-export ([fetch/3, remove/3, store/4, reverse/1]).

%---------------------------------ASSGINMENT 3..-..PART 2 ERLANG-------------------------------
%---------------------------------VIOLETA AI NAHARRO ZALDIVAR------------------------------

%% -----------------------------------------------------------------------------
%% Fetches the first tuple whose Nth element is equal to the value of key from
%% the specified TupleList.
%% fetch(Key, N, TupleList) where:
%%   * Key::term() is the Key that identifies the tuple in TupleList.
%%   * N::integer() is the 1-based index of the tuple element that is to be
%%     compared to the value of Key.
%%   * TupleList::[tuple()] is the list of tuples to be searched.
%% Returns: The first occurrence of the tuple whose Nth element matches the
%%          value of Key, or false if no such tuple exists.
%% -----------------------------------------------------------------------------
fetch(Key, N, TupleList) when is_integer(N), N > 0 ->
  fetch2(Key, N, TupleList).

  fetch2(_, _, []) -> false;
  fetch2(Key, N, [Tuple | _]) when element(N, Tuple) == Key -> Tuple;
  fetch2(Key, N, [_ | T]) -> fetch2(Key, N, T).
%% -----------------------------------------------------------------------------
%% Removes all tuples whose Nth element is equal to the value of key from the
%% specified TupleList.
%% remove(Key, N, TupleList) where:
%%   * Key::term() is the Key that identifies the tuple in TupleList.
%%   * N::integer() is the 1-based index of the tuple element that is to be
%%     compared to the value of Key.
%%   * TupleList::[tuple()] is the list of tuples to be processed.
%% Returns: A new TupleList that does not contain tuples whose Nth matches the
%%          value of Key, or the original TupleList if no such tuples are
%%          found.
%% -----------------------------------------------------------------------------
remove(Key, N, TupleList) when is_integer(N), N > 0 ->
  remove2(Key, N, TupleList).

  remove2(_, _, []) -> [];
  remove2(Key, N, [Tuple | Rest]) when tuple_size(Tuple) >= N, element(N, Tuple) == Key ->
      remove2(Key, N, Rest);
  remove2(Key, N, [Tuple | Rest]) ->
      [Tuple | remove2(Key, N, Rest)].

%% -----------------------------------------------------------------------------
%% Replaces all the tuples whose Nth element is equal to the value of Key with
%% the new tuple New from the specified TupleList.
%% store(Key, N, TupleList, New) where:
%%   * Key::term() is the Key that identifies the tuple in TupleList.
%%   * N::integer() is the 1-based index of the tuple element that is to be
%%     compared to the value of Key.
%%   * TupleList::[tuple()] is the list of tuples to be processed.
%%   * New::tuple() is the new tuple to be inserted in place of the old tuple.
%% Returns: A new TupleList wherein all tuples whose Nth element matching the
%%          value of Key are replaced with New. If no such tuples are found,
%%          TupleList is returned with New appended to its tail.
%% -----------------------------------------------------------------------------
store(Key, N, TupleList, New) when is_integer(N), N > 0, is_tuple(New) ->
  store2(Key, N, TupleList, New, false).

  store2(_, _, [], New, false) -> [New];
  store2(_, _, [], _, true) -> [];
  store2(Key, N, [Tuple | Rest], New, _) when tuple_size(Tuple) >= N, element(N, Tuple) == Key ->
      [New | store2(Key, N, Rest, New, true)];
  store2(Key, N, [Tuple | Rest], New, Found) ->
      [Tuple | store2(Key, N, Rest, New, Found)].

%% -----------------------------------------------------------------------------
%% Reverses the specified list.
%% reverse(List) where:
%%   * List:list() is the list to be reversed.
%% Returns: A new List with the order of its elements reversed.
%% -----------------------------------------------------------------------------
reverse(List) ->
  reverse2(List, []).

  reverse2([], Acc) ->
    Acc;
  reverse2([H|T], Acc) ->
    reverse2(T, [H|Acc]).
