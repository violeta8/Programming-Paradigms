-module(hello).
-export([say_hello/0, say_hello/1, int/2]).
say_hello() ->
io:format("Hello world~n").

say_hello(Name) ->
io:format("Hello there, ~s~n", [Name]),
0.

int(Max, Max) ->
Max;
int(I, Max) when is_number(I), is_number(Max), I >= 0, Max >= I ->
I + int(I + 1, Max).
