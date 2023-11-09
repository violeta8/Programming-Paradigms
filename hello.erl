-module(hello).
-export([say_hello/0, say_hello/1]).
say_hello() -> …
say_hello(Name) ->
io:format(”Hello there, ~s~n”, [Name]),
0.
