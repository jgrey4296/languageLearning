-module(test1).
-export([testFunc/0, add_two/1]).
-compile(export_all).

testFunc() ->
    io:format("hello").

add_two(X) ->
    X + 2.

add_three(X) ->
    X + 3.
