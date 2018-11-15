-module(test1).
-export([testFunc/0, add_two/1]).

testFunc() ->
    io:format("hello").

add_two(X) ->
    X + 2.
