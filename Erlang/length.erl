-module(length).
-export([horner/3, print/1]).
horner([H|T], X, Acc) -> horner(T, X*X, Acc + (X * H));
horner([], _, Acc) -> Acc.
print(X) ->
    io:format("~w~n", [X]).
