-module(horner_eval).
-export([horner/1]).
horner(L) ->
    % fun(B) -> A * B end.
    fun(X) -> lists:foldl(fun(C, Acc) -> X*Acc+C end,0, lists:reverse(L)) end.

