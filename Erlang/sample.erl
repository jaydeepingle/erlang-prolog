-module(sample).
-export([poly_eval/1]).
poly_eval(Coeffs) ->
    fun(X) -> lists:foldl(fun(C, Acc) -> X*Acc+C end, 0, Coeffs)
    end.
