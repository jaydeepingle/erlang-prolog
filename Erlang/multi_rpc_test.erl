-module(multi_rpc_test).
-compile([main/0, loop/1, for/3, for/3]).

main() ->
    L = for(1, 10, fun() -> spawn( fun() -> loop(100) end) end ),
    io:format("~p~n", [L]),
    noop.

loop(0) ->
    ok;
loop(X) ->  
    io:format("~p processing ~p\n", [self(), X]),
    loop(X-1).

for( N, N, F ) -> 
    [F()];
for( I, N, F ) ->
    io:format("process created: ~w~n", [I] ),
    [F() | for(I+1, N, F)].
