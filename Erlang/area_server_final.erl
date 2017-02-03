-module(area_server_final).
-export([start/0, area/2]).
% -import(roots1, [roots/3]).
start() -> spawn(fun loop/0).
% area(ServerPid, What) ->
    % rpc(ServerPid, What).
area(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        {Pid, Response} ->
            Response
    end.

loop() ->
    receive
        {ClientPid, Coeffs} ->
            A = element(1, Coeffs),
            B = element(2, Coeffs),
            C = element(3, Coeffs),
            ClientPid ! {self(), {
                           ((-1) * B + math:sqrt((B * B) - (4 * A * C))) / (2 * A), 
                           ((-1) * B + math:sqrt((B * B) - (4 * A * C))) / (2 * A)}},
            loop()
    end.
