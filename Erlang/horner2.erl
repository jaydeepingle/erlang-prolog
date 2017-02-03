-module(horner2).
-export([server/1, client/2]).

%Given Coeffs list [C[N], C[N-1], ..., C[1], C[0]], set up a server
%to evaluate the polynomial
%
%   C[N]*X^(N) + C[N-1]]*X^(N-1) + ... + C[1]*X^(1) + C[0]
%
%at some (yet unspecified) X.
%
%The server must consist of multiple Erlang processes with one Erlang 
%process per polynomial coefficient (there may be additional processes too).
%The processes for the coefficients should store their respective
%coefficients and communicate among themselves to evaluate the
%polynomial at a specified X using Horner's rule.
%
%The return value of this function should be the PID of the server.
%
%The details of the messages exchanged between the processes for
%evaluating the polynomial can be chosen by the implementation.
%However, the server must respond to a 'stop' message by stopping
%all the processes as well as itself.
%
%When a process for a coefficient is assisting in evaluating the
%polynomial, it must log to standard error it's stored coeffient
%and the value accumulated so far in the Horner evaluation.
%When a process for a coefficient is being stopped, it must
%log to standard error it's stored coefficient and the 'stop'
%message. 
%Return the value at X of the polynomial stored within the polynomial server
%specified by PID.
-export([start/1, calc_stuff/1]).
-import(lists, [map/2]).

%This function takes coefficients and spawns calc_stuff multiple times which is
%equal to the number of coefficients
server(Coeffs) -> 
    Pids = map(fun(X) -> spawn(horner2, calc_stuff, [X]) end, Coeffs), 
    spawn(horner2, start, [Pids]).

%This function starts the evaluation of polynomial
start(Pids) ->
    receive
        {X, ClientPID} ->
            Self = lists:nth(1, Pids),
            Self ! {Pids, X, 0, ClientPID}, 
            start(Pids);
        stop ->
            map(fun(X) -> X ! stop end, Pids), 
            true
    end.

%This function gets spawned multiple times depends on the number of coefficients
calc_stuff(C) ->
    receive
        {[_], X, Acc, ClientPID} ->
            Final = C + (Acc * X),
            do_log(C, Final),
            ClientPID ! Final, 
            calc_stuff(C);
        {[_, B|Tail], X, Acc, ClientPID} ->
            Next = C + (Acc * X),
            do_log(C, Next),
            B ! {[B]++Tail, X, Next, ClientPID}, 
            calc_stuff(C);
        stop ->
            do_log(C, stop), 
            true
    end.

%client which takes Pid and X and calls the start function using pattern
%matching
client(Pid, X) ->
    Pid ! {X, self()}, 
    receive
       Final ->
            Final
    end.

%function to log the values
do_log(Coeff, Msg) ->
    io:format(standard_error, "coeff = ~w; ~w~n", [Coeff, Msg]).
