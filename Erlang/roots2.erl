-module(roots2).
-export([server/0, client/2]).
-import(roots1, [roots/3]).

%Return the PID of a quadratic-solver Erlang process which when given
%a message containing the coefficients A, B, C responds with a message
%giving the roots of the quadratic equation A*X^2 + B*X + C.  It is
%assumed that the return'd roots are not complex.

%This spawns the loop function
server() ->
    spawn(fun loop/0).

%Given the PID of a quadratic-solver and Coeffs a 3-tuple {A, B, C}, 
%this function uses that quadratic-solver to return the roots of the
%quadratic equation A*X^2 + B*X + C.  The roots are return'd as a
%2-tuple, with the first element of the return'd tuple using the
%positive square-root of the discriminant and the second element of
%the return'd tuple using the negative square-root of the
%discriminant.  It is assumed that the return'd roots are not complex.

%This function takes 2 arguments Pid of loop function and Coeffs
client(Pid, Coeffs) ->
    Pid ! {self(), Coeffs},
    receive
        {Pid, Roots} ->
            Roots;
        stop ->
            true
    end.

%Function which returns the roots by calling the roots1 module function 
loop() ->
    receive
        {ClientPid, Coeffs} ->
            A = element(1, Coeffs),
            B = element(2, Coeffs),
            C = element(3, Coeffs),
            Roots = roots(A, B, C),
            ClientPid ! {self(), Roots},
            loop();
        stop ->
            true
    end.

