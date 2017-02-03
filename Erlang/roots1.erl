-module(roots1).
-export([roots/3]).

%Return a 2-element tuple containing the roots of the quadratic
%equation A*X^2 + B*X + C.  You may assume that the discriminant >= 0.
%The first element of the return'd tuple should use the positive
%square-root of the discriminant and the second element of the
%return'd tuple should use the negative square-root of the discriminant.

%It takes arguments A, B and C and returns the tuple of roots
roots(A, B, C) ->
    X1 = ((-1) * B + math:sqrt((B * B) - (4 * A * C))) / (2 * A),
    X2 = ((-1) * B - math:sqrt((B * B) - (4 * A * C))) / (2 * A),
    {X1, X2}.  
