-module(mandelbrot_lib).
-export([mandelbrot/1, complex/5, split/2]).


% a complex number is a tuple: {A, B} := A + iB
% square and sum complex numbers
square({A, B}) ->
    {A*A - B*B, 2*A*B}.

sum({A, B}, {C, D}) ->
    {A + C, B + D}.

f(Z, C) ->
    sum(square(Z), C).


min2({A, _}) ->
    abs(A) < 2.


isMandelbrot(_, _, 0, _) ->  % in mandelbrot (given 50)
    0;

isMandelbrot(_, _, Iter, false) ->  % not in Mandelbrot
    Iter;

isMandelbrot(C, Z, Iter, true) ->
    Znew = f(Z, C),
    M = min2(Z),
    isMandelbrot(C, Znew, Iter-1, M).


isMandelbrot(C) ->
    isMandelbrot(C, {0, 0}, 50, true).  % max of 50 recursive calls

reals(_, _, 0, Acc) ->
    lists:reverse(Acc);

reals(Inf, Delta, Size, Acc) ->
    reals(Inf+Delta, Delta, Size-1, [Inf | Acc]).

reals(Inf, Sup, Size) ->
    Delta = (Sup - Inf) / Size,
    reals(Inf, Delta, Size, []).


repeat(_, 0, Acc) ->
    Acc;

repeat(X, N, Acc) ->
    repeat(X, N-1, [X|Acc]).

repeat(X, N) ->
    repeat(X, N, []).

superZip([], _, _, Acc) ->
    Acc;

superZip([X|L1], L2, Len, Acc) ->
    superZip(L1, L2, Len, lists:zip(repeat(X, Len), L2)++Acc).

superZip(L1, L2) ->
    superZip(L1, L2, length(L2), []).

complex(InfR, SupR, InfI, SupI, Size) ->
    superZip(reals(InfR, SupR, Size), reals(InfI, SupI, Size)).

mandelbrot({Complex}) ->
    lists:flatten(lists:foldl(fun(X, Y) -> [isMandelbrot(X)|Y] end, [], Complex)).  % lists:zip(Comp, mapfun(Comp)).


% split the list L into L1, L2;
% L2 contains the first min(N, len(L)) elements of L 
% L1 contains the rest
split(L1, L2, 0) ->
    {L1, L2};

split([], L2, _) ->
    {[], L2};

split([El|L1], L2, N) ->
    split(L1, [El | L2], N-1).

split(L, N) ->
    split(L, [], N).
