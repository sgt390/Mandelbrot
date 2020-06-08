-module(mandelbrot).
-export([start/2, mandelbrot/1, server/1, servant/3]).


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


isMandelbrot(_, _, 0, _) ->
    0;

isMandelbrot(_, _, Iter, false) ->
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

mandelbrot(Complex) ->
    lists:flatten(lists:foldl(fun(X, Y) -> [isMandelbrot(X)|Y] end, [], Complex)).  % lists:zip(Comp, mapfun(Comp)).

split([], L2, _) ->
    {[], lists:reverse(L2)};

split(L1, L2, 0) ->
    {L1, lists:reverse(L2)};

split([El|L1], L2, N) ->
    split(L1, [El | L2], N-1).

split(L, N) ->
    split(L, [], N).

sender([], _, _, _) ->
    done;

sender(Complex, Size, Server, Counter) ->
    {NewComplex, C} = split(Complex, Size),
    Server ! {C, Counter},
    sender(NewComplex, Size, Server, Counter+1).

receiver(0, Acc) -> 
    lists:flatten([lists:reverse(Y) || {_, Y} <- lists:sort(Acc)]);

receiver(NJobs, Acc) ->
    receive
        {R, Pos} ->
            receiver(NJobs-1, [{Pos, R} | Acc])
    end.

receiver(NJobs) ->
    receiver(NJobs, []).

start(N, NJobs) ->
    {ok, S} = file:open("results.txt", [write]),  % delete results.txt content
    Server = spawn(mandelbrot, server, [self()]),
    Complex = complex(-2, 1, -1, 1, N),
    sender(Complex, round(N*N/NJobs), Server, 0),
    Mandelbrot = receiver(NJobs),
    io:write(S, Mandelbrot),
    file:close(S).


server(Pid) ->
    receive
        {Complex, Order} ->
            spawn(mandelbrot, servant, [Complex, Pid, Order]), % mandelbrot(N1, Range1)
            server(Pid);
        stop ->
            Pid ! stop
    end.


servant(Complex, Pid, Order) ->
    Pid ! {mandelbrot(Complex), Order}.
