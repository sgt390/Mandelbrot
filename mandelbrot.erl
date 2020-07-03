-module(mandelbrot).
-import(mandelbrot_lib, [mandelbrot/1, split/2, complex/5]).
-export([start/2, server/2, servant/2, client/1, controller/3, timed_start/2]).


server(Creator, WaitingList) ->
    process_flag(trap_exit, true),

    receive
        {req, {Client, F, Args}} ->
            Servant = spawn_link(mandelbrot, servant,[F, Args]),
            server(Creator, [{Servant, Client, F, Args} | WaitingList]);

        {answ, {Servant, Result}} ->
            {_, Client, F, Args} = lists:keyfind(Servant, 1, WaitingList),
            Client ! {answ, {Client, F, Args}, Result},
            server(Creator, WaitingList -- [{Servant, Client, F, Args}]);
        {stop, Creator} ->
            stopped
    end.


servant(F, Args) ->
    pserver ! {answ, {self(), F(Args)}}.

client(Complex) ->
    Msg = {self(), fun(Comp)->mandelbrot_lib:mandelbrot(Comp) end, {Complex}},
    pserver ! {req, Msg},

    receive
        {answ, Msg, Result} ->
            pcontroller ! {self(), Result}
    after 6000 ->
            exit(fail)
    end.


decomposer([], _, _, WaitingList) ->
    WaitingList;

decomposer(Complex, Size, Position, WaitingList) ->
    {RestComp, Partition} = mandelbrot_lib:split(Complex, Size),
    Client = spawn_link(mandelbrot, client, [Partition]),
    decomposer(RestComp, Size, Position+1, [{Client, Position, {Partition}} | WaitingList]).


controller(Creator, Complex, Size) ->
    register(pcontroller, self()),
    WaitingList = decomposer(Complex, Size, 0, []),
    Res = composer(WaitingList, []),
    Creator ! {self(), Res}.


composer([], Results) ->  
    % flattens the lists and reverse to undo
    % the implicit reverse of mandelbrot_lib:split
    % and decomposer
    lists:reverse(lists:flatten([Y || {_, Y} <- lists:sort(Results)]));

composer(WaitingList, Results) ->
    receive
        {Client, Result} ->
            X = lists:keyfind(Client, 1, WaitingList),
            {_, Position, _} = X,
            composer(WaitingList--[X], [{Position, Result} | Results])
    end.



start(N, NJobs) ->  % output is of size NxN
    {ok, S} = file:open("results.txt", [write]),  % replace results.txt
    Complex = mandelbrot_lib:complex(-2, 1, -1, 1, N),
    Server = spawn_link(mandelbrot, server, [self(), []]),
    register(pserver, Server),
    Controller = spawn_link(mandelbrot, controller, [self(), Complex, ceil(N*N/NJobs)]),
    % split evenly between each job. The last job can have less to do.
    receive
        {Controller, Mandelbrot} ->
            io:fwrite("done:controller~n", []),
            Server ! {stop, self()},
            io:write(S, Mandelbrot),
            file:close(S)
    after 120000 ->
        exit(timeout)
    end.



timed_start(N,Jobs) ->
    {Ms, _} = timer:tc(fun(X,Y)->start(X,Y) end, [N,Jobs]),  % compute execution time
    Ms/1000.