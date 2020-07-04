-module(mandelbrot_errors).
-import(mandelbrot_lib, [mandelbrot/1, split/2, complex/5]).
-export([start/2, server/2, servant/2, client/1, controller/3, timed_start/2]).


server(Creator, WaitingList) ->

    receive
        {req, {Client, F, Args}} ->
            Servant = spawn_link(mandelbrot_errors, servant,[F, Args]),
            server(Creator, [{Servant, Client, F, Args} | WaitingList]);

        {answ, {Servant, Result}} ->
            Elem = lists:keyfind(Servant, 1, WaitingList),
            case Elem of
                {_, Client, F, Args} ->
                    Client ! {answ, {Client, F, Args}, Result},
                    server(Creator, WaitingList -- [{Servant, Client, F, Args}]);
                false ->
                    server(Creator, WaitingList)
            end;
        
        {fail, Client} -> % remove the client request from the waiting list
            Elem = lists:keyfind(Client, 2, WaitingList),
            case Elem of
                {Servant, _, F, Args} ->
                    server(Creator, WaitingList -- [{Servant, Client, F, Args}]);
                false ->
                    server(Creator, WaitingList)
            end;

        {stop, Creator} ->
            stopped
    end.


servant(F, Args) ->
    pserver ! {answ, {self(), F(Args)}}.

client(Complex) -> % modified just to make it fail.
    Msg = {self(), fun(Comp)->mandelbrot_lib:mandelbrot(Comp) end, {Complex}},
    pserver ! {req, Msg},
    % client errors just after sending the request
    case rand:uniform(3) of 
        1 ->
            exit(clienterror);
        _ ->
            receive
                {answ, Msg, Result} ->
                    % client errors after receiving the result of the request 
                    case rand:uniform(3) of
                        1 ->
                            exit(clienterror);
                        _ ->
                            pcontroller ! {self(), Result}
                    end
            after 6000 ->
                    exit(clienttimeout)
            end
    end.


decomposer([], _, _, WaitingList) ->
    WaitingList;

decomposer(Complex, Size, Position, WaitingList) ->
    {RestComp, Partition} = mandelbrot_lib:split(Complex, Size),
    Client = spawn_link(mandelbrot_errors, client, [Partition]),
    decomposer(RestComp, Size, Position+1, [{Client, Position, {Partition}} | WaitingList]).


controller(Creator, Complex, Size) ->
    register(pcontroller, self()),
    process_flag(trap_exit, true),
    WaitingList = decomposer(Complex, Size, 0, []),
    Res = composer(WaitingList, []),
    Creator ! {self(), Res}.


composer([], Results) ->  
    lists:reverse(lists:flatten([Y || {_, Y} <- lists:sort(Results)]));

composer(WaitingList, Results) ->
    receive

        {Client, Result} ->
            X = lists:keyfind(Client, 1, WaitingList),
            {_, Position, _} = X,
            composer(WaitingList--[X], [{Position, Result} | Results]);

        {'EXIT', Client, clienterror} ->
            {_, Position, {Partition}} = lists:keyfind(Client, 1, WaitingList),
            io:fwrite("client failed ~w~n", [Position]),
            NewClient = spawn_link(mandelbrot_errors, client, [Partition]),
            pserver ! {fail, Client}, % notify the server that the Client failed
            composer([{NewClient, Position, {Partition}} | WaitingList--[{Client, Position, {Partition}}]], Results);
        {'EXIT', _, clienttimeout} ->
            exit(client_timeout)
        after 2000 ->
            exit(composer_timeout)
    end.



start(N, NJobs) ->  % output is of size NxN
    {ok, S} = file:open("results.txt", [write]),  % replace results.txt
    Complex = mandelbrot_lib:complex(-2, 1, -1, 1, N),
    Server = spawn_link(mandelbrot_errors, server, [self(), []]),
    register(pserver, Server),
    Controller = spawn_link(mandelbrot_errors, controller, [self(), Complex, ceil(N*N/NJobs)]),
    % split evenly between each job. The last job can have less to do.
    receive
        {Controller, Mandelbrot} ->
            Server ! {stop, self()},
            io:write(S, Mandelbrot),
            file:close(S),
            io:fwrite("done: length = ~w~n", [length(Mandelbrot)])
    after 120000 ->
        exit(timeout)
    end.



timed_start(N,Jobs) ->
    {Ms, _} = timer:tc(fun(X,Y)->start(X,Y) end, [N,Jobs]),  % compute execution time
    Ms/1000.
