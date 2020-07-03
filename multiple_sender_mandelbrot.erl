-module(multiple_sender_mandelbrot).
-import(mandelbrot_lib, [mandelbrot/1, split/2, complex/5]).
-export([start/2, start_error/2, server/1, servant/3]).


received(_, []) ->
    false;

received(P, [{Pos, _} | Acc]) ->
    (P == Pos) or received(P, Acc).


% discard esisting values
receiver_multiple(0, Acc) ->  % returns the ordered output
    lists:flatten([Y || {_, Y} <- lists:sort(Acc)]);

receiver_multiple(NJobs, Acc) ->
    receive
        {R, Pos} ->
            Existing = received(Pos, Acc),
            if  Existing ->
                io:fwrite("Discarded: ~w~n", [Pos]),
                receiver_multiple(NJobs, Acc);
            true ->
                io:fwrite("Received: ~w~n", [Pos]),
                receiver_multiple(NJobs-1, [{Pos, R} | Acc])
            end
    end.


% vanilla receiver
receiver(0, Acc) ->  % returns the ordered output
    lists:flatten([Y || {_, Y} <- lists:sort(Acc)]);

receiver(NJobs, Acc) ->
    receive
        {R, Pos} ->
            io:fwrite("Received: ~w~n", [Pos]),
            receiver(NJobs-1, [{Pos, R} | Acc])

    end.


receiver(NJobs) ->
    receiver(NJobs, []).


receiver_multiple(NJobs) ->
    receiver_multiple(NJobs, []).


% Create and send the requests to the server
sender([], _, _, _) ->
    done;

sender(Complex, Size, Server, Counter) ->
    % builds a subset of the complex numbers and sends it to the server
    {NewComplex, C} = split(Complex, Size),
    Server ! {C, Counter},
    Server ! {C, Counter},
    sender(NewComplex, Size, Server, Counter+1).


server(Pid) ->
    receive
        {Complex, Order} ->
            spawn(multiple_sender_mandelbrot, servant, [Complex, Pid, Order]),
            server(Pid)
    end.


servant(Complex, Pid, Order) ->
    timer:sleep(round(rand:uniform()*10)),
    io:fwrite("sending ~w~n", [Order]),
    Pid ! {mandelbrot(Complex), Order}.


start(N, NJobs) ->  % output is of size NxN
    {ok, S} = file:open("results.txt", [write]),  % replace results.txt
    Server = spawn_link(multiple_sender_mandelbrot, server, [self()]),
    process_flag(trap_exit, true),
    Complex = complex(-2, 1, -1, 1, N),
    % split evenly between each job. The last job can have less to do.
    sender(Complex, ceil(N*N/NJobs), Server, 0),
    Mandelbrot = receiver_multiple(NJobs),
    io:write(S, Mandelbrot),
    file:close(S).


% multiple equal messages and vanilla receiver
start_error(N, NJobs) ->  % output is of size NxN
    {ok, S} = file:open("results.txt", [write]),  % replace results.txt
    Server = spawn(multiple_sender_mandelbrot, server, [self()]),
    Complex = complex(-2, 1, -1, 1, N),
    % split evenly between each job. The last job can have less to do.
    sender(Complex, ceil(N*N/NJobs), Server, 0),
    Mandelbrot = receiver(NJobs),
    io:write(S, Mandelbrot),
    file:close(S).


% for testing: (ctrl+G resets the BM, to discard useless messages) <<< TODO