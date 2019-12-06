-module(totientrange2Workers).
-export([hcf/2,
 	 relprime/2,
	 euler/1,
     startServer/0,
     server/4,
     totientWorker/0
	]).

%% totientrange2Workers.erl - Parellel Euler Totient Function (Erlang Version)
%% compile from the shell: >c(totientrange2Workers).
%% run from the shell:     >totientrange2Workers:startServer().
%%                         >server ! {range, x, y}.

%% Max Kirker Burton 2260452b

%% This program calculates the sum of the totients between a lower and an 
%% upper limit. It is based on earlier work by: Nathan Charles, 
%% Hans-Wolfgang Loidl and Colin Runciman

%% The comments provide (executable) Haskell specifications of the functions

%% hcf x 0 = x
%% hcf x y = hcf y (rem x y)

hcf(X,0) -> X;
hcf(X,Y) -> hcf(Y,X rem Y).

%% relprime x y = hcf x y == 1

relprime(X,Y) -> 
  V = hcf(X,Y),
  if 
    V == 1 
      -> true;
    true 
      -> false
  end.

%%euler n = length (filter (relprime n) (mkList n))

euler(N) -> 
  RelprimeN = fun(Y) -> relprime(N,Y) end,  
  length (lists:filter(RelprimeN,(lists:seq(1,N)))).

%% Take completion timestamp, and print elapsed time

printElapsed(S,US) ->
  {_, S2, US2} = os:timestamp(),
                       %% Adjust Seconds if completion Microsecs > start Microsecs
  if
    US2-US < 0 ->
      S3 = S2-1,
      US3 = US2+1000000;
    true ->
      S3 = S2,
      US3 = US2
  end,
  io:format("Time taken in Secs, MicroSecs ~p ~p~n",[S3-S,US3-US]).

%% We pass in these variables to keep their value over successive receive calls
server(Total, Count, S, US) ->
    receive
        %% Check if all workers have finished, otherwise continue
        {finished, Sum} when Count < 1 ->
            io:format("Server: Received Sum: ~p~n", [Sum]),
            server(Total+Sum, Count+1, S, US);
        {finished, Sum} ->
            io:format("Server: Received Sum: ~p~n", [Sum]),
            io:format("Server: Sum of totients: ~p~n", [Total+Sum]),
            worker1 ! finished,
            worker2 ! finished,
            printElapsed(S,US);
        {range, Lower, Upper} ->
            {_, Sec, USec} = os:timestamp(),
            register(worker1, spawn(totientrange2Workers, totientWorker, [])),
            worker1 ! {range, Lower, trunc(Upper/2)},
            register(worker2, spawn(totientrange2Workers, totientWorker, [])),
            worker2 ! {range, trunc(Upper/2 + 1), Upper},
            server(Total, Count, Sec, USec)
    end.

%% calculate sumTotient on a subset of the dataset
totientWorker() ->
    receive
        finished ->
            io:format("Worker: finished~n", []);
        {range, Lower, Upper} ->
            io:format("Worker: computing range ~p to ~p~n", [Lower, Upper]),
            Sum = lists:sum(lists:map(fun euler/1,lists:seq(Lower, Upper))),
            server ! {finished, Sum},
            totientWorker()
    end.



%%start the server process
startServer() ->
    register(server, spawn(totientrange2Workers, server, [0, 0, 0, 0])).
