-module(totientRange2Workers).
-export([hcf/2,
 	 relprime/2,
	 euler/1,
     startServer/0,
     server/2,
     totientWorker/0
	]).

%% TotientRange2Workers.erl - Parellel Euler Totient Function (Erlang Version)
%% compile from the shell: >c(totientRange2Workers).
%% run from the shell:     >totientRange2Workers:startServer().
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

server(Total, Count) ->
    {_, S, US} = os:timestamp(),
    receive
        {finished, Sum} when Count < 1 ->
            io:format("Server: Received Sum: ~p~n", [Sum]),
            server(Total+Sum, Count+1);
        {finished, Sum} ->
            io:format("Server: Received Sum: ~p~n", [Sum]),
            io:format("Server: Sum of totients: ~p~n", [Total+Sum]),
            worker1 ! finished,
            worker2 ! finished,
            printElapsed(S,US);
        {range, Lower, Upper} ->
            register(worker1, spawn(totientRange2Workers, totientWorker, [])),
            worker1 ! {range, Lower, trunc(Upper/2)},
            register(worker2, spawn(totientRange2Workers, totientWorker, [])),
            worker2 ! {range, trunc(Upper/2 + 1), Upper},
            server(Total, Count)
    end.

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
    register(server, spawn(totientRange2Workers, server, [0, 0])).
