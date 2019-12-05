-module(totientRangeNWorkers).
-export([hcf/2,
 	 relprime/2,
	 euler/1,
     startServer/0,
     workerName/1,
     registerWorker/3,
     server/5,
     totientWorker/0
	]).

%% TotientRangeNWorkers.erl - Parallel Euler Totient Function (Erlang Version)
%% compile from the shell: >c(totientRangeNWorkers).
%% run from the shell:     >totientRangeNWorkers:startServer().
%%                         >server ! {range, x, y}.

%% Max Kirker Burton 2260452b 20/10/2018

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

workerName([]) ->
    0;
workerName(Range) ->
    [Head | Rest] = Range,
    WorkerName = list_to_atom( "worker" ++ integer_to_list( Head )),
    server ! {names, WorkerName, Head},
    workerName(Rest).

registerWorker(WorkerNum, Lower, Upper) ->
    register(WorkerNum, spawn(totientRangeNWorkers, totientWorker, [])),
    WorkerNum ! {range, Lower, Upper}.

server(Total, Count, Lower, Upper, N_workers) ->
    {_, S, US} = os:timestamp(),
    receive
        {finished, Sum, Pid} when Count < (N_workers-1) ->
            io:format("Server: Received Sum: ~p~n", [Sum]),
            Pid ! finished,
            server(Total+Sum, Count+1, Lower, Upper, N_workers);
        {finished, Sum, Pid} ->
            io:format("Server: Sum of totients: ~p~n", [Total+Sum]),
            Pid ! finished,
            printElapsed(S,US);
        {names, WorkerNum, Num} when Num == N_workers ->
            LocalLower = 1 + Lower + trunc(((Upper - Lower)/N_workers) * (Num - 1)),
            LocalUpper = Upper,
            registerWorker(WorkerNum, LocalLower, LocalUpper),
            server(Total, Count, Lower, Upper, N_workers);
        {names, WorkerNum, Num} when Num == 1 ->
            LocalLower = Lower,
            LocalUpper = Lower + trunc(((Upper - Lower)/N_workers) * (Num)),
            registerWorker(WorkerNum, LocalLower, LocalUpper),
            server(Total, Count, Lower, Upper, N_workers);
        {names, WorkerNum, Num} ->
            LocalLower = 1 + Lower + trunc(((Upper - Lower)/N_workers) * (Num - 1)),
            LocalUpper = Lower + trunc(((Upper - Lower)/N_workers) * (Num)),
            registerWorker(WorkerNum, LocalLower, LocalUpper),
            server(Total, Count, Lower, Upper, N_workers);
        {range, L, U, N} ->
            Range = lists:seq(1, N),
            workerName(Range),
            %%spawn(totientRangeNWorkers, workerName, [Range]),
            server(Total, Count, L, U, N)
    end.

totientWorker() ->
    receive
        finished ->
            io:format("Worker: finished~n", []);
        {range, Lower, Upper} ->
            io:format("Worker: started~n", []),
            io:format("Worker: computing range ~p to ~p~n", [Lower, Upper]),
            Sum = lists:sum(lists:map(fun euler/1,lists:seq(Lower, Upper))),
            server ! {finished, Sum, self()},
            totientWorker()
    end.



%%start the server process
startServer() ->
    register(server, spawn(totientRangeNWorkers, server, [0, 0, 0, 0, 0])).
