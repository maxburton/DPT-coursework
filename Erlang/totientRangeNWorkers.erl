-module(totientrangeNWorkers).
-export([hcf/2,
 	 relprime/2,
	 euler/1,
     startServer/0,
     workerName/1,
     registerWorker/3,
     server/7,
     totientWorker/0
	]).

%% totientrangeNWorkers.erl - Parallel Euler Totient Function (Erlang Version)
%% compile from the shell: >c(totientrangeNWorkers).
%% run from the shell:     >totientrangeNWorkers:startServer().
%%                         >server ! {range, x, y, N}.

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

workerName(Num) ->
    list_to_atom( "worker" ++ integer_to_list( Num )).

workerNames([]) ->
    0;
workerNames(Range) ->
    [Head | Rest] = Range,
    WorkerName = workerName(Head),
    server ! {names, WorkerName, Head},
    workerNames(Rest).

registerWorker(WorkerNum, Lower, Upper) ->
    register(WorkerNum, spawn(totientrangeNWorkers, totientWorker, [])),
    WorkerNum ! {range, Lower, Upper}.

%% We pass in these variables to keep their value over successive receive calls
server(Total, Count, Lower, Upper, N_workers, S, US) ->
    receive
        %% Check if all workers have finished, otherwise continue
        {finished, Sum, Pid} when Count < (N_workers-1) ->
            io:format("Server: Received Sum: ~p~n", [Sum]),
            Pid ! finished,
            server(Total+Sum, Count+1, Lower, Upper, N_workers, S, US);
        {finished, Sum, Pid} ->
            io:format("Server: Received Sum: ~p~n", [Sum]),
            io:format("Server: Sum of totients: ~p~n", [Total+Sum]),
            Pid ! finished,
            printElapsed(S,US);
        %% if this is the first worker, start from Lower
        {names, WorkerNum, Num} when Num == 1 ->
            LocalLower = Lower,
            LocalUpper = Lower + trunc(((Upper - Lower)/N_workers) * (Num)),
            registerWorker(WorkerNum, LocalLower, LocalUpper),
            server(Total, Count, Lower, Upper, N_workers, S, US);
        %% if this is the last worker, set the Upper limit
        {names, WorkerNum, Num} when Num == N_workers ->
            LocalLower = 1 + Lower + trunc(((Upper - Lower)/N_workers) * (Num - 1)),
            LocalUpper = Upper,
            registerWorker(WorkerNum, LocalLower, LocalUpper),
            server(Total, Count, Lower, Upper, N_workers, S, US);
        %% otherwise calculate an equal range
        {names, WorkerNum, Num} ->
            LocalLower = 1 + Lower + trunc(((Upper - Lower)/N_workers) * (Num - 1)),
            LocalUpper = Lower + trunc(((Upper - Lower)/N_workers) * (Num)),
            registerWorker(WorkerNum, LocalLower, LocalUpper),
            server(Total, Count, Lower, Upper, N_workers, S, US);
        {range, L, U, N} ->
            {_, Sec, USec} = os:timestamp(),
            Range = lists:seq(1, N),
            workerNames(Range),
            server(Total, Count, L, U, N, Sec, USec)
    end.

%% calculate sumTotient on a subset of the dataset
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
    register(server, spawn(totientrangeNWorkers, server, [0, 0, 0, 0, 0, 0, 0])).
