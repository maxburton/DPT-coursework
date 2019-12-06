-module(totientrangeNWorkersReliable).
-export([hcf/2,
 	 relprime/2,
	 euler/1,
     startServer/0,
     workerNames/1,
     workerName/1,
     registerWorker/3,
     server/7,
     totientWorker/0,
     watcher/3,
     workerChaos/2,
     testRobust/2
	]).

%% totientrangeNWorkersReliable.erl - Parallel & Reliable Euler Totient Function (Erlang Version)
%% compile from the shell: >c(totientrangeNWorkersReliable).
%% run from the shell:     >totientrangeNWorkersReliable:startServer().
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

%% Generate a worker name with a given number
workerName(Num) ->
    list_to_atom( "worker" ++ integer_to_list( Num )).

%% Recursively generate worker names from a sequential list
workerNames([]) ->
    0;
workerNames(Range) ->
    [Head | Rest] = Range,
    WorkerName = workerName(Head),
    server ! {names, WorkerName, Head},
    workerNames(Rest).

%% register a worker, then pass a message to it
registerWorker(WorkerNum, Lower, Upper) ->
    register(WorkerNum, spawn_link(totientrangeNWorkersReliable, totientWorker, [])),
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
            spawn(totientrangeNWorkersReliable, watcher, [WorkerNum, LocalLower, LocalUpper]),
            server(Total, Count, Lower, Upper, N_workers, S, US);
        %% if this is the last worker, set the Upper limit
        {names, WorkerNum, Num} when Num == N_workers ->
            LocalLower = 1 + Lower + trunc(((Upper - Lower)/N_workers) * (Num - 1)),
            LocalUpper = Upper,
            spawn(totientrangeNWorkersReliable, watcher, [WorkerNum, LocalLower, LocalUpper]),
            server(Total, Count, Lower, Upper, N_workers, S, US);
        %% otherwise calculate an equal range
        {names, WorkerNum, Num} ->
            LocalLower = 1 + Lower + trunc(((Upper - Lower)/N_workers) * (Num - 1)),
            LocalUpper = Lower + trunc(((Upper - Lower)/N_workers) * (Num)),
            spawn(totientrangeNWorkersReliable, watcher, [WorkerNum, LocalLower, LocalUpper]),
            server(Total, Count, Lower, Upper, N_workers, S, US);
        {range, L, U, N} ->
            {_, Sec, USec} = os:timestamp(),
            Range = lists:seq(1, N),
            workerNames(Range),
            server(Total, Count, L, U, N, Sec, USec)
    end.

%% If an exit message is received and the worker had not completed, trap it and respawn the linked worker
watcher(WorkerNum, LocalLower, LocalUpper) ->
    process_flag(trap_exit, true),
    registerWorker(WorkerNum, LocalLower, LocalUpper),
    io:format("Watcher: Watching Worker ~p~n", [WorkerNum]),
    receive
        Msg -> 
            {_Message, _Pid, Reason} = Msg,
            if 
                Reason == chaos ->
                    watcher(WorkerNum, LocalLower, LocalUpper);
                true ->
                    0
            end
    end.

%% calculate sumTotient on a subset of the dataset
totientWorker() ->
    receive
        finished ->
            0;
        {range, Lower, Upper} ->
            io:format("Worker: computing range ~p to ~p~n", [Lower, Upper]),
            Sum = lists:sum(lists:map(fun euler/1,lists:seq(Lower, Upper))),
            server ! {finished, Sum, self()},
            totientWorker()
    end.

workerChaos(NVictims, NWorkers) ->
    lists:map(
        fun( _ ) ->
            timer:sleep(500),   %% Sleep for .5s
                                %% Choose a random victim
            WorkerNum = rand:uniform(NWorkers),
            io:format("workerChaos killing ~p~n",
                      [workerName(WorkerNum)]),
            WorkerPid = whereis(workerName(WorkerNum)),
            if %% Check if victim is alive
                WorkerPid == undefined ->
                    io:format("workerChaos already dead: ~p~n",
                              [workerName(WorkerNum)]);
                true -> %% Kill Kill Kill
                    exit(whereis(workerName(WorkerNum)),chaos)
                end
            end,
            lists:seq( 1, NVictims ) ).

testRobust(NWorkers, NVictims) ->
    ServerPid = whereis(server),
    if %% Is server already running?
        ServerPid == undefined ->
            startServer();
        true ->
            ok
    end,
    server ! {range, 1, 15000, NWorkers},
    workerChaos(NVictims,NWorkers).

%%start the server process
startServer() ->
    register(server, spawn(totientrangeNWorkersReliable, server, [0, 0, 0, 0, 0, 0, 0])).
