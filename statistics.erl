-module(statistics).
-export([
    countTypesInAgentList/1, countTypesInWorld/1, prop/1,
    allClusters/2, largestCluster/2,
    newLog/3, logger/2, zipLogs/4, logToFile/2,
    inspect/1, summary/1, matrix/4, fileMatrix/2, matrix/2,
    unitTests/0
]).

-record(world,{size,population,links}).

%% Statistics
%% _____________________________________________________________

countTypesInWorld({world,_,Population,_}) -> countTypesInAgentList(Population). 

countTypesInAgentList(Agents) -> %% note each *link* get's counted twice by this algorithm,
                                 %% it counts connections/agent of types 
    lists:foldl(fun({agent,_,_,{A,B}},{X,Y})-> {X+A,B+Y} end, {0,0}, Agents).


%% Calculate the proportion of the total links is held by the majority type    
prop({_,X}) when X < 1 -> 0;
prop({X,_}) when X < 1 -> 0;
prop({C1,C2}) -> (C1/(C1+C2)).

%% find the size of the largest cluster (slow method)
allClusters(Type,{world,_,Pop,_}=World) -> [world:getCluster(U,Type,World) || {agent,U,_,_} <- Pop].

len([]) -> 0;
len([_|T])-> 1+len(T).

largestCluster(Type,World) -> lists:max( [len(C) || C <- allClusters(Type,World)] ).
    
sortRatio({X,Y}) when X >= Y -> {X,Y}; 
sortRatio({X,Y}) when Y > X -> {Y,X}. %% reverse order if Y > X

%% Logging
%%

-record(log,{prop,max1,max2}).

newLog(Prop,Max1,Max2) -> #log{prop=Prop,max1=Max1,max2=Max2}.

logger(Log,World) ->
    P = prop(sortRatio(countTypesInWorld(World))),
    Max1 = 0, %%largestCluster(1,World),
    Max2 = 0, %% largestCluster(2,World),
    Log++[newLog(P,Max1,Max2)].

%% Expected format of logs is [| LogList]
%% we want [[prop,prop,prop,prop,max1,max2,max1,max2,max1,max2,max1,max2]|LogList]

%% [{log,prop,max1,max2}|ExperimentList] -> [[prop|proplist],[max1,max2|maxlist]]

unzipExperimentLogList({log,E1P,E1M1,E1M2}, {log,E2P,E2M1,E2M2},
                         {log,E3P,E3M1,E3M2}, {log,E4P,E4M1,E4M2} ) ->
        [E1P,E2P,E3P,E4P,E1M1,E1M2,E2M1,E2M2,E3M1,E3M2,E4M1,E4M2 ].

zipLogs([],[],[],[]) -> [];
zipLogs([H1|T1],[H2|T2],[H3|T3],[H4|T4]) ->
        [unzipExperimentLogList(H1,H2,H3,H4)|zipLogs(T1,T2,T3,T4)].

rFileLog([],_) -> true;
rFileLog([{log,P,M1,M2}|T],File) ->
    io:fwrite(File,"~w, ~w, ~w, ~n ",[P,M1,M2]),
    rFileLog(T,File).

logToFile(Logs,FName) ->
    {_,File} = file:open(FName,write),
    rFileLog(Logs,File),
    file:close(File).


%% Inspection

inspect({agent,Uid,Last,{T1C,T2C}}) -> %% Inspect an agent
   io:fwrite("A(~w) Last:~w Totals:(~w,~w)~n",[Uid,Last,T1C,T2C]);
   
inspect({X,Y}) when X >= Y -> io:fwrite("~w:~w ",[X,Y]); %% Inspect a ratio
inspect({X,Y}) when Y > X -> io:fwrite("~w:~w ",[Y,X]); %% Inspect a ratio

inspect({world,_,Population,_}=W) ->  %% Inspect a World
    Prnt = fun(X) -> io:fwrite("~w, ",[X]) end,
    io:fwrite("Ratio ~w~n",[countTypesInWorld(W)]),
    io:fwrite("~nAgents : ~n"),
    lists:foreach(fun(A)->inspect(A) end, Population),
    io:fwrite("~nLinks : ~n"),
    lists:foreach(Prnt, W#world.links)
    .
    
summary(W) -> io:fwrite("Ratio ~w~n",[countTypesInWorld(W)]).


%% Association Matrix 
%% _________________________

exists(R,C,T,Links) -> world:hasLink({link,R,C,T},Links).

makeRow(_,_,_,0) -> [];
makeRow(R,T,Links,C) ->
    E = exists(R,C,T,Links),
    if E -> [1 | makeRow(R,T,Links,C-1)];
       true -> [0 | makeRow(R,T,Links,C-1)]
    end .

matrix(_,_,_,0) -> [];
matrix(Size,T,Links,R) ->
    [makeRow(R,T,Links,Size)| matrix(Size,T,Links,R-1)].
    
matrix({world,Size,_,Links},Type) ->  matrix(Size,Type,Links,Size).

fileMatrix(_,[]) -> true;
fileMatrix(File,[H|T]) ->
    W = fun(X) -> io:fwrite(File,"~w	",[X]) end,
    lists:foreach(W,H),
    io:fwrite(File,"~n",[]),
    fileMatrix(File,T).
    
%% Unit tests
%% _________________
unitTests() ->
    Al= [
        {agent,1,0,{3,6}},
        {agent,2,0,{2,1}},
        {agent,3,0,{5,2}},
        {agent,4,0,{3,3}},
        {agent,5,0,{1,9}}
        
    ],
    eUnit:assertEquals(countTypesInAgentList(Al),{14,21},1),
    
    true.