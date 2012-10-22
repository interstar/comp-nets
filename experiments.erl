-module(experiments).
-export([
    getRandomType/0, getRandomAgent/0, getAlwaysLastType/0, getAlwaysMostest/0, 
    getHoodPreference/0, getDiffident/0, getInitiator/0,
    runOne/5, multiRun/6, all/1, all/0,
    unitTests/0
]).

-record(agent,{uid,last=0,linkCount={0,0}}).

%% Agent Choosing Rules
%% -----------------------------------

randomAgent({world,Size,_,_}) -> random:uniform(Size-1).
randomAgent([H],_) -> H;
randomAgent([H|T],Len) ->
    R = random:uniform(Len),
    if R == 1 -> H;
       true -> randomAgent([T],Len)
    end .
    
getRandomAgent() -> fun(W) -> randomAgent(W) end.

%% Type Choosing Rules
%% ------------------------------------

%% Random type
%% _____________________________________
randomType(_,_,_) -> random:uniform(2).
getRandomType() -> fun(A,B,W) -> randomType(A,B,W) end.


%% Prefer previous, but random in case of conflict.
%% ___________________________________________________
altp(X,Y) when X == 0, Y == 0 -> randomType(0,0,0);
altp(X,Y) when X /= 0, Y == 0 -> X;
altp(X,Y) when X == 0, Y /= 0 -> Y;
altp(X,Y) when X /= 0, Y /= 0 -> randomType(0,0,0).

alwaysLastType(U1,U2,World) -> 
    A1 = world:getAgent(U1,World), 
    A2 = world:getAgent(U2,World),
    altp(A1#agent.last,A2#agent.last).

getAlwaysLastType() -> fun(A,B,W)-> alwaysLastType(A,B,W) end.


%% Agent chooses the type already using more commonly (except random on tie)
%% ____________________________________________________________________________
preference({X,Y}) when X == Y -> {randomType(0,0,0),0};
preference({X,Y}) when X >= Y -> {1,X};
preference({_,Y}) -> {2,Y}.

agentPreference({agent,_,_,{X,Y}}) -> preference({X,Y}).

mostest( {AT,AV}, {BT,BV} ) when AV == BV, AT /= BT -> randomType(0,0,0);
mostest( {AT,AV}, {_,BV} ) when AV > BV -> AT;
mostest( {_,_}, {BT,_} ) -> BT.

alwaysMostest(U1,U2,World) ->
    A1 = world:getAgent(U1,World),
    A2 = world:getAgent(U2,World),
    %%io:fwrite("~w, ~w, ~n",[agentPreference(A1),agentPreference(A2)]),
    mostest(agentPreference(A1),agentPreference(A2)).

getAlwaysMostest() -> fun(A,B,W)->alwaysMostest(A,B,W)end .


%% Choose the most popular in your hood
%% _________________________________________________________________________
hoodOfAgent(A,World) -> world:agentsFromList(
        world:dedupe( lists:append(
            world:hood(A,1,World),
            world:hood(A,2,World) ) ) ,
            World).

hoodCounts(A, World) -> statistics:countTypesInAgentList(hoodOfAgent(A,World)).

hoodPrefers(U1,U2,World) ->
    {A1,B1} = preference(hoodCounts(U1,World)), %% type 1 hood
    {A2,B2} = preference(hoodCounts(U2,World)), %% type 2 hood
    mostest({A1,B1},{A2,B2}). %% choose the most popular
    
getHoodPreference() -> fun(A,B,W) -> hoodPrefers(A,B,W)end.    


%% Choose most popular in the hood but with diffidence.
%% _______________________________________________________________________

uncertain(M) ->
   R = random:uniform(10),
   if R < 6 -> false; 
      true -> M
   end.


diffidentHoodPrefers(U1,U2,World) ->
    {A1,B1} = preference(hoodCounts(U1,World)), %% type 1 hood
    {A2,B2} = preference(hoodCounts(U2,World)), %% type 2 hood
    M = mostest({A1,B1},{A2,B2}),
    if M < 2 -> uncertain(M); 
       true -> M 
    end.

getDiffident() -> fun(A,B,W) -> diffidentHoodPrefers(A,B,W) end.

%% Initiator wins
%% ___________________________________________________

initiatorWins(U,_,W) -> 
    {agent,_,Last,_} = world:getAgent(U,W),
    if Last == 0 -> randomType(0,0,0);
       true -> Last
    end.

getInitiator() -> fun(A,B,W) -> initiatorWins(A,B,W) end.



    

%% Actually do the experiments
%% _________________________________________

run(0,World,_,_,_,Log) -> {Log,World};
run(N,World,FChooseAgent,FChooseType,FLogger,Log) ->
    NewLog = FLogger(Log,World),
    run(N-1,world:addLinkToWorld(World,FChooseAgent,FChooseType),FChooseAgent,FChooseType,FLogger,NewLog).
    
rnd() -> 
    {A,B,C}=now(),
    random:seed(A,B,C).

runOne(NoAgents,NoLinks,FAgent,FType,FileName) ->
    io:fwrite("~s~n",[FileName]),
    rnd(),
    {Log,World} = run(NoLinks,world:newWorld(NoAgents),FAgent,FType,fun(L,W) -> statistics:logger(L,W) end,[]),
    statistics:logToFile(Log,FileName),
    fileMatrix(FileName++"Mat1.csv", statistics:matrix(World,1)),
    fileMatrix(FileName++"Mat2.csv", statistics:matrix(World,2)),

    {Log,World}.

multiRun(_,_,_,_,_,0) -> 0;
multiRun(NoAgents,NoLinks,FAgent,FType,FileNameRoot,NoProcesses) ->
    spawn(experiments,runOne,[NoAgents,NoLinks,FAgent,FType,FileNameRoot++integer_to_list(NoProcesses)++".csv" ]),
    multiRun(NoAgents,NoLinks,FAgent, FType, FileNameRoot, NoProcesses-1).
    
%% examples
%% experiments:multiRun(70,500,experiments:getRandomAgent(),experiments:getRandomType(),"Random",5).
%% experiments:multiRun(70,500,experiments:getRandomAgent(),experiments:getAlwaysLastType(),"Last",5).
%% experiments:multiRun(70,500,experiments:getRandomAgent(),experiments:getAlwaysMostest(),"Mostest",5).
%% experiments:multiRun(70,500,experiments:getRandomAgent(),experiments:getHoodPreference(),"Hood",5).
%% experiments:runOne(70,500,experiments:getRandomAgent(),experiments:getInitiator(),"Init",5).



%% fileMatrix("matRandom1.txt", statistics:matrix(W1,1)),
fileMatrix(FName,Matrix) ->
    {Msg,File} = file:open(FName,write),
    statistics:fileMatrix(File,Matrix),
    file:close(File).

all() -> all(10).

all(N) ->
    experiments:multiRun(50,1500,experiments:getRandomAgent(),experiments:getRandomType(),"Random",N),
    experiments:multiRun(50,1500,experiments:getRandomAgent(),experiments:getAlwaysLastType(),"Last",N),
    experiments:multiRun(50,1500,experiments:getRandomAgent(),experiments:getAlwaysMostest(),"Mostest",N),
    experiments:multiRun(50,1500,experiments:getRandomAgent(),experiments:getHoodPreference(),"Hood",N),

    experiments:multiRun(50,1500,experiments:getRandomAgent(),experiments:getDiffident(),"Diffident",N),
    experiments:multiRun(50,1500,experiments:getRandomAgent(),experiments:getInitiator(),"Initiator",N).


%% Unit tests
%% _________________

unitTests() ->
    A = (world:newAgent(1))#agent{linkCount={3,2}},
    eUnit:assertEquals(agentPreference(A),{1,3},1),
    B = (world:newAgent(1))#agent{linkCount={1,5}},
    eUnit:assertEquals(agentPreference(B),{2,5},2),
    eUnit:assertEquals(mostest({1,4},{2,3}),1,3),
    eUnit:assertEquals(mostest({1,4},{2,7}),2,4),    
    
    W = {world,5,[
            {agent,1,0,{2,0}},
            {agent,2,0,{1,0}},
            {agent,3,0,{1,1}},
            {agent,4,0,{0,2}},
            {agent,5,0,{0,1}},
            {agent,6,0,{0,0}} ],
        [{link,1,2,1}, {link,1,3,1}, {link,3,4,2}, {link,4,5,2}]
        },
        
        
    eUnit:assertEquals(world:len(hoodOfAgent(1,W)),2,5),
    eUnit:assertEquals(world:len(hoodOfAgent(3,W)),2,5.1),

    eUnit:assertEquals(hoodCounts(1,W),{2,1},6),
    eUnit:assertEquals(hoodPrefers(2,3,W),1,7),
    eUnit:assertEquals(hoodPrefers(5,3,W),2,7.1),
    eUnit:assertEquals(hoodPrefers(2,6,W),1,7.2),
    eUnit:assertEquals(hoodPrefers(5,1,W),1,7.3),

    
    eUnit:assertEquals(alwaysMostest(4,6,W),2,8),
    eUnit:assertEquals(alwaysMostest(1,5,W),1,9)
    
    .
    