-module(world).
-export([
    newAgent/1, equals/2, 
    newLink/3, involves/2, hasLink/2, addLink/2,
    newPopulation/1,  newWorld/1, getAgent/2, updateAgentWithLink/2, 
    linksTo/3, agentsFromList/2, getPopFromWorld/1, dedupe/1, find/2,
    addLinkToWorld/3, hood/3, getCluster/3, getClusterSize/3, len/1,
    test/0, randomWorld/2,
    unitTests/0
]).

-record(agent,{uid,last=0,linkCount={0,0}}).
-record(link,{uid1,uid2,type}).
-record(world,{size,population,links}).


%% Agents
%% ____________________________________________________________
newAgent(U) -> #agent{uid=U,last=0,linkCount={0,0}}.

%% Links
%% _____________________________________________________________
newLink(Uid1,Uid2,T) -> #link{uid1=Uid1,uid2=Uid2,type=T}. 


incLinkCount({X,Y},1) -> {X+1,Y};
incLinkCount({X,Y},2) -> {X,Y+1}.

updateAgentWithLink({agent,_,_,LinkCount}=Agent,{link,_,_,Type}=Link) ->
    Involved = involves(Link,Agent),
    if Involved -> 
	Agent#agent{last=Type, linkCount=incLinkCount(LinkCount,Type)};
        true -> Agent
    end.


%% Equality
equals({agent,U1,L1,{A1,B1}},{agent,U2,L2,{A2,B2}})
    when U1==U2,L1==L2,A1==A2,B1==B2 -> true;
equals({agent,_,_,{_,_}},{agent,_,_,{_,_}}) -> false;

equals({link,A1,B1,T1},{link,A2,B2,T2})
    when A1==A2,B1==B2,T1==T2 -> true;
equals({link,_,_,_},{link,_,_,_}) -> false.

equals2({link,A1,B1,_},{link,A2,B2,_}) %% equality ignoring types
    when A1==A2,B1==B2 -> true;
equals2({link,_,_,_},{link,_,_,_}) -> false.

involves({link,U1,_,_},{agent,U1,_,_}) -> true;
involves({link,_,U2,_},{agent,U2,_,_}) -> true;
involves(_,_) -> false.

hasLink(Test,Links) -> lists:any(fun(X) -> equals2(X,Test) end, Links).

addLink(Link,Links) ->
    Has = hasLink(Link,Links),
    if not Has -> [Link|Links];
       true -> Links
    end.


%% Population
%% _____________________________________________________________

newPopulation(N) -> [newAgent(X) || X <- lists:seq(0,N-1)].

getPopFromWorld(W) -> W#world.population.

updatePopulation(Link,Pop) -> [updateAgentWithLink(A,Link) || A <- Pop].

%% Get all links to an agent A of type T
linksTo(A,T,{world,_,_,Links}) -> linksTo(A,T,Links);
linksTo(_,_,[]) -> [];
linksTo(A,Type,[{link,V1,_,T2}=L|T]) when
       A==V1, Type==T2 -> [L|linksTo(A,Type,T)];
linksTo(A,Type,[{link,_,V2,T2}=L|T]) when
       A==V2, Type==T2 -> [L|linksTo(A,Type,T)];
linksTo(A,Type,[_|T]) -> linksTo(A,Type,T).

linksTo(A,W) -> linksTo(A,1,W) ++ linksTo(A,2,W).


%% Neighbours

%% get the agent record from the world
getAgent(Id,{world,_,Pop,_}) -> getAgent(Id,Pop);

getAgent(_,[]) -> false; % error, how to handle?
getAgent(Id,[H|_]) when H#agent.uid == Id -> H;
getAgent(Id,[_|T]) -> getAgent(Id,T).

%% turn a list of ids into a list of agent records
agentsFromList([],W) ->[];
agentsFromList([H|T],W) -> [getAgent(H,W)|agentsFromList(T,W)].

%% get all the agents from a list of links except the A agent
getAgentsFromLinkListExcept([],A) -> [];
getAgentsFromLinkListExcept([{link,A1,A2,_}|T],A) when A==A1 -> [A2 | getAgentsFromLinkListExcept(T,A)];
getAgentsFromLinkListExcept([{link,A1,A2,_}|T],A) when A==A2 -> [A1 | getAgentsFromLinkListExcept(T,A)].

find(_,[]) -> false;
find(X,[H|_]) when X == H -> true;
find(X,[H|T]) -> find(X,T).

dedupe([]) -> []; %% remove duplicates
dedupe([H|T]) ->
    F = find(H,T),
    if F -> dedupe(T);
       true -> [H|dedupe(T)]
    end.
    

hood(A,Type,{world,_,_,Links}=W)-> hood(A,Type,Links,W).
hood(A,Type,Links,{world,_,Pop,Links}=W) ->
    AllLinks = linksTo(A,Type,Links),
    dedupe(getAgentsFromLinkListExcept(AllLinks,A)).


getCluster(A,Type,{world,_,Pop,_}=World) ->  dedupe(getCluster(A,Type,World,[])).

getCluster(A,Type,World,Visited) ->
    NewVisited = [A|Visited],
    {Old,New} = lists:partition(fun(X) -> lists:member(X,NewVisited) end, hood(A,Type,World)), %% get all the univisited into New
    Recursive = fun(X) -> getCluster(X,Type,World,NewVisited) end,
    [A |
        lists:flatmap(Recursive,New)
    ].

len([]) -> 0;
len([_|T])-> 1+len(T).

getClusterSize(A,Type,World) -> len(getCluster(A,Type,World)).
    


%% World
%% _____________________________________________________________

newWorld(Size) ->
    #world{size=Size,population=newPopulation(Size),links=[]}.

makeLink(World,FChooseAgent, FChooseType) ->
    A = FChooseAgent(World),
    B = FChooseAgent(World),    
    T = FChooseType(A,B,World),
    if  A == B -> makeLink(World,FChooseAgent,FChooseType); %% can't make link to self.
  	T == false -> makeLink(World,FChooseAgent,FChooseType); %% in some rules, agents can refuse to make a link
	true -> newLink(A,B,T) %% seems to be OK       
    end.

addLinkToWorld(World,FChooseAgent,FChooseType) ->
   Link = makeLink(World, FChooseAgent, FChooseType),
   World#world{
        population = updatePopulation(Link,World#world.population),
        links = addLink(Link,World#world.links)
   }.

%% Testing


randomWorld(Size,NoLinks) ->
    RandomLink = fun(X) -> newLink(random:uniform(Size),random:uniform(Size),1) end,
    NewLinks = [RandomLink(X) || X <- lists:seq(0,NoLinks)],
    {world,Size,Pop,Links} = newWorld(Size),
    {world,Size,Pop,NewLinks}.
    
test() ->
    {world,Size,Pop,Links} = W = newWorld(10),
    FA = fun(_) -> random:uniform(10) end,
    FT = fun(_,_,_) -> 1 end,
    W1 = addLinkToWorld(W,FA,FT),
    W2 = addLinkToWorld(W1,FA,FT),
    W3 = addLinkToWorld(W2,FA,FT),
    io:fwrite("~w~n",[W3]),
    hood(1,1,addLinkToWorld(W3,FA,FT)).


%% Unit tests
%% _________________
unitTests() -> true.
