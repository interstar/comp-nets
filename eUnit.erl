-module(eUnit).
-export([
    assertEquals/3, assertNEquals/3
]).

%% Unit Testing
%% _____________________________________________________

assertEquals(X,Y,_) when X == Y -> true;
assertEquals(X,Y,Err) ->
    io:fwrite("Error ~w: (~w  /= ~w )",[Err,X,Y]), false.

assertNEquals(X,Y,_) when X /= Y -> true;
assertNEquals(X,Y,Err) ->
    io:fwrite("Error ~w: (~w == ~w )",[Err,X,Y]), false.
