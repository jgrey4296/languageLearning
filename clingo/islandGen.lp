%Generating connected islands
%%% 
% Inputs:
#const numIslands = 10.
#const minConnections = 1.
#const maxConnections = 4.
#const numHubs = 2.

% Outputs: 
% island(id). islandEdge(id, id).

% Define:
island(1..numIslands).

% Generate:
minConnections { islandEdge(S, T) : island(S) } maxConnections :- island(T).

% Test:
numEdges(C, I) :- island(I), C = #count { edge(T) : islandEdge(I, T); edge(T) : islandEdge(T,I) }.
maxEdge(M) :- M = #max { X : numEdges(X, _) }.
hub(I) :- maxEdge(M), numEdges(M, I).

edgeChain(S,E,1) :- islandEdge(S,E).
edgeChain(S,E,T) :- edgeChain(S,M,A), edgeChain(M,E,B), T=A+B, T < 10.

largestEdgeChain(S,E,T) :- edgeChain(S,E,T), T = #max { A : edgeChain(S,_,A); B : edgeChain(_,S,B) }.

%Constrain
%Ensure min and max connectivity
:- islandEdge(S,T), islandEdge(T,S).
:- islandEdge(S,S).
:- edgeChain(S,S,_).
:- largestEdgeChain(S,S,_).





%Ensure there are hubs of max connectivity
:- maxEdge(M), C = #count { I : hub(I) }, C != numHubs, M != maxConnections.

% Output

%#show edgeChain/3.
#show largestEdgeChain/3.
