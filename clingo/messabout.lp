%%initial declarations
%%the disks and pegs used
size(8).
%%the max number of moves:
moves(18).

grid(X,Y) :- size(S), X=1..S, Y=1..S.
%%start state and goal state
startPos(1,1).
endPos(S,S) :- size(S).
%%valid directions:
validDir(X,0) :- X=-1..1, X != 0.
validDir(0,Y) :- Y=-1..1, Y != 0.


%%generate
1 { move(XD,YD,T) : validDir(XD,YD) } 1 :- moves(M), T=1..M.

%%define
on(X,Y,0) :- startPos(X,Y).
on(X+DX,Y+DY,T+1) :- on(X,Y,T),move(DX,DY,T+1), not moves(T).

%%test
:- on(X,Y,_), not grid(X,Y).
:- not on(X,Y,0), startPos(X,Y).
:- not on(X,Y,E), moves(E), endPos(X,Y).
:- not on(3,3,_).

%%#show move/3.
#show on/3.
