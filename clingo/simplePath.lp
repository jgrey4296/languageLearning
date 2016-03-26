%%initial declarations
%%the disks and pegs used
size(7).
%%the max number of moves:
moves(30).

grid(X,Y) :- size(S), X=0..S, Y=0..S.
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
%%MUST GO THROUGH:
:- not on(1,5,_).
:- not on(5,1,_).
:- on(X,_), X < 0.
:- on(_,Y), Y < 0.


%%#show move/3.
#show on/3.
