%%reminder: clingo -n 5 --rand-freq 0.5 --verbose=0 simplePath.lp

%%----------initial declarations
%%the disks and pegs used
size(7).
%%the max number of moves:
moves(30).
%% the grid itself
grid(0..X,0..Y) :- size(S), X=0..S, Y=0..S.
%%start state and goal state
startPos(0,0).
endPos(S,S) :- size(S).
%% waypoints:
waypoint(1,6,1).
waypoint(6,1,2).
%%valid directions:
validDir(X,0) :- X=-1..1, X != 0.
validDir(0,Y) :- Y=-1..1, Y != 0.

%%----------generate
1 { move(XD,YD,T) : validDir(XD,YD) } 1 :- moves(M), T=1..M.

%%----------define
on(X,Y,0) :- startPos(X,Y).
on(X+DX,Y+DY,T+1) :- on(X,Y,T),move(DX,DY,T+1), not moves(T).
used(X,Y) :- on(X,Y,_).

%%----------test:
:- used(X,Y), not grid(X,Y).
:- startPos(X,Y), not used(X,Y).
:- endPos(X,Y), not used(X,Y).
%%MUST GO THROUGH:
:- waypoint(X,Y,_), not used(X,Y).
:- waypoint(X,Y,T), waypoint(X2,Y2,T2), on(X,Y,T3), on(X2,Y2,T4), T < T2, T3 > T4.
:- used(X,_), X < 0.
:- used(_,Y), Y < 0.


#show used/2.

