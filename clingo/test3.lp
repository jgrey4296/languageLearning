#const size = 3.
#const maxTime = 6.

nodes(1..size,1..size).

start(1,1). end(size,size).

moveDelta(0,0; -1,0; 0,-1; 1,0; 0,1).

time(0..maxTime).

position(0,X,Y) :- start(X,Y).
position(maxTime,X,Y) :- end(X,Y).

{ position(T+1,Xp,Yp) : position(T,_,_) } = 1 :-
    position(T,X,Y),
    moveDelta(Dx,Dy),
    Xp = X+Dx,
    Yp = Y+Dy,
    nodes(Xp,Yp),
    T < maxTime.

:- 2 { position(T,_,_) }.

#show position/3.


