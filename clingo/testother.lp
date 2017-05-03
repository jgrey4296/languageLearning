%Enumerate types of move:
moveType(1..10).
%specify size of conversation:
moves(5).
%specify start conditions:
start_on(1).
end_on(5).

%Generate moves:
{ move(MT, T) : moveType(MT) } = 1 :- moves(S), T=1..S.

%Define features

%Constraints on the moves
:- move(2,T), move(3,T+1).
:- move(X,T), move(X,T2), T != T2.

:- not move(5,3).
:- not move(2,_).

:- move(2,T), move(5,T2), T < T2.

%Output:
#show move/2.
