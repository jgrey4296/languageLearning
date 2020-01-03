%%initial declarations
%%the disks and pegs used
disk(1..3).
peg(1..3).
%%start state and goal state
disk_init_on_peg(D,1) :- disk(D).
disk_fin_on_peg(D,3) :- disk(D).
%%the max number of moves:
moves(7).

%%generate
1 { move(D,P,T) : disk(D),peg(P) } 1 :- moves(M), T=1..M.

%%define
move(D,T) :- move(D,_,T).
on(D,P,0) :- disk_init_on_peg(D,P).
on(D,P,T) :- move(D,P,T).
on(D,P,T+1) :- on(D,P,T), not move(D,T+1), not moves(T).

blocked(D-1,P,T+1) :- on(D,P,T),disk(D), not moves(T).
blocked(D-1,P,T) :- blocked(D,P,T), disk(D).

%%test
:- move(D,P,T),blocked(D-1,P,T).
:- move(D,T), on(D,P,T-1), blocked(D,P,T).
:- disk_fin_on_peg(D,P), not on(D,P,M), moves(M).
:- not 1 { on(D,P,T) : peg(P) } 1, disk(D), moves(M), T = 1..M.

#show move/3.
