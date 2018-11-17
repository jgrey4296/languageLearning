A :- [1,2,3,4].
member(X, [X|_]).
member(X,[_|T]) :- member(X,T).

%% Counting:
len([],0).
len([_|T],N) :- len(T,X), N is X+1.
%% Accumulation:
accLen([_|T],A,L) :- Anew is A+1, accLen(T, Anew, L).
accLen([],A,A).
leng(List, Length) :- accLen(List, 0, Length).

%% Max:

accMax([H|T],A,Max) :- H > A, accMax(T,H,Max).
accMax([H|T],A,Max) :- H =< A, accMax(T,A,Max).
accMax([], A, A).
max(List, Max) :- List = [H|_], accMax(List, H, Max).

append([],L,L).
append([H|T],L2,[H|L3]) :- append(T,L2,L3).
