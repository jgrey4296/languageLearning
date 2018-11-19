:- module(list_test,[palindrome/1]).

%%A :- [1,2,3,4].
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

%% Appending:
append([],L,L).
append([H|T],L2,[H|L3]) :- append(T,L2,L3).

prefix(P,L) :- append(P,_,L).
suffix(S,L) :- append(_,S,L).
sublist(SubL,L) :- suffix(S,L), prefix(SubL,S).


accRev([H|T],C,R) :- accRev(T,[H|C],R).
accRev([],C,C).
my_reverse(L,RevL) :- accRev(L,[],RevL).


palindrome(X) :- my_reverse(X,Y), X = Y.

accFlat([H|T],C,R) :- accFlat(H,C,CP), accFlat(T,CP,R).
accFlat([],C,C). 
accFlat(A,C,[A|C]).

flatten(L,FL) :- accFlat(L,[],RevFL), my_reverse(RevFL,FL).
