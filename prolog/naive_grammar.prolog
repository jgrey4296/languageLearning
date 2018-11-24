:- module(naive_grammar, []).
:- use_module(list_test).

%% Naive Grammar:

s(Z) :- np(X), vp(Y), append(X,Y,Z).
np(Z) :- det(X), n(Y), append(X,Y,Z).
vp(Z) :- v(X), np(Y), append(X,Y,Z).
vp(Z) :- v(Z).

det([the]).
det([a]).
n([woman]).
n([man]).
v([shoots]).
         
