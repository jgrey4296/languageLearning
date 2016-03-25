person(bob). person(bill). person(jill).

friends(X,Y) :- person(X),person(Y),not(X=Y).
