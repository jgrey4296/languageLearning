
test(1..6).

mycount(X) :- X= #max { Y : test(Y) }.
