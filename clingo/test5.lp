
test(1..6).

mycount(X) :- X= #sum { Y : test(Y) }.
