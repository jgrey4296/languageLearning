r(a).
r(b).
r(c).

q(1..5).

{ p(X,Y) : q(X), X < 2 } = 1 :- r(Y).


s(Y) :- Y = #sum { X : q(X) }.

1 { 2: a(x); 8: a(y) } 1
