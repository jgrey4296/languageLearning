node(a;b;c;d).
edge(a,b;b,c;c,d).
edge(a,c).
colour(red;green;blue).

1 { assign(N,C) : colour(C) } 1 :- node(N).

:- edge(N1,N2), assign(N1,C), assign(N2,C).

#show assign/2.
