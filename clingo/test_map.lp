size(4).
tile(X, Y) :- size(N), X=1..N, Y=1..N.
tilecount(X) :- X = #count { tile(A,B) : tile(A,B) }.
:- tilecount(X), size(S), X != S*S.
start(1,1).
end(N,N) :- size(N).
speech(greet;goodbye;question;answer;counter;avoidance;smalltalk).

{move(X,Y,S) : speech(S) } = 1 :- tile(X,Y).

:- MC = #count { move(X,Y,S) : move(X,Y,S) }, tilecount(TC), TC != MC.

:- not move(X,Y,greet), start(X,Y).
:- not move(X,Y,goodbye), end(X,Y).

:- C = #count { move(X,Y,greet) : move(X,Y,greet) }, C > 1.
:- C = #count { move(X,Y,goodbye) : move(X,Y,goodbye) }, C > 1.
:- C = #count { move(X,Y) : move(X,Y,(question;answer;counter;avoidance)) }, C != 1.



#show move/3.
