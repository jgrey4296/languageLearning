%%reminder: clingo -n 5 --rand-freq 0.5 --verbose=0 simpleMelody.lp

%%----------initial declarations
%%Pitches - Chromatic
notePitch(1;2;3;4;5;6;7;8;9;10;11;12;13).
%%Melody Options:
numOfBars(4).
interval(0,4,8).

%%----------generate
%%select a chord for each bar
1 { chord(A+B,A+C,A+D,T) : notePitch(A), interval(B,C,D) } 1 :- numOfBars(W), T=1..W.

%%modulo it to keep it in range:
modChord(X,Y,Z,T) :- chord(A,B,C,T), X= A\12, Y= B\12, Z= C\12.

%%----------define
root(N,T) :- modChord(N,_,_,T).


%%----------test:
:- not root(0,1).
:- not root(0,M), numOfBars(M).
:- not root(5,M-1), numOfBars(M).


#show modChord/4.
