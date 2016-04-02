%%reminder: clingo -n 5 --rand-freq 0.5 --verbose=0 simpleMelody.lp

%%----------initial declarations
%%Pitches - Chromatic
notePitch(1;2;3;4;5;6;7;8;9;10;11;12;wait).
%%Lengths: Quaver, Crotchet, Minim, Semibreve
noteLength(1;2;4;8).
%%Melody Options:
melodyLength(8).

%%Key Signature...

%%scale

%%channels:
channels(2).

%%----------generate
%%select a note, with a length, for each T of the melody length
1 { note(P,L,T,C) : notePitch(P), noteLength(L) } 1 :- melodyLength(A), T=1..A, channels(B), C=1..B.

%%----------define
%%harmony

%%discord

%%passing notes

%%important beats


%%----------test:
%%ends on key origin

%%cadence


