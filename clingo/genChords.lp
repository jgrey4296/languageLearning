% Generate sequences of chords
% Output format: chord(root, inversion, time).
% implicit in the program that consumes: root, scale, time, instrumentation

% Inputs: numSteps, numCadences, finalCadence, maxChordRepeats

%Generate: A Sequence of chord roots
%Generate: A Sequence of chord inversions

%combine: roots + inversions

% Test: Start on root, Number of Cadences, Final Cadence.
% Test: Number of chord repeats

%%----------Inputs
#const numSteps = 8. 
#const numCadences = 1.
#const finalCadence = 5.
#const maxChordRepeats = 2.

%%----------initial declarations
%%Pitches - Chromatic
notePitch(1..12).
inversion(1..5).
timeStep(1..numSteps).

allowableCadences(1;4).

%%----------generate
%%select a chord for each bar
{ chordRootAtT(T, X) : notePitch(X) } = 1 :- timeStep(T).
{ inversionAtT(T, X) : inversion(X) } = 1 :- timeStep(T).

% possible: generate cadences
final_cadence(X,Y) :- chordRootAtT(numSteps-1,X), chordRootAtT(numSteps,Y).
correctStart :- chordRootAtT(1,1). %start at tonic
correctFinish :- chordRootAtT(numSteps, finalCadence). %end on dominant


%Constrain
:- not correctStart.
:- not correctFinish.
:- { final_cadence(X,_) : allowableCadences(X) } = 0.

%%----------Output

#show chordRootAtT/2.
%#show inversionAtT/2.
#show final_cadence/2.
