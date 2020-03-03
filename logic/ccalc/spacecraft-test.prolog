% File 'spacecraft-test':  "Describing Additive Fluents in C+", Section 7

:- maxAdditive :: 7.

:- macros
   mass -> 1;
   maxForce -> 2.

:- include 'spacecraft'.

:- query
   maxstep :: 1;
   0: (pos(x) = -1  &  pos(y) =  0  &  pos(z) =  1);
   0: (vel(x) =  0  &  vel(y) =  1  &  vel(z) =  1);
   1: (pos(x) =  0  &  pos(y) =  3  &  pos(z) =  1).

:- show pos(Ax); vel(Ax).
