% File 'coins'

:- macros
   n -> 10.

:- constants
   c :: inertialFluent(0..n);
   a ,
   b :: exogenousAction.

:- variables
   I :: 0..n-1.


a causes c=I+1 if c=I.
b causes c=I+2 if c=I, I+2 < n.

nonexecutable a if c >= n.
nonexecutable b if c >= n-2.

:- query          % prediction
   label :: 1;
   maxstep :: 2;
   0: c=5;
   0: a;
   1: a.

:- query          % postdiction
   label :: 2;
   maxstep :: 2;
   maxstep: c=5;
   0: a;
   1: a.

:- query          % planning
   label :: 3;
   maxstep :: 0..infinity;
   0: c=4;
   maxstep: c=10.
