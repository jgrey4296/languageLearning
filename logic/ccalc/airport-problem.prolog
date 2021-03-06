% File 'airport-problem'

:- include 'airport-domain'.

:- query
   maxstep :: 2;
   0: at(i,desk),
   at(desk,home),
   at(car,home),
   at(home,county),
   at(airport,county),
   -at(desk,car),
   disjoint(home,airport);
   maxstep: at(i,airport).
