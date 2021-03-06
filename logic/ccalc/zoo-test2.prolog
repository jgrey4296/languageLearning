% File 'zoo-test2'

:- sorts
   animal >> (elephant; horse; dog).

:- variables
   SP                           :: species;
   E                            :: elephant;
   HR                           :: horse;
   D                            :: dog.

:- objects
   elephantSpecies,
   horseSpecies,
   dogSpecies                   :: species.

caused sp(E)=elephantSpecies.
constraint sp(ANML)=elephantSpecies ->> [\/E | ANML=E].

caused sp(HR)=horseSpecies.
constraint sp(ANML)=horseSpecies ->> [\/HR | ANML=HR].

caused sp(D)=dogSpecies.
constraint sp(ANML)=dogSpecies ->> [\/D | ANML=D].

default largeSpecies(SP).
caused -largeSpecies(dogSpecies).


:- objects
   homer                        :: human.


% The gate was closed, and Homer was outside; after two steps,
% he was inside.  What can we say about his initial position?

:- query
   maxstep :: 2;
   0: -opened(gateAO),
   loc(pos(homer))=outside;
   maxstep: loc(pos(homer))=cageA.
