% File 'mcp-test': "Describing Additive Fluents in C+", Section 8

:- maxAdditive :: 3.

:- include 'mcp'.

:- query
   maxstep :: 6..7;
   0: (num(mi,bank1) = 3 & num(ca,bank1) = 3);
   0: (num(mi,bank2) = 0 & num(ca,bank2) = 0);
   0: [/\V | loc(V) = bank1];
   maxstep: (num(mi,bank1) = 0 & num(ca,bank1) = 0).

:- show loc; num.