% File 'mbpp'
% Prediction and postdiction in the Monkey and Bananas domain
% ("Nonmonotonic Causal Theories," Section 3.3)

:- include mb.

% Prediction: Initially, the monkey is at l1, the bananas are at l2,
% and the box is at l3.  The monkey walks to l3 and then pushes the
% box to l2.  Does it follow that in the resulting state the monkey, the
% bananas and the box are at the same location?

:- query
   label :: 1;
   maxstep :: 2;
   0: loc(monkey)=l1,
   loc(bananas)=l2,
   loc(box)=l3,
   walk(l3);
   1: pushBox(l2);
   2: -(loc(monkey)=loc(bananas) & loc(bananas)=loc(box)).


% Postdiction: The monkey walked to location l3 and then pushed the box.
% Does it follow that the box was initially at l3?

:- query
   label :: 2;
   maxstep :: 2;
   0: walk(l3);
   1: [\/L | pushBox(L)];
   0: loc(box)\=l3.
