child(janet, caroline).
child(caroline, bridget).
child(bridget,anne).

descendant(X,Y) :- child(X,Y).
%% descendant(X,Y) :- child(X,Z), child(Z,Y).
descendant(X,Y) :- child(X,Z), descendant(Z,Y).
