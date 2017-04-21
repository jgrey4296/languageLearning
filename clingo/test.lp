0 { rain;sprinkler;summer } 1.

%:- rain, sprinkler,summer.

wet(rain) :- rain.
wet(sprinkler) :- sprinkler.

dry :- not wet(_).
-dry :- wet(_).

%%:- not wet(_).


