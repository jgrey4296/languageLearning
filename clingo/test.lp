0 { rain;sprinkler } 2.

:- rain, sprinkler.

wet(rain) :- rain.
wet(sprinkler) :- sprinkler.

dry :- not wet(_).
-dry :- wet(_).

:- not wet(_).


