import Prob

diceRoll = 1/6 :: Rational

d1 = Prior "one" diceRoll
d2 = Prior "two" diceRoll
d3 = Prior "three" diceRoll
d4 = Prior "four" diceRoll
d5 = Prior "five" diceRoll
d6 = Prior "six" diceRoll
dModel = Model [d1,d2,d3,d4,d5,d6]
t1 = verifyModel dModel
t2 = getProbValue (Dependent "oneAndSix" d1 d6)-- == (1/36::Rational)


tests = all id [ t1 ]
