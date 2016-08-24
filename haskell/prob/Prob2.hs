
type Probability = Rational

-- When Mutually Exclusive:
-- P(A or B) = P(A) + P(B)

-- When not mutually exclusive:
-- P(A or B) = P(A) + P(B) - P(A and B)

-- independent and:
-- P(A and B) = P(A) * P(B)

-- conditional / dependent and
-- P(A and B) = P(A) * P(B | A)

--conditional And alt
-- P(B | A) = P(A and B) / P(A)

-- conditional and reduces to independent and for one term as
--  P(B | A) -> A has happened thus is independent.


total = 29 :: Rational
gCubes = 8 / total
gCirc = 9 / total
yCubes = 5 / total
yCirc = 7 / total

pCubes = gCubes + yCubes
pYellow = yCubes + yCirc

-- p(yellow and cube) = p(yellow) * p(cube | yellow)
-- p(yellowCube | yellow) = p(yellowCube and cube) / p(yellowCube)
pYellowCubes = cubeGivenYellow / pYellow where
  cubeGivenYellow = (yCubes * pCubes) / yCubes

pYOrCube = pYellow + pCubes - yCubes

pGCubeOryCirc = gCubes + yCirc
