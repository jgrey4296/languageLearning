module Prob where

type Action = String
type State = String

data Probability = Prior Action Rational
                 | Independent Action Probability Probability
                 | Dependent Action Probability Probability deriving (Show,Eq,Ord)

data Model = Model [Probability] deriving (Show,Eq)

data Belief = Belief (Action, Int) (State, Int) deriving (Show,Eq,Ord)

name Prior a _ = a
name Independent a _ _ = a
name Dependent a _ _ = a

getProbValue (Prior _ f) = f
getProbValue (Independent _ f g) = getProbValue f * getProbValue g
getProbValue (Dependent _ f g) =  (prior * given) / given where
  prior = getProbValue f
  given = getProbValue g

and f g = Independent (name f ++ " and "  ++ name g) f g
or f g = Independent (name f ++ " or " ++ name g) f g 

-- A Probability model must sum to 1
--verifyModel :: [Probability] -> Bool
verifyModel (Model xs) = 1 == sumProb where
  sumProb = sum $ map (getProbValue) xs


