-- TypeSynonymInstances, OverlappingInstances, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses
-- OverloadedStrings, OverloadedLists
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
------------------------------------------
-- A Test program to explore sorting rules into layers based on their inputs and outputs
------------------------------------------
import Prelude hiding (fst, snd)
import qualified Prelude (fst, snd)
import Data.Foldable (foldl')
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified KahnSortM as KS

--A Variable is, at heart, a name:
type BaseVar = String
--Adjacency List alias:
type AdjList a = KS.AdjList a

--But I want to now how 'symbolic' each variable is
--Where 'symbolic' means the number of steps from the base primitives
--Base Primitives are of Order 1
data OrderVar = OrderVar BaseVar Int deriving (Eq)
data Rule = Rule { inputs :: [OrderVar]
                 , outputs :: [OrderVar]
                 } deriving (Eq)


-- Min, Max, Output of a rule
type RuleOrder = (Int, Int, Int)
fst :: RuleOrder -> Int
fst (a, _, _) = a
snd :: RuleOrder -> Int
snd (_, b, _) = b
thd :: RuleOrder -> Int
thd (_, _, c) = c

--Display OrderVars and Rules            
instance Show OrderVar where
  show (OrderVar x y) = "Var( " ++ x ++ " : " ++ (show y) ++ ")"

instance Show Rule where
  show x = result
    where result = "Rule( " ++ ins ++ " -> " ++ outs ++ ")"
          ins = foldl' (++) "" $ map (\(OrderVar a b) -> a) (inputs x)
          outs = foldl' (++) "" $ map (\(OrderVar a b) -> a) (outputs x)
    
--Utilities to make an OrderVar of order 1 and rules
mkOVar :: BaseVar -> OrderVar
mkOVar x = OrderVar x 1

mkRule :: [BaseVar] -> [BaseVar] -> Rule
mkRule a b = Rule (map mkOVar a) (map mkOVar b)

-- Get the min and max of a list of ints
-- from https://stackoverflow.com/questions/33328413
range :: [Int] -> (Int, Int)
range (x:xs) = foldr (\x (tailMin, tailMax) -> (min tailMin x, max tailMax x)) (x, x) xs

--Get the min and max orders of input variables
ruleOrderRange :: Rule -> (Int, Int)
ruleOrderRange x = (minInput, maxInput)
  where inputVals = map (\(OrderVar a b) -> b) (inputs x)
        (minInput, maxInput) = range inputVals

--Get the output order of any variables
ruleOrder :: Rule -> Int
ruleOrder x = result
  where orderRange = ruleOrderRange x
        result = 1 + Prelude.snd orderRange



--Convert a single rule to: [forall inputs: (input, outputs)]
ruleToNeighbours :: Rule -> ([(BaseVar, Set.Set BaseVar)], [(BaseVar, Set.Set BaseVar)])
ruleToNeighbours r = result
  where ins = map (\(OrderVar a b) -> a) $ inputs r
        inSet = Set.fromList ins
        outs = map (\(OrderVar a b) -> a) $ outputs r
        outSet = Set.fromList outs
        descendents = map (\x -> (x, outSet)) ins
        ancestors = map (\x -> (x, inSet)) outs
        result = (descendents, ancestors)
        

--Used to fold all (input , [output]) pairs into a map/adjacencyList
addOrUpdateAdjListDesc :: AdjList BaseVar -> (BaseVar, Set.Set BaseVar) -> AdjList BaseVar
addOrUpdateAdjListDesc m (k, vs) = result
  where result = if Map.notMember k m then Map.insert k (vs, Set.empty) m
                 else Map.update (\(x, y) -> Just (Set.union x vs, y)) k m

addOrUpdateAdjListAnc :: AdjList BaseVar -> (BaseVar, Set.Set BaseVar) -> AdjList BaseVar
addOrUpdateAdjListAnc m (k, vs) = result
  where result = if Map.notMember k m then Map.insert k (Set.empty, vs) m
                 else Map.update (\(x, y) -> Just (x, Set.union y vs)) k m

--builds an adjacency list from a list of rules
buildAdjList :: [Rule] -> AdjList BaseVar -> AdjList BaseVar
buildAdjList [] m = m
buildAdjList (x:xs) m = buildAdjList xs m''
  where neighbours = ruleToNeighbours x
        m' = foldl' (addOrUpdateAdjListDesc) m $ Prelude.fst neighbours
        m'' = foldl' (addOrUpdateAdjListAnc) m' $ Prelude.snd neighbours


-- Topologically Sort, returned sorted rules or a conflict
applyTopologicalSort :: [Rule] -> Either [Rule] (BaseVar, BaseVar)
applyTopologicalSort = undefined

-- Primitives of Order 1
initPrimitives :: [OrderVar]
initPrimitives = map mkOVar [ "a", "b", "c", "d", "e", "f", "g" ]


-- Example Rules
exampleRules :: [Rule]
exampleRules = [ mkRule ["a", "b"] ["c"] --Order of C: 2
               , mkRule ["a", "c"] ["d"] --Order of D: 3
               , mkRule ["d"] ["e"]      --Order of E: 4
               , mkRule ["a", "e"] ["f"] --Order of F: 5
               ]
adj1 = buildAdjList exampleRules Map.empty

badRules :: [Rule]
badRules = [ mkRule ["a", "b"] ["c"] --Order of C: 2
               , mkRule ["a", "c"] ["d"] --Order of D: 3
               , mkRule ["d"] ["e"]      --Order of E: 4
               , mkRule ["a", "e"] ["f"] --Order of F: 5
               , mkRule ["a", "e"] ["c"] --Order of C: FAIL
               ]
adj2 = buildAdjList badRules Map.empty


