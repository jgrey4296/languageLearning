{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE  ScopedTypeVariables #-}

------- Kahn's Algorithm to Topological sort a graph
-- L ← Empty list that will contain the sorted elements
-- S ← Set of all nodes with no incoming edge
-- while S is non-empty do
--     remove a node n from S
--     add n to tail of L
--     for each node m with an edge e from n to m do
--         remove edge e from the graph
--         if m has no other incoming edges then
--             insert m into S
-- if graph has edges then
--     return error (graph has at least one cycle)
-- else 
--     return L (a topologically sorted order)

module KahnSortM (
  AdjList,
  KahnResult,
  runKahnSortM
                ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Foldable as DFold
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Identity
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe

-------------------------------- TYPES
type Node = String

--Adjacency List of:  Node -> [Descendents, Ancestors]
type AdjList a = Map.Map a (Set.Set a, Set.Set a)
type NodeOrder = (Node, Int)
data KahnData = KahnState { adj :: AdjList Node,
                            sorted :: Map.Map Node Int,
                            frontier :: Set.Set Node,
                            discovered :: Set.Set Node }
type KahnResult = Either (Map.Map Node Int) (Node, Node)
-- CORE MONAD TRANSFORMER:                
type KahnM a = StateT KahnData (ReaderT (AdjList Node) Identity) a

-------------------------------- MONAD ENTRY:
runKahnSortM :: AdjList Node -> [Node] -> KahnResult
runKahnSortM adj initial = runIdentity $ runReaderT (evalStateT (kahnSortM_init initial) d) adj
  where d = KahnState adj (Map.empty) (Set.fromList initial) (Set.empty)

--iniialise the initial list orders:
kahnSortM_init :: [Node] -> KahnM KahnResult
kahnSortM_init initial = do
  sortList <- gets sorted
  let sortList' = DFold.foldl' (\m k -> Map.insert k 1 m) sortList initial
  modify (\x -> x { sorted = sortList' })
  kahnSortM

--Base Case
kahnSortM :: KahnM KahnResult
kahnSortM = do
  (front, disc, sortList) <- gets getFrontDiscSort
  if Set.null front && Set.null disc
    then return $ Left sortList
    else if Set.null front
         then detectConflictM
         else kahnSortM_Step

--Recursion Step
kahnSortM_Step :: KahnM KahnResult
kahnSortM_Step = do
  currentNode <- popFrontier
  newActiveNodes <- removeEdges currentNode
  updateActiveOrders newActiveNodes currentNode
  exhausted <- addToFrontier newActiveNodes
  let unexhausted = Set.difference newActiveNodes exhausted
  updateDiscovered exhausted unexhausted
  kahnSortM


--To Fail out, detect a cycle
detectConflictM :: KahnM KahnResult
detectConflictM = do
  --Get the smallest ordered active node
  orders <- gets sorted
  disco <- gets discovered
  let discoOrders = fmap (createOrderPair orders) $ Set.toList disco
  let (minNode, minOrder)  = DFold.foldl1 (getXPair (<)) discoOrders
  --get its remaining ancestors
  adjList <- gets adj
  let ancestors = getAncestors adjList (minNode, minOrder)  
  --get the largest ordered ancestor | an ancestor of 0 order
  let ancestorOrders = fmap (\x -> createOrderPair orders x) $ Set.toList ancestors
  let (maxAncestor, _) = DFold.foldl1 (getXPair (>)) ancestorOrders
  return $ Right (maxAncestor, minNode)
              
--Utilities
getXPair :: Ord a => (a -> a -> Bool) -> (Node, a) -> (Node, a) -> (Node, a)
getXPair cmp (c, cc) (n, nc)
  | cc `cmp` nc = (c, cc)
  | otherwise = (n, nc)

getAncestors :: AdjList Node -> (Node, Int) -> (Set.Set Node)
getAncestors m (n, i) = ancestors
  where result :: Maybe (Set.Set Node, Set.Set Node) = Map.lookup n m
        ancestors = maybe Set.empty (snd) result

createOrderPair :: Map.Map Node Int -> Node -> (Node, Int)
createOrderPair m n = result
  where result = (n, Map.findWithDefault 0 n m)

getFrontDiscSort :: KahnData -> (Set.Set Node, Set.Set Node, Map.Map Node Int)
getFrontDiscSort d = (front, disc, sortList)
  where 
    front = frontier d
    disc = discovered d
    sortList = sorted d

popFrontier :: KahnM Node
popFrontier = do
  front <- gets frontier
  let next = Set.elemAt 0 front
  modify (\d -> d { frontier = Set.deleteAt 0 front })
  return next

--Given a node, remove it from its descendents
removeEdges :: Node -> KahnM (Set.Set Node)
removeEdges n = do
  let emptyResult = (Set.empty, Set.empty)
  adjL <- gets adj
  let descendents =  fst $ (Map.findWithDefault emptyResult n adjL)
  --forall $x : delete n.descendent.$x.ancestor.n
  let adjL' = Set.foldl' (\x y -> Map.update (\(i, j) -> Just (i,
                                                                    Set.delete n j ))
                           y x) adjL descendents
  --Store it as the new adjacency list
  modify (\x -> x { adj = adjL' })
  return descendents

--Given a set of nodes, increment if necessary
updateActiveOrders :: Set.Set Node -> Node -> KahnM ()
updateActiveOrders active n = do
  currentOrder :: Int <- gets (\x -> Map.findWithDefault 1 n $ sorted x)
  sortMap <- gets sorted
  let sortMap' = Set.foldl' (\m k -> Map.alter (incOrder currentOrder) k m) sortMap active
  modify (\x -> x { sorted = sortMap' })

incOrder :: Int -> Maybe Int -> Maybe Int
incOrder curr Nothing = Just $ curr + 1
incOrder curr (Just x) = Just $ max x (curr + 1)

--Given a set of nodes, add any where len(x.ancestors) == 0
addToFrontier :: Set.Set Node -> KahnM (Set.Set Node)
addToFrontier active = do
  let emptyResult = (Set.empty, Set.empty)
  adj <- gets (\x -> adj x)
  --filter by ancestor size
  let activeFiltered = Set.filter (\x -> Set.null $ snd $ Map.findWithDefault emptyResult x adj) active
  --add remaining to frontier
  front <- gets frontier
  modify (\x -> x { frontier = Set.union front activeFiltered })
  return activeFiltered

--Remove exhausted from the active set,
--and add the unexhausted
updateDiscovered :: Set.Set Node -> Set.Set Node -> KahnM ()
updateDiscovered exhausted unexhausted = do
  disco <- gets discovered
  let disco' = Set.difference disco exhausted
  let disco'' = Set.union disco' unexhausted
  modify (\x -> x { discovered = disco'' })
