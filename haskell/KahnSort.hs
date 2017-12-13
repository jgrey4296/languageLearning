{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
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

module KahnSort where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Foldable as DFold

type Node = String
--Adjacency List of:  Node -> [Descendents, Ancestors]
type AdjList a = Map.Map a (Set.Set a, Set.Set a)
type NodeOrder = (Node, Int)

defaultEmptyEntry :: (Set.Set Node, Set.Set Node)
defaultEmptyEntry = (Set.empty, Set.empty)

----TOP LEVEL ENTRY
-- Returns either Sorted OrderVals, or a Back-Edge
kahnSort :: AdjList Node -> [Node] -> Either (Map.Map Node Int) (Node, Node)
kahnSort adj initial = result
  where result = kahnSortRec adj adj Map.empty (Set.fromList initial) Set.empty


----The Recursive portion of kahnSort
kahnSortRec :: AdjList Node  --Graph
            -> AdjList Node  --Pristine Graph
            -> Map.Map Node Int --Sorted
            -> Set.Set Node  --Frontier
            -> Set.Set Node  --Active
            -> Either (Map.Map Node Int) (Node, Node) --Sorted or BackEdge
kahnSortRec adj adjPure orderMap frontier discovered
  | Set.null frontier && Set.null discovered = Left $ orderMap
  | Set.null frontier = Right $ detectConflict adj discovered
  | otherwise = result
  where result = kahnSortRec adj' adjPure orderMap' frontier'' discovered''
        --Get the current state of things
        currentNode = Set.elemAt 0 frontier
        frontier' = Set.deleteAt 0 frontier
        --Increment the nodes order from the max of any ancestor
        orderMap' = calculateOrderForNode currentNode orderMap adjPure
        --Delete edges, expand the frontier, store discovered but still active nodes
        (addToFrontier, potentialActives, adj') = processNode currentNode adj
        frontier'' = Set.union frontier' addToFrontier
        stillActive = Set.difference potentialActives (Map.keysSet orderMap)
        discovered' = Set.union discovered stillActive
        discovered'' = Set.difference discovered' addToFrontier


calculateOrderForNode :: Node -> Map.Map Node Int -> AdjList Node -> Map.Map Node Int
calculateOrderForNode n currentOrders adj = result
  where ancestorOrder = do
          ancestors <- Map.lookup n adj >>= (\x -> Just $ Set.toList $ snd x)
          orders <- Just $ fmap (\x -> Map.findWithDefault 0 x currentOrders) ancestors
          maxOrder <- Just $ DFold.foldl' (max) 0 orders
          return maxOrder
        updatedOrders = case ancestorOrder of
                          Just x -> Map.insert n (x + 1) currentOrders
                          Nothing -> Map.insert n 1 currentOrders
        result = updatedOrders


--Given a node, delete edges to descendents,
--and return nodes without any ancestors
-- along with discovered nodes that have ancestors
-- and the modifed adjaceny list
processNode :: Node -> AdjList Node -> (Set.Set Node, Set.Set Node, AdjList Node)
processNode n adj = (frontier, active, adj')
  where
    descendents = fst $ Map.findWithDefault defaultEmptyEntry n adj
    frontier = Set.filter (hasOnlyParentN n adj) descendents
    active = Set.filter (\x -> not $ hasOnlyParentN n adj x) descendents
    adj' = Map.map (\(decs, ancs) -> (decs, Set.delete n ancs)) adj

--Checks a given Node for having only the specified ancestor
hasOnlyParentN :: Node -> AdjList Node -> Node -> Bool
hasOnlyParentN targetParent adj current = result
  where ancestors = snd $ Map.findWithDefault defaultEmptyEntry current adj
        result = Set.size ancestors == 1 && Set.member targetParent ancestors

detectConflict :: AdjList Node -> Set.Set Node -> (Node, Node)
detectConflict adj disc = ("a", "b")
