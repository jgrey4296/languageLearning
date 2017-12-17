
import qualified KahnSortM as KS
import qualified Data.Map as Map
import qualified Data.Set as Set 
import qualified Data.Foldable as DFold

type Node = String
type AdjList = KS.AdjList Node

--utility to construct adjacency lists
--Takes a list of tuples of (Node, Descendents)
buildAdjList :: [(Node, Node)] -> AdjList
buildAdjList xs = populated
  where 
        initialList = Map.empty
        populated = DFold.foldl' (\m v -> addPairToAdjList v m) initialList xs
        

addPairToAdjList :: (Node, Node) -> AdjList -> AdjList
addPairToAdjList (p, c) adj = adj''
  where emptyResult = (Set.empty, Set.empty)
        (p_desc, p_anc) = Map.findWithDefault emptyResult p adj
        p_entry = (Set.insert c p_desc, p_anc)
        (c_desc, c_anc) = Map.findWithDefault emptyResult c adj
        c_entry = (c_desc, Set.insert p c_anc)
        adj' = Map.insert p p_entry adj
        adj'' = Map.insert c c_entry adj'

testCompare :: (([(Node, Node)], [Node]), Either [(Node, Int)] (Node, Node)) -> Bool
testCompare ((graph, init), Left trueResult) = sorted == trueResult
  where adjList = buildAdjList graph
        Left result = KS.runKahnSortM adjList init
        sorted = Map.toList result
        
testCompare ((graph, init), Right (a, b)) = a == x && b == y
  where adjList = buildAdjList graph
        Right (x, y) = KS.runKahnSortM adjList init
  

-- Test Values of ((edgePairs, edge), Either EdgeOrders ConflictPair)
testValues = [
  (([("a", "b")], ["a"]), Left $ [("a", 1), ("b", 2)])
  
  ,(([("a", "b"),
      ("b", "c")], ["a"]), Left $ [("a", 1),
                                   ("b", 2),
                                   ("c", 3)])

  ,(([("a", "c"),
      ("b", "c")], ["a", "b"]), Left $ [("a", 1),
                                        ("b", 1),
                                        ("c", 2)])

  ,(([("a", "c"),
      ("b", "c"),
      ("c", "d")], ["a", "b"]), Left $ [("a", 1), ("b", 1),
                                         ("c", 2), ("d", 3)])

  ,(([("a", "c"),
      ("b", "c"),
      ("c", "d"),
      ("d", "c")], ["a", "b"]), Right $ ("d", "c"))
  ]
        
test :: Bool
test = all (testCompare) testValues
