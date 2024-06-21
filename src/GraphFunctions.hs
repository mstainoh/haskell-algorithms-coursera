module GraphFunctions (
    Node, Edge, Graph, emptyGraph,
    addNode, getNodes, 
    addEdge, addDoubleEdge, fromEdges, reverseGraph,
    depthFirstSearch, depthFirstSearchFull
 ) where

---------------
-- Graph manipulation
-- Graph are double hash tables (node -> node -> a), "a" can be anything, but in a simple case is a boolean (place holder) or a distance (float)
-- edges are tuples (Node, Node, "a")
-- Nodes are integers
------------

import qualified Data.Map as Map
import Data.Map(Map)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (intercalate)

type Node = Int
type Edge a = (Node, Node, a)
type Graph a = Map Node (Map Node a)

emptyGraph = Map.empty

---------------
-- Graph manipulation
------------
-- add the node to the graph, if key in map, ignore, otherwise create it as empty set. O(log n)
addNode:: Graph a -> Node -> Graph a
addNode g node = Map.insertWith (const id) node Map.empty g

getNodes:: Graph a -> [Node]
getNodes = Map.keys

-- add the keys, then add the second node to the first, i.e. v1: {...} -> v1: {v2, ...}. O(log m + log n)
addEdge:: Graph a -> Edge a -> Graph a
addEdge g e = Map.adjust (Map.insert n2 val) n1 g_new
    where 
        g_new = foldl addNode g [n1, n2]
        (n1, n2, val) = e

fromEdges :: [Edge a] -> Graph a
fromEdges = foldl addEdge emptyGraph

-- adds a double edge (e.g. non directed graphs)
addDoubleEdge :: Graph a -> Edge a -> Graph a
addDoubleEdge g e = addEdge  (addEdge g e) (n2, n1, val)
    where 
        (n1, n2, val) = e

-- reverses the graph, double fold {u1: {v1: a1, v2: a2}, u2: {...} ). First on each value v_i inside each key u, then for each key u
-- Supposed to be in O(m * log m), since m operations, each insert is O(log m) in the heap 
reverseGraph :: Graph a -> Graph a
reverseGraph g_old = Map.foldlWithKey (\g u vs -> Map.foldlWithKey (\g v val -> addEdge g (v, u, val)) g vs) emptyGraph g_old 

--------------
-- DFS
--------------
-- graph -> Startin node -> visited (set), node_finish time (stack)
depthFirstSearch:: Graph a -> Node -> (Set Node, [Node]) -> (Set Node, [Node])
depthFirstSearch g u (visited, nodeFinishTimes)
    -- if the node is visited (or is not in G) -> continue
    |not $ Map.member u g = (visited, nodeFinishTimes)
    |Set.member u visited = (visited, nodeFinishTimes)

    -- if the node is not visited, we recurse on its output edges, and once that is finished, we push the node to the stack (u:node)
    |otherwise = (visitedSub, u:nodeFinishTimesSub) 
    where
        nextNodes = Map.keysSet $ Map.findWithDefault Map.empty u g
        visitNextNode = depthFirstSearch g --recursive function to fold
        (visitedSub, nodeFinishTimesSub) = Set.foldr visitNextNode (Set.insert u visited, nodeFinishTimes) nextNodes

depthFirstSearchFull :: Graph a -> (Set Node, [Node])
depthFirstSearchFull g = foldr (depthFirstSearch g) (Set.empty, []) (getNodes g)

-- test function
main :: IO()
main = do
    let edges = [(1,2, 0.5), (2,3, 0.5), (3,4, 10), (4,2, 5), (4, 5, 0.3), (6, 1, 4)]
    let g = fromEdges edges :: Graph Float
    print("graph:")
    print(g)

    print("Grev")
    let grev = reverseGraph g
    print(grev)

    print("DFS on grev")
    let (visited, nodeFinishTimes) = depthFirstSearchFull grev
    print("visited: " ++ show visited)
    print("finish times: " ++ show nodeFinishTimes)


