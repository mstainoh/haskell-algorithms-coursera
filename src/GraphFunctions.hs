module GraphFunctions (
    Node, Edge, Graph, emptyGraph,
    addNode, getNodes, 
    addEdge, addDoubleEdge, fromEdges,
    reverseGraph,
    depthFirstSearch, depthFirstSearchFull
 ) where

import qualified Data.Map as Map
import Data.Map(Map)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (intercalate)

type Node = Int
type Edge = (Node, Node)
type Graph = Map Node (Set Node)

emptyGraph = Map.empty :: Graph

---------------
-- Graph manipulation
------------
-- if key in map, ignore, otherwise create it as empty set
addNode:: Graph -> Node -> Graph
addNode g node = Map.insertWith (const id) node Set.empty g

getNodes:: Graph -> [Node]
getNodes = Map.keys

-- add keys, then add the second node to the first, i.e. v1: {...} -> v1: {v2, ...}
addEdge:: Graph -> Edge -> Graph
addEdge g e = Map.adjust (Set.insert $ snd e) (fst e) g_new
    where g_new = foldl addNode g [fst e, snd e]

fromEdges :: [Edge] -> Graph 
fromEdges = foldl addEdge emptyGraph

addDoubleEdge :: Graph -> Edge -> Graph
addDoubleEdge g e = addEdge  (addEdge g e) (snd e, fst e)

reverseGraph :: Graph -> Graph
reverseGraph g_old = Map.foldlWithKey (\g u vs -> Set.foldl (\g v -> addEdge g (v, u)) g vs) emptyGraph g_old 

--------------
-- DFS
--------------
-- graph -> Startin node -> visited (set), node_finish time (stack)
depthFirstSearch:: Graph -> Node -> (Set Node, [Node]) -> (Set Node, [Node])
depthFirstSearch g u (visited, nodeFinishTimes)
    -- if the node is visited (or is not in G) -> continue
    |not $ Map.member u g = (visited, nodeFinishTimes)
    |Set.member u visited = (visited, nodeFinishTimes)

    -- if the node is not visited, we recurse on its output edges, and once that is finished, we push the node to the stack (u:node)
    |otherwise = (visitedSub, u:nodeFinishTimesSub) 
    where
        nextNodes = Map.findWithDefault Set.empty u g
        visitNextNode = depthFirstSearch g --recursive function to fold
        (visitedSub, nodeFinishTimesSub) = Set.foldr visitNextNode (Set.insert u visited, nodeFinishTimes) nextNodes

depthFirstSearchFull :: Graph -> (Set Node, [Node])
depthFirstSearchFull g = foldr (depthFirstSearch g) (Set.empty, []) (getNodes g)

-- test function
main :: IO()
main = do
    let edges = [(1,2), (2,3), (3,4), (4,2), (4, 5), (6, 1)]
    let g = foldl addEdge emptyGraph edges
    let grev = reverseGraph g
    print("graph:")
    print(g)
    print("graph reversed:")
    print(grev)