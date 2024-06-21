module Dykstra (dykstra, test, getDistanceTo, getPathTo) where

import GraphFunctions
import qualified Data.Map as Map
import Data.Map(Map)
import Data.Set (Set)
import qualified Data.Set as Set

type Distance = Float
type Visited = Map Node (Distance, [Node])
type Unvisited = Set (Distance, Node, Node) --main data structure, order by distance
type Unvisited' = Map Node (Distance, Node) -- auxiliary data structure, order by node

-- function that updates the variables in each iteration:
-- visited (u: distance and path) -> unvisited (set: distance, node, node_from) -> unvisited': like unvisited node - current max distance
subRoutine:: Graph Distance -> Visited -> Unvisited -> Unvisited' -> (Visited, Unvisited, Unvisited')
subRoutine g visited unvisited unvisited'
    |Set.null unvisited = (visited, unvisited, unvisited')         --if not edges to go, finish
    |otherwise = subRoutine g visitedNew unvisitedNew unvisited'New  --else recurse
        where
            -- 1) find + remove the minimum from unvisited set
            ((dv, v, u), unvisitedRest) = Set.deleteFindMin unvisited

            -- 2) remove also from auxiliary unvisited hash map
            unvisited'Rest = Map.delete v unvisited'
            
            -- 3,4) Add the minimum to visited (distance and path)
            uPath = snd $ Map.findWithDefault (-1, []) u visited
            visitedNew = Map.insert v (dv, (u:uPath)) visited

            -- 5) Get new edges from the new vertex v, skip nodes that are already visited
            newEdges = Map.filterWithKey (\w _ -> Map.notMember w visited) $ g Map.! v
            
            -- 6) update B with new edges
            (unvisitedNew, unvisited'New) = Map.foldlWithKey (updateUnvisited dv) (unvisitedRest, unvisited'Rest) newEdges

            -- current distance to v -> current unvisited -> w (node to) -> current dykstra distance to w
            updateUnvisited :: Distance -> (Unvisited, Unvisited') -> Node -> Distance -> (Unvisited, Unvisited')
            updateUnvisited d_sv (ws, ws') w d_vw = (Set.insert (dw', w, v') ws, Map.insert w (dw', v') ws')
                where
                    vNew = v
                    dwNew = d_vw + d_sv
                    (dwOld, vOld) = Map.findWithDefault ((1/0), -1) w ws'
                    v' = if dwOld <= dwNew then vOld else vNew
                    dw' = if dwOld <= dwNew then dwOld else dwNew

dykstra :: Graph Distance -> Node -> Visited
dykstra g s = visited
    where
        visited0 = Map.empty :: Visited
        unvisited0 = Set.singleton (0, s, s)
        unvisited'0 = Map.insert s (0, s) Map.empty
        (visited, _, _) = subRoutine g visited0 unvisited0 unvisited'0

getDistanceTo :: Visited -> Node -> Distance -- (Distance, [Node])
getDistanceTo vs = fst.(vs Map.!)

getPathTo :: Visited -> Node -> [Node] -- (Distance, [Node])
getPathTo vs = snd.(vs Map.!)

test :: IO()
test = do
    let edges = [(1, 2, 7.0),(1, 3, 9.0),(1, 6, 14.0),(2, 3, 10.0),(2, 4, 15.0),(3, 4, 11.0),(3, 6, 2.0),(4, 5, 6.0),(5, 6, 9.0)]
    let g = fromEdges edges
    print g
    let visited = dykstra g 1
    print visited
    