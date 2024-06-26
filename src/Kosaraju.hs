module Kosaraju(kosaraju, test) where

--------------------
-- Week 1: kosaraju algorithm for SCCs
-------------------

import GraphFunctions
import Data.Set(Set)
import qualified Data.Set as Set

------------
-- main function
-------------
kosaraju:: Graph a -> [[Node]]
kosaraju g = sccs
    where
        grev = reverseGraph g

        -- first pass
        (_, nodeOrder) = depthFirstSearchFull grev

        -- second pass
        collectSCCs :: [Node] -> (Set Node, [[Node]]) -> [[Node]]
        collectSCCs [] (_, sccList) = sccList -- once the list of nodes is exhausted, finish
        collectSCCs (n:ns) (visited, sccList)
            | Set.member n visited = collectSCCs ns (visited, sccList) -- if the node is visited, skip it
            | otherwise = -- if it is not visited, collect the nodes using depthFirstSerach
                let (newVisited, scc) = depthFirstSearch g n (visited, [])
                in collectSCCs ns (newVisited, scc : sccList)
            
        sccs = collectSCCs nodeOrder (Set.empty, [])

------------
-- test
-------------
test :: IO()
test = do
    let edges = [(1,2,True), (2,3, True), (3,4, True), (4,2, True), (4, 5, True), (6, 1, True)]
    let g = fromEdges edges
    let grev = reverseGraph g
    print("graph:")
    print(g)
    print("graph reversed:")
    print(grev)
    
    putStrLn "First Pass"
    let (_, nodeOrder) = depthFirstSearchFull grev
    print nodeOrder

    putStrLn "Strongly Connected Components:"
    let sccs = kosaraju g
    print sccs
    print (length sccs)