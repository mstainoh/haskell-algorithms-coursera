module Main where

import GraphFunctions

main :: IO()
main = do
    let edges = [(1,2), (2,3), (3,4), (4,2), (4, 5), (6, 1)]
    let g = foldl addEdge emptyGraph edges
    let grev = reverseGraph g
    print("graph:")
    print(g)
    print("graph reversed:")
    print(grev)

    let nodes = getNodes g
    let (_, nodeOrder) = depthFirstSearchFull grev
    
    putStrLn "First Pass"
    print nodeOrder

    let sccs = kosaraju g

    putStrLn "Strongly Connected Components:"
    print sccs

    -- print("Second pass")
    -- let (visited_2, node_order_2) = depthFirstSearchFull g (take 2 node_order)
    -- print(visited_2)
    -- print(node_order_2) 
