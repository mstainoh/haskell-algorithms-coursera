module Main where

import Data.List (sortOn)
import System.IO (IOMode(ReadMode), openFile, hGetContents, hClose) 

import GraphFunctions
import Kosaraju
import FileParser

parseEdgeList :: String -> [Edge Bool]
parseEdgeList ss = map toTup $ map parseInts (lines ss)
    where
        toTup :: [Int] -> (Node, Node, Bool)
        toTup [] = error "empty List"
        toTup (x:xs) = (x, head xs, True)

main :: IO()
main = do
    let fname = "graphsWeek1.txt"
    fpath <- getFilePathFromName fname
    handle <- openFile fpath ReadMode
    contents <- hGetContents handle

    let edges = parseEdgeList contents
    let g = fromEdges edges
    
    putStrLn "Running"
    let sccs = kosaraju g

    putStrLn "Strongly Connected Components:"
    -- print sccs
    print "top 5 scc size:"
    print (take 5 $ sortOn negate $ map length sccs)

    hClose handle