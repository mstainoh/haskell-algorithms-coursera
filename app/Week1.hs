module Main where

import Data.List (sortOn)
import System.IO (IOMode(ReadMode)) 

import GraphFunctions
import Kosaraju
import FileParser

parseEdgeList :: String -> [Edge]
parseEdgeList ss = map toTup $ map parseInts (lines ss)
    where
        toTup :: [Int] -> (Node, Node)
        toTup [] = error "empty List"
        toTup (x:xs) = (x, head xs)

main :: IO()
main = do
    let fname = "week1.txt"
    fpath <- getFilePathFromName fname
    handle <- hOpenFile fpath ReadMode
    contents <- hReadFile handle

    let edges = parseEdgeList contents
    let g = fromEdges edges
    
    putStrLn "Running"
    let sccs = kosaraju g

    putStrLn "Strongly Connected Components:"
    -- print sccs
    print "top 5 scc size:"
    print (take 5 $ sortOn negate $ map length sccs)

    hCloseFile handle