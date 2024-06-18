module Main where

import Data.List (sortOn)
import System.IO (IOMode(ReadMode)) 

import GraphFunctions
import Kosaraju
import FileParser

main :: IO()
main = do
    let fname = "week1.txt"
    fpath <- getFilePathFromName fname
    handle <- hOpenFile fpath ReadMode
    contents <- hReadFile handle

    let edges = map (\(x:xs) -> (x, head xs)) $ map (parseInts) (lines contents)
    let g = fromEdges edges
    
    putStrLn "Running"
    let sccs = kosaraju g

    putStrLn "Strongly Connected Components:"
    -- print sccs
    print "top 5 scc size:"
    print (take 5 $ sortOn negate $ map length sccs)

    hCloseFile handle