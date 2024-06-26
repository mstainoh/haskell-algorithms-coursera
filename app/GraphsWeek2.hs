import Dykstra
import GraphFunctions
import ReadData
import System.IO (IOMode(ReadMode), openFile, hGetContents, hClose) 
import Data.List.Split (splitOn)

lineParser :: String -> [Edge Float]
lineParser t = es 
    where
        ss = words t
        u = read (head ss) :: Int
        vsDistances = map (splitOn ",") (tail ss)
        vs = map (read.head) vsDistances :: [Int]
        distances = map (read.last) vsDistances :: [Float]
        es = zipWith (\v d -> (u, v, d)) vs distances
    

main :: IO()
main = do
    let fname = "graphsWeek2.txt"
    fpath <- getFilePathFromName fname
    handle <- openFile fpath ReadMode
    contents <- hGetContents handle
    let edges = concatMap lineParser (lines contents)
    let g = fromEdges edges

    let output = dykstra g 1
    let targets = [7,37,59,82,99,115,133,165,188,197]

    let results = map (truncate . getDistanceTo output) targets ::[Int]
    print(results)

    hClose handle
    print "done"