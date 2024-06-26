{-
Auxiliary functions for IO handling
-}
module ReadData
    (
      getFilePathFromName , dataFolder
      ,getCurrentDirectory
      --,hOpenFile, hCloseFile, hReadFile
      ,hGetContentsLines
      ,testRead
    ) 
  where

import System.Directory (getCurrentDirectory)
import System.IO (hGetLine, hIsEOF, Handle, IOMode(ReadMode), hClose, openFile)

--------------------
-- Path functions
--------------------

dataFolder :: FilePath
dataFolder = "/data/"

-- Get the directory of the file inside the repo
getFilePathFromName :: FilePath -> IO FilePath
getFilePathFromName filename = do
    dir <- getCurrentDirectory
    return (dir ++ dataFolder ++ filename)

--------------------
-- Aux File reading functions
--------------------
hGetContentsLines :: Handle -> Int -> IO [String]
hGetContentsLines handle n
  | n <= 0 = return []  -- Base case: return empty list if n is less than or equal to 0
  | otherwise = do
      line <- hGetLine handle
      eof <- hIsEOF handle
      if eof
        then return []  -- End-of-file reached, return empty list
        else do
          rest <- hGetContentsLines handle (n-1)  -- Recursively read remaining lines
          return (line : rest)  -- Prepend current line to the rest of the lines
  
--------------------
-- Test
--------------------
-- test
parseInts :: String -> [Int]
parseInts = map read . words 

testRead :: IO ()
testRead = do
  let fname = "week1.txt"
  fpath <- getFilePathFromName fname
  n <- putStrLn "Enter the number of lines to read: " >> getLine
  let numLines = read n ::Int
  handle <- openFile fpath ReadMode
  contents <- hGetContentsLines handle numLines
  hClose handle
  putStrLn "Read Lines:"
  putStrLn . unlines $ contents

  print $ map parseInts contents
