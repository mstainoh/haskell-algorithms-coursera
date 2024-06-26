module FileParser 
    (
      getFilePathFromName , dataFolder, getCurrentDirectory
      ,parseInts, parseWords
      --,hOpenFile, hCloseFile, hReadFile
      ,hGetContentsLines
      ,testRead
    ) where

import System.Directory (getCurrentDirectory)
import System.IO (hGetLine, hIsEOF, Handle, IOMode(ReadMode), hClose, openFile)


--------------------
-- File handling functions (UNUSED)
--------------------
-- hCloseFile :: Handle -> IO ()
-- hCloseFile = hClose

-- hOpenFile :: FilePath -> IOMode -> IO Handle
-- hOpenFile = openFile

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
-- File reading functions
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
-- File parsing functions (UNUSED)
--------------------
-- | Parse a string containing whitespace-separated integers into a list of integers.
parseInts :: String -> [Int]
parseInts = map read . words

-- | Parse a string into a list of words.
parseWords :: String -> [String]
parseWords = words


--------------------
-- Test
--------------------
-- test
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
