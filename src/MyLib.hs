module MyLib (someFunc) where
import qualified Data.Map as Map

-------
-- this is a test library
------------

emptyMap = Map.empty

someFunc :: IO ()
someFunc = putStrLn "someFunc"
