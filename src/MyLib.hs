module MyLib (someFunc) where
import qualified Data.Map as Map

emptyMap = Map.empty

someFunc :: IO ()
someFunc = putStrLn "someFunc"
