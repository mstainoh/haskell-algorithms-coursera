--- rolling median exercise

import qualified ArrayHeap as Heap
import ArrayHeap(Heap, insertMax, insertMin, popMin, popMax)

emptyHeap :: Heap Float
emptyHeap = Heap.emptyHeap

getHead :: Heap Float -> Float
getHead = flip Heap.getValue 0

------
{-
main function: the values alternate between a left (max) heap and a right min heap
the invariant is that always head right >= head left
therefore a new value x is 
    - inserted in the right only if x >= top left
    - inserted in the left only if x  <= top left
if this is not possible, it is inserted in the other branch but the top is moved to balance the quantities

Since top left >= all nodes in left and top right <= all nodes in right, the invariant is always kept  
-}
------
rollingHeaps :: [Float] -> [(Heap Float, Heap Float)]
rollingHeaps xs = scanl dummy (emptyHeap, emptyHeap) (zip [0..] xs)
    where
        dummy :: (Heap Float, Heap Float) -> (Int, Float) -> (Heap Float, Heap Float)
        dummy (h1, h2) (0, x) = (insertMax h1 x, h2)                --start case         
        dummy (h1, h2) (i, x)
            |odd i && (x >= getHead h1)    = (h1, insertMin h2 x)       --insert at the right (min heap)
            |odd i                         = (insertMax h1' x, h2')     --insert at the left but swap a value from left to right for balance
            |even i && (x <= getHead h1)   = (insertMax h1 x, h2)       --insert at the left (max heap)
            |otherwise                     = (h1'', insertMin h2'' x)   --insert at the right but swap a value from right to left for balance
            where
                -- swap 1
                (h1', h1_top) = popMax h1
                h2' = insertMin h2 h1_top
                -- swap 2
                (h2'', h2_top) = popMin h2
                h1'' = insertMax h1 h2_top

-- given the pair of heaps, the rolling mean chooses the first top value if i is odd, else the average of the heads  
rollingMean :: [Float] -> [Float]
rollingMean xs = map (uncurry median) $ tail (zip [0..] heaps)
    where
        heaps = rollingHeaps xs

        median :: Int -> (Heap Float, Heap Float) -> Float
        median i (h1, h2)
            |odd i = getHead h1
            |otherwise = (getHead h1 + getHead h2) / 2



main :: IO()
main = do
    let xs = [1..10] ++ (reverse [11..20]) :: [Float]
    let rs = rollingHeaps xs
    print rs
    let ms = rollingMean xs
    print ms
