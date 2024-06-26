--- rolling median exercise

import qualified ArrayHeap as Heap
import ArrayHeap(Heap, insertMax, insertMin, popMin, popMax)
import System.IO (IOMode(ReadMode), openFile, hGetContents, hClose) 
import ReadData
--import qualified Data.SortedList as SortedList

getHead :: Heap a -> a
getHead = flip Heap.getValue 0

getHeadWithDefault:: Heap a -> a -> a
getHeadWithDefault h x
    |Heap.getSize h == 0 = x 
    |otherwise = getHead h

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
rollingHeaps :: (Real a) => [a] -> [(Heap a, Heap a)]
rollingHeaps xs = tail $ scanl dummy (Heap.emptyHeap, Heap.emptyHeap) (zip [0..] xs)
    where
        dummy :: (Real a) => (Heap a, Heap a) -> (Int, a) -> (Heap a, Heap a)
        dummy (h1, h2) (0, x) = (insertMax h1 x, h2)                --start case         
        dummy (h1, h2) (i, x)
            |odd i && (x >= head1)    = (h1, insertMin h2 x)       --insert at the right (min heap)
            |odd i                         = (insertMax h1' x, h2')     --insert at the left but swap a value from left to right for balance
            |even i && (x <= head2)   = (insertMax h1 x, h2)       --insert at the left (max heap)
            |otherwise                     = (h1'', insertMin h2'' x)   --insert at the right but swap a value from right to left for balance
            where
                head1 = getHead h1
                head2 = getHeadWithDefault h2 head1
                -- swap 1
                (h1', h1_top) = popMax h1
                h2' = insertMin h2 h1_top
                -- swap 2
                (h2'', h2_top) = popMin h2
                h1'' = insertMax h1 h2_top

-- given the pair of heaps, the rolling mean chooses the first top value if i is odd, else the average of the heads  
-- NOTE: in the exercise we don't do the actual median, but rather choose the smallest of the 2 medians if length xs is even
--rollingMean :: (Real a, Fractional b) => [a] -> [b]
rollingMean :: (Real a) => [a] -> [a]
rollingMean xs = map (uncurry median) $ (zip [1..] heaps)
    where
        heaps = rollingHeaps xs

        median :: (Real a) => Int -> (Heap a, Heap a) -> a
        median _ (h1, _) = (getHead h1)
        -- median :: (Real a, Fractional b) => Int -> (Heap a, Heap a) -> b
        -- median i (h1, h2) = (getHead h1)
            -- |odd i = realToFrac (getHead h1)
            -- |otherwise = realToFrac (getHead h1 + getHead h2) / 2



-- rollingMean2 :: [Int] -> [Int]
-- rollingMean2 [] = []
-- rollingMean2 xs = fst $ scanr dummy e xs
--     where
--         e = SortedList.empty
--         dummy :: Int-> (SortedList Int, Int) -> (SortedList Int, Int)
--         dummy x (s, x') = (s', m')
--             where
--                 s' = SortedList.insert x e
--                 l = SortedList.length s'
--                 m' = (SortedList.!!) s' (div l 2)


aux :: [Int] -> IO()
aux xs = do
    print("starting")
    let hs = rollingHeaps xs
    let ms = rollingMean xs
    let l = length ms
    let n = 5
    print("Length:" ++ show l)
    print("First " ++ show n ++ " inputs: " ++ show (take n xs))
    print("First " ++ show n ++ " medians: " ++ show (take n ms))
    print("First " ++ show n ++ " heaps: " ++ show (take n hs))
    print("heap " ++ show n ++ ": " ++ show (hs !! (n - 1)))
    print("Sum: " ++ show (sum ms))
    print("sum mod 10000: " ++ show (mod (sum ms) 10000))
    print("Done")
    print("------------")

test :: IO()
test = do
    print "TEST"
    let xs =  [6331, 2793, 1640, 9290, 225, 625, 6195, 2303, 5685, 1354] :: [Int]
    aux xs
    print "TEST2"
    let xs2 = [23, 9, 35, 4, 13, 24, 2, 5, 27, 1, 34, 8, 15, 39, 32, 22, 29, 21, 19, 20, 36, 33, 7, 31, 14, 17, 26, 16, 38, 6, 30, 40, 25, 28, 11, 37, 3, 10, 18, 12]
    aux xs2


main :: IO()
main = do
    test
    -- read
    let fname = "graphsWeek3.txt"
    fpath <- getFilePathFromName fname
    handle <- openFile fpath ReadMode
    contents <- hGetContents handle
    let xs = map read (lines contents) ::[Int]

    aux xs

    hClose handle
