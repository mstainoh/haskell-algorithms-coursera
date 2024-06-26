{-
Heap implementation using arrays
haskell is not very easy for this since
 -lists are stacks: O(1) for head insert/pop but O(n) for lookup
 -vectors are immutable: O(1) for lookup but O(n) for pop/insert

We use Sequence which is a double linked list with tree-like structure and supports O(log n) for lookup and O(1) for head insert/pop  
-}
module ArrayHeap (
    Heap, Policy, emptyHeap,
    getSize, getHeight, getValue,
    getChildren, getParent, getLevelValues,
    bubbleDown, bubbleUp,
    fromListMin, fromListMax,
    insertValue, insertMin, insertMax,
    popValue, popMin, popMax,
    heapsort
)
where

import qualified Data.Sequence as Seq
import Data.Sequence (Seq)

type Heap = Seq
data Policy = MinPolicy | MaxPolicy
type Upwards = Bool

emptyHeap = Seq.empty

-----------------------------------
--- functions for max and min heaps
-----------------------------------
getReducer :: (Ord a) =>  Policy -> (a -> a -> a)
getReducer MinPolicy = min
getReducer MaxPolicy = max

-----------------------------------
--- functions for inspecting the heap
-----------------------------------
getParent:: Int -> Int
getParent 0 = 0
getParent n = (n - 1) `div` 2

getChildren :: Int -> [Int]
getChildren n = [2 * n + 1, 2 * n + 2]

getSize :: Heap a -> Int
getSize = Seq.length

getHeight :: Heap a -> Int
getHeight h = 1 + floor (log (fromIntegral $ getSize h) / log 2)


getValue :: Heap a -> Int -> a
getValue = Seq.index

getLevelValues :: Heap a -> Int -> [a]
getLevelValues h n = map (getValue h) ns
  where
    start = 2 ^ n -1
    end = min (getSize h - 1) (2 * start)
    ns = [start..end]

-----------------------------------
-- functions to update the heap
-----------------------------------
updateHeap :: Int -> a -> Heap a -> Heap a
updateHeap = Seq.update

-- recursive swapping up and/or down following parent/child direction
-- upwards/downwards -> parent is min/max -> starting node -> target Heap -> resul
swapRec :: (Ord a) => Upwards -> Policy -> Int -> Heap a -> Heap a
swapRec upwards policy node h
  |x == x' = h
  |otherwise = swapRec upwards policy nextnode h'
  where
    -- get nodes indices (parent:childs)
    parent = if upwards then getParent node else node
    children' = if upwards then [node] else getChildren node
    children = filter (< getSize h) children'

    -- get parent and children current node values
    (x:xs) = map (getValue h) (parent:children)

    -- define "winning" new parent node value based on policy (min or max)
    reducer = getReducer policy
    (x', parent') = foldl reducer (x, parent) (zip xs children)

    -- update heap by flipping the values (lazy)
    h' = foldr (uncurry updateHeap) h [(parent', x), (parent, x')]

    -- set next node for recursion: 
    -- if upwards always choose, else winning node (child or parent)
    -- note that if the parent wins based on policy then parent=parent' => x=x' and recursion stops 
    nextnode = if upwards then parent else parent'

bubbleUp :: (Ord a) => Policy -> Heap a -> Heap a
bubbleUp policy h =
  let sz = getSize h in
    swapRec True policy (sz - 1) h

bubbleDown :: (Ord a) => Policy -> Heap a -> Heap a
bubbleDown policy h = swapRec False policy 0 h

-----------------------------------
-- insertion and deletion using bubble up / down
-----------------------------------
insertValue :: (Ord a) => Policy -> Heap a -> a -> Heap a
insertValue policy h x = bubbleUp policy (h Seq.|> x)

insertMin :: (Ord a) => Heap a -> a -> Heap a
insertMin = insertValue MinPolicy

insertMax :: (Ord a) => Heap a -> a -> Heap a
insertMax = insertValue MaxPolicy

popValue :: (Ord a) => Policy -> Heap a -> (Heap a, a)
popValue policy h
  |sz == 0 = (emptyHeap, error "Empty heap")
  |sz == 1 = (emptyHeap, x0)
  |otherwise = (bubbleDown policy h', x0)
  where
    sz = getSize h
    x0 = getValue h 0
    x1 = getValue h (sz - 1)
    h' = Seq.take (sz - 1) $ updateHeap 0 x1 h

popMin :: (Ord a) => Heap a -> (Heap a, a)
popMin = popValue MinPolicy

popMax :: (Ord a) => Heap a -> (Heap a, a)
popMax = popValue MaxPolicy

-----------------------------------
-- Other functions: construction, heapsort
-----------------------------------
-- construction with fold
fromList :: (Ord a) => Policy -> [a] -> Heap a
fromList policy = foldl (insertValue policy) emptyHeap

fromListMin :: (Ord a) => [a] -> Heap a
fromListMin = fromList MinPolicy

fromListMax :: (Ord a) => [a] -> Heap a
fromListMax = fromList MaxPolicy

-- auxiliary print view
treeView :: (Show a) => Heap a -> IO()
treeView h = do
  let hmax = getHeight h - 1
  let n = 0
  let xs = map (getLevelValues h) [0..hmax]
  let s = concatMap (++ "\n") $ map show xs
  print s

-- heapsort: also for testing that heap is correctly built
heapsort :: (Ord a) => [a] -> [a]
heapsort [] = []
heapsort xs = snd $ dummy (fromList policy xs, [])
  where
    policy = MaxPolicy
    popAux :: (Ord a) => Heap a -> (Heap a, a)
    popAux = popValue policy
    dummy :: (Ord a) => (Heap a, [a]) -> (Heap a, [a])
    dummy (h, ys)
      |getSize h ==0  = (h, ys)
      |otherwise      = let (h', y) = popAux h in dummy (h', y:ys)


main :: IO()
main = do
  let s1 = Seq.singleton 1
  let s2 = Seq.singleton 1
  let xs =  [5,4,3,2,1,4, 10] ++ [7,8,9, 12, 40]
  let s3 = Seq.fromList xs
  let s4 = foldr (uncurry updateHeap) s3 (zip [0..] (reverse xs))
  treeView s3
  treeView s4
  print "--------"
  let minFromList = fromList MinPolicy
  let maxFromList = fromList MaxPolicy

  treeView (minFromList (take 2 xs))
  print (getParent 1)
  treeView (minFromList (take 3 xs))
  treeView (minFromList (take 4 xs))
  treeView (minFromList xs)
  print(s2 == s1)
  treeView (maxFromList xs)

  print "----------"
  print (heapsort [5,4,2,1,6, 8, 0, 9, 11, 10, 30, -1, -4])
