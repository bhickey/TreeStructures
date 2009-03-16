--
-- Copyright (c) 2009 Brendan Hickey - http://bhickey.net
-- Simplified BSD License (see http://www.opensource.org/licenses/bsd-license.php)
--

module Data.Heap.Binary where

import Prelude hiding (head, tail)

data (Ord n, Eq n) => BinaryHeap n =
    Leaf
  | Node n Int (BinaryHeap n) (BinaryHeap n) deriving (Eq)

instance (Ord n, Eq n, Show n) => Show (BinaryHeap n) where
  show Leaf = "Leaf"
  show (Node n _ h1 h2) = "Node " ++ (show n) ++ " (" ++ (show h1) ++ " " ++ (show h2) ++ ")"

instance (Ord n, Eq n) => Ord (BinaryHeap n) where
  compare Leaf Leaf = EQ
  compare Leaf _    = LT
  compare _ Leaf    = GT
  compare (Node n1 _ _ _) (Node n2 _ _ _) = compare n2 n1

rank :: (Ord n, Eq n) => BinaryHeap n -> Int
rank Leaf = 0
rank (Node _ d _ _) = d

empty :: (Ord a, Eq a) => BinaryHeap a
empty = Leaf 

singleton :: (Ord a, Eq a) => a -> BinaryHeap a
singleton a = Node a 1 Leaf Leaf

merge :: (Ord a, Eq a) => BinaryHeap a -> BinaryHeap a -> BinaryHeap a
merge Leaf n = n
merge n Leaf = n
merge h1@(Node n1 d1 h1l h1r) h2@(Node n2 d2 h2l h2r) = 
  if h1 >= h2
  then if rank h1l < rank h1r
       then (Node n1 (d1 + d2) (merge h1l h2) h1r)
       else (Node n1 (d1 + d2) h1l (merge h1r h2))
  else merge h2 h1

-- null is not used within this code, I included it because it's a nice thing to have
null :: (Ord a, Eq a) => BinaryHeap a -> Bool
null Leaf = True
null _    = False

-- this function is voodoo! It should NOT be used on infinite lists.
toList :: (Ord a, Eq a) => BinaryHeap a -> [a]
toList Leaf = []
toList h@(Node _ _ _ _) = (head h):(toList $ tail h)

fromList :: (Ord a) => [a] -> BinaryHeap a
fromList [] = Leaf
fromList l =  (\ ((hd:_):_) -> hd) $! dropWhile (\ x -> length x > 1) $ iterate (pairWise merge) $ map singleton l

pairWise :: (a -> a -> a) -> [a] -> [a] 
pairWise f [] = []
pairWise f (a:b:tl) = (f a b):(pairWise f tl)
pairWise f a = a


head :: (Ord a, Eq a) => BinaryHeap a -> a
head Leaf = error "Data.Heap: empty list"
head (Node n _ _ _) = n

tail :: (Ord a, Eq a) => BinaryHeap a -> BinaryHeap a
tail Leaf = error "Data.Heap empty list"
tail (Node _ _ h1 h2) = merge h1 h2

-- everyone loves heapsort.
heapSort :: (Ord a, Eq a) => [a] -> [a]
heapSort [] = []
heapSort a = toList $! fromList a

