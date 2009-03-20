--
-- Copyright (c) 2009 Brendan Hickey - http://bhickey.net
-- Simplified BSD License (see http://www.opensource.org/licenses/bsd-license.php)
--

-- | 'Data.Heap.Binary' provides a binary min-heap. Balance is maintained through descendant counting.
module Data.Heap.Binary 
(BinaryHeap, head, tail, merge, singleton, empty, null, fromList, toList, insert) 
where

import Prelude hiding (head, tail, null)

data (Ord n) => BinaryHeap n =
    Leaf
  | Node n Int (BinaryHeap n) (BinaryHeap n) deriving (Eq, Ord)

instance (Ord n, Show n) => Show (BinaryHeap n) where
  show Leaf = "Leaf"
  show (Node n _ h1 h2) = "Node " ++ (show n) ++ " (" ++ (show h1) ++ " " ++ (show h2) ++ ")"

rank :: (Ord n) => BinaryHeap n -> Int
rank Leaf = 0
rank (Node _ d _ _) = d

-- | /O(1)/. 'empty' produces an empty heap.
empty :: (Ord a) => BinaryHeap a
empty = Leaf 

-- | /O(1)/. 'singleton' consumes an element and constructs a singleton heap.
singleton :: (Ord a) => a -> BinaryHeap a
singleton a = Node a 1 Leaf Leaf

-- | 'merge' consumes two binary heaps and merges them.
merge :: (Ord a) => BinaryHeap a -> BinaryHeap a -> BinaryHeap a
merge Leaf n = n
merge n Leaf = n
merge h1@(Node n1 d1 h1l h1r) h2@(Node n2 d2 _ _) = 
  if n1 == n2 || h1 <= h2
  then if rank h1l < rank h1r
       then (Node n1 (d1 + d2) (merge h1l h2) h1r)
       else (Node n1 (d1 + d2) h1l (merge h1r h2))
  else merge h2 h1

-- | /O(lg n)/.
insert :: (Ord a) => BinaryHeap a -> a -> BinaryHeap a
insert h a = merge h (singleton a)

-- | /O(1)/.
null :: (Ord a) => BinaryHeap a -> Bool
null Leaf = True
null _    = False

-- | /O(n lg n)/.
toList :: (Ord a) => BinaryHeap a -> [a]
toList Leaf = []
toList h@(Node _ _ _ _) = (head h):(toList $ tail h)

-- | /O(n)/. 'fromList' constructs a binary heap from an unsorted list.
fromList :: (Ord a) => [a] -> BinaryHeap a
fromList [] = Leaf
fromList l =  (\ ((hd:_):_) -> hd) $! dropWhile (\ x -> length x > 1) $ iterate (pairWise merge) $ map singleton l

pairWise :: (a -> a -> a) -> [a] -> [a] 
pairWise _ [] = []
pairWise f (a:b:tl) = (f a b):(pairWise f tl)
pairWise _ a = a

-- | /O(1)/. 'head' returns the element root of the heap.
head :: (Ord a) => BinaryHeap a -> a
head Leaf = error "Data.Tree.Heap: empty list"
head (Node n _ _ _) = n

-- | /O(lg n)/. 'tail' discards the root of the heap and merges the subtrees.
tail :: (Ord a) => BinaryHeap a -> BinaryHeap a
tail Leaf = error "Data.Heap empty list"
tail (Node _ _ h1 h2) = merge h1 h2
