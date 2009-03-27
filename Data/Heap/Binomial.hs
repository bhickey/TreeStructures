--
-- Copyright (c) 2009 Brendan Hickey - http://bhickey.net
-- Simplified BSD License (see http://www.opensource.org/licenses/bsd-license.php)
--

module Data.Heap.Binomial 
(BinomialHeap, head, tail, merge, singleton, empty, null, fromList, toList, insert) 
where

import Prelude hiding (head, tail, null)
import Data.List (delete)

data (Ord a, Ord b, Eq a, Eq b) => HeapNode a b = HeapNode a {-# UNPACK #-} !Int [b]

data (Ord a, Eq a) => BinomialHeap a = 
    EmptyHeap
  | Heap {-# UNPACK #-} ![HeapNode a (BinomialHeap a)] deriving (Eq, Ord)

instance (Ord a, Ord b, Eq a, Eq b) => Ord (HeapNode a b) where
  compare (HeapNode e1 _ _) (HeapNode e2 _ _) = compare e1 e2

instance (Ord a, Ord b, Eq a, Eq b) => Eq (HeapNode a b) where
  (HeapNode e1 _ _) == (HeapNode e2 _ _) = e1 == e2
 
rank :: (Ord a, Ord b, Eq a, Eq b) => HeapNode a b -> Int
rank (HeapNode _ n _) = n

hRank :: (Ord a, Ord b, Eq a, Eq b) => [HeapNode a b] -> Int
hRank [] = 0
hRank (hd:_) = rank hd

extract :: (Ord a, Ord b, Eq a, Eq b) => HeapNode a b -> a
extract (HeapNode n _ _) = n

empty :: (Ord a) => BinomialHeap a
empty = EmptyHeap

null :: (Ord a) => BinomialHeap a -> Bool
null EmptyHeap = True
null _         = False

-- | /O(1)/.
singleton :: (Ord a) => a -> BinomialHeap a
singleton n = Heap [HeapNode n 1 []]

-- | /O(lg n)/
insert :: (Ord a) => a -> BinomialHeap a -> BinomialHeap a
insert a h = merge (singleton a) h

-- | /O(lg n)/.
merge :: (Ord a) => BinomialHeap a -> BinomialHeap a -> BinomialHeap a
merge EmptyHeap n = n
merge n EmptyHeap = n
merge (Heap h1) (Heap h2) = Heap $! (mergeNodes h1 h2)

mergeNodes :: (Ord a, Eq a) => [HeapNode a (BinomialHeap a)] -> [HeapNode a (BinomialHeap a)] -> [HeapNode a (BinomialHeap a)]
mergeNodes [] h  = h
mergeNodes h  [] = h
mergeNodes f@(h1:t1) s@(h2:t2) =
 if rank h1 == rank h2
 then let merged = (combine h1 h2) 
          r = rank merged in
      if r /= hRank t1
      then if r /= hRank t2
           then merged:(mergeNodes t1 t2)
           else mergeNodes (merged:t1) t2
      else if r /= hRank t2
           then mergeNodes t1 (merged:t2)
           else merged:(mergeNodes t1 t2)
 else if rank h1 < rank h2
      then h1:(mergeNodes t1 s)
      else h2:(mergeNodes t2 f)

combine :: (Ord a, Eq a) => HeapNode a (BinomialHeap a) -> HeapNode a (BinomialHeap a) -> HeapNode a (BinomialHeap a)
combine h1@(HeapNode e1 n1 l1) h2 =
  if h1 <= h2
  then HeapNode e1 (n1 + 1) (l1 ++ [Heap [h2]])
  else combine h2 h1

-- | /O(lg n)/
head :: (Ord a) => BinomialHeap a -> a
head EmptyHeap = error "Data.Heap: empty list"
head (Heap hn) = extract $! minimum hn

-- | /O(lg n)/
tail :: (Ord a) => BinomialHeap a -> BinomialHeap a
tail EmptyHeap = error "Data.Heap: empty list"
tail (Heap hn) = 
  let n@(HeapNode _ _ hd) = (minimum hn) in
    foldl merge (Heap (delete n hn)) hd

-- | /O(n)/
fromList :: (Ord a, Eq a) => [a] -> BinomialHeap a
fromList =  foldl merge EmptyHeap . map singleton

-- | /O(n lg n)/
toList :: (Ord a) => BinomialHeap a -> [a]
toList EmptyHeap  = []
toList (Heap [])  = []
toList h@(Heap _) = (head h):(toList $ if null h then h else tail h)
