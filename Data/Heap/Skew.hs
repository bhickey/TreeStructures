--
-- Copyright (c) 2009 Brendan Hickey - http://bhickey.net
-- Simplified BSD License (see http://www.opensource.org/licenses/bsd-license.php)
--

module Data.Heap.Skew 
(SkewHeap, head, tail, merge, singleton, empty, null, fromList, toList, insert) 
where

import Prelude hiding (head, tail, null)
import qualified Data.List as L

data (Ord a) => SkewHeap a =
    SkewLeaf
  | SkewHeap a (SkewHeap a) (SkewHeap a) deriving (Eq, Ord)

empty :: (Ord a) => SkewHeap a
empty = SkewLeaf

null :: (Ord a) => SkewHeap a -> Bool
null SkewLeaf = True
null _ = False

singleton :: (Ord a) => a -> SkewHeap a
singleton n = SkewHeap n SkewLeaf SkewLeaf

insert :: (Ord a) => a -> SkewHeap a -> SkewHeap a
insert h a = merge h (singleton a)

merge :: (Ord a) => SkewHeap a -> SkewHeap a -> SkewHeap a
merge SkewLeaf n = n
merge n SkewLeaf = n
merge h1 h2 = foldl1 assemble $ reverse $ listMerge head (cutRight h1) (cutRight h2)

listMerge :: (Ord b) => (a -> b) -> [a] -> [a] -> [a]
listMerge _ [] s = s
listMerge _ f [] = f
listMerge c f@(h1:t1) s@(h2:t2) =
  if (c h1) <= (c h2)
  then h1:(listMerge c t1 s)
  else h2:(listMerge c f t2)

cutRight :: (Ord a) => SkewHeap a -> [SkewHeap a]
cutRight SkewLeaf = []
cutRight (SkewHeap a l r) = (SkewHeap a l SkewLeaf):(cutRight r)

-- assumes h1 >= h2, merge relies on this
assemble :: (Ord a) => SkewHeap a -> SkewHeap a -> SkewHeap a
assemble h1 (SkewHeap a l SkewLeaf) = SkewHeap a h1 l
assemble _ _ = error "invalid heap assembly"

head :: (Ord a) => SkewHeap a -> a
head SkewLeaf = error "head of empty heap"
head (SkewHeap a _ _) = a

tail :: (Ord a) => SkewHeap a -> SkewHeap a
tail SkewLeaf = error "tail of empty heap"
tail (SkewHeap _ l r) = merge l r

toList :: (Ord a) => SkewHeap a -> [a]
toList SkewLeaf = []
toList (SkewHeap n l r) = n:(toList $ merge l r)

fromList :: (Ord a) => [a] -> SkewHeap a
fromList [] = SkewLeaf
fromList l =  (\ ((hd:_):_) -> hd) $! dropWhile (\ x -> length x > 1) $ iterate (pairWise merge) $ map singleton l

pairWise :: (a -> a -> a) -> [a] -> [a] 
pairWise _ [] = []
pairWise f (a:b:tl) = (f a b):(pairWise f tl)
pairWise _ a = a
