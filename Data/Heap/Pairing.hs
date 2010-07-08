{-# LANGUAGE RankNTypes #-}
--
-- Copyright (c) 2010 Brendan Hickey - http://bhickey.net
-- New BSD License (see http://www.opensource.org/licenses/bsd-license.php)
--

module Data.Heap.Pairing
(PairingHeap, head, tail, merge, singleton, empty, null, fromList, toList, insert) 
where

import Prelude hiding (head, tail, null)

data (Ord a) => PairingHeap a =
      EmptyHeap
    | Heap a [PairingHeap a] deriving (Eq, Ord)

empty :: (Ord a) => PairingHeap a
empty = EmptyHeap

null :: (Ord a) => PairingHeap a -> Bool
null EmptyHeap = True
null _         = False

singleton :: (Ord a) => a -> PairingHeap a
singleton n = Heap n []

insert :: (Ord a) => a -> PairingHeap a -> PairingHeap a
insert a = merge (singleton a)

merge :: (Ord a) => PairingHeap a -> PairingHeap a -> PairingHeap a
merge EmptyHeap n = n
merge n EmptyHeap = n
merge hp1@(Heap h1 c1) hp2@(Heap h2 c2) =
    if h1 < h2
    then Heap h1 (hp2:c1)
    else Heap h2 (hp1:c2)

head :: (Ord a) => PairingHeap a -> a
head EmptyHeap = error "Data.Heap.Pairing: empty heap"
head (Heap h _) = h

tail :: (Ord a) => PairingHeap a -> PairingHeap a
tail EmptyHeap = error "Data.Heap.Pairing: empty heap"
tail (Heap _ []) = EmptyHeap
tail (Heap _ ch) = mergeList ch 

toAscList :: (Ord a) => PairingHeap a -> [a]
toAscList EmptyHeap = []
toAscList (Heap a []) = [a]
toAscList h@(Heap a _) = a : toAscList (tail h)

fromAscList :: (Ord a) => [a] -> PairingHeap a
fromAscList [] = EmptyHeap
fromAscList [a] = Heap a []
fromAscList x = mergeList $ map singleton x

toList :: (Ord a) => PairingHeap a -> [a]
toList = toAscList

fromList :: (Ord a) => [a] -> PairingHeap a
fromList = fromAscList

mergeList :: forall a. (Ord a) => [PairingHeap a] -> PairingHeap a
mergeList [a] = a
mergeList x = mergeList (mergePairs x)

mergePairs :: forall a. (Ord a) => [PairingHeap a] -> [PairingHeap a]
mergePairs (a:b:c) = merge a b : mergePairs c
mergePairs x = x
