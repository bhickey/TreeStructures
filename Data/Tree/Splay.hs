--
-- Copyright (c) 2009 Brendan Hickey - http://bhickey.net
-- Simplified BSD License (see http://www.opensource.org/licenses/bsd-license.php)
--

module Data.Tree.Splay where

import Prelude hiding (head, tail, lookup)

data (Ord k) => SplayTree k v = 
    Leaf
  | SplayTree k v (SplayTree k v) (SplayTree k v)

singleton :: (Ord k) => (k,v) -> SplayTree k v
singleton (k,v) = SplayTree k v Leaf Leaf

lookup :: (Ord k) => SplayTree k v -> k -> SplayTree k v
lookup Leaf _ = Leaf
lookup n@(SplayTree k v l r) sk =
  if sk == k
  then n
  else if k > sk
       then case l of
              Leaf -> n
              (SplayTree k1 v1 l1 r1) -> lookup (SplayTree k1 v1 l1 (SplayTree k v r1 r)) sk
       else case r of
              Leaf -> n
              (SplayTree k1 v1 l1 r1) -> lookup (SplayTree k1 v1 (SplayTree k v l l1) r1) sk

insert :: (Ord k) => SplayTree k v -> (k,v) -> SplayTree k v
insert t (k,v) =
  case lookup t k of
    Leaf -> (SplayTree k v Leaf Leaf)
    (SplayTree k1 v1 l r) ->
        if k1 < k
        then (SplayTree k v (SplayTree k1 v1 l Leaf) r)
        else (SplayTree k v l (SplayTree k1 v1 Leaf r))

head :: (Ord k) => SplayTree k v -> (k,v)
head Leaf = error "head of empty tree"
head (SplayTree k v _ _) = (k,v)

tail :: (Ord k) => SplayTree k v -> SplayTree k v
tail Leaf = error "tail of empty tree"
tail (SplayTree _ _ Leaf r) = r
tail (SplayTree _ _ l Leaf) = l
tail (SplayTree _ _ l r)    = 
  case splayRight l of
    (SplayTree k v l1 Leaf) -> (SplayTree k v l1 r)
    _ -> error "splay tree corruption"

splayRight :: (Ord k) => SplayTree k v -> SplayTree k v
splayRight Leaf = Leaf
splayRight h@(SplayTree _ _ _ Leaf) = h
splayRight (SplayTree k1 v1 l1 (SplayTree k2 v2 l2 r2))  = splayRight $ (SplayTree k2 v2 (SplayTree k1 v1 l1 l2) r2)

splayLeft :: (Ord k) => SplayTree k v -> SplayTree k v
splayLeft Leaf = Leaf
splayLeft h@(SplayTree _ _ Leaf _) = h
splayLeft (SplayTree k1 v1 (SplayTree k2 v2 l2 r2) r1)  = splayLeft $ (SplayTree k2 v2 l2 (SplayTree k1 v1 r2 r1))

fromList :: (Ord k) => [(k,v)] -> SplayTree k v
fromList [] = Leaf
fromList l = foldl (\ acc x -> insert acc x) Leaf l

toList :: (Ord k) => SplayTree k v -> [(k,v)]
toList h@(SplayTree _ _ Leaf _) = (head h):(toList $ tail h)
toList Leaf = []
toList h = toList $ splayLeft h

