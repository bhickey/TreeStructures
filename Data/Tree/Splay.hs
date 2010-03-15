--
-- Copyright (c) 2009 - 2010 Brendan Hickey - http://bhickey.net
-- New BSD License (see http://www.opensource.org/licenses/bsd-license.php)
--

module Data.Tree.Splay 
(SplayTree, head, tail, singleton, empty, null, fromList, fromAscList, toList, toAscList, insert, lookup, (!!), splay, size, delete) 
where

import Prelude hiding (head, tail, lookup, null, (!!))

data (Ord k) => SplayTree k v = 
    Leaf
  | SplayTree k v Int (SplayTree k v) (SplayTree k v) deriving (Ord, Eq)

-- | /O(1)/. 'singleton' constructs a splay tree containing one element.
singleton :: (Ord k) => (k,v) -> SplayTree k v
singleton (k,v) = SplayTree k v 0 Leaf Leaf

-- | /O(1)/. 'empty' constructs an empty splay tree.
empty :: (Ord k) => SplayTree k v
empty = Leaf

-- | /O(1)/. 'null' returns true if a splay tree is empty.
null :: (Ord k) => SplayTree k v -> Bool
null Leaf = True
null _ = False

size :: (Ord k) => SplayTree k v -> Int
size Leaf = 0
size (SplayTree _ _ d _ _) = d

-- | /Amortized O(lg n)/. Given a splay tree and a key, 'lookup' attempts to find a node with the specified key and splays this node to the root. If the key is not found, the nearest node is brought to the root of the tree.
lookup :: (Ord k) => k -> SplayTree k v -> SplayTree k v
lookup _ Leaf = Leaf
lookup k' t@(SplayTree k _ _ l r)
  | k' == k = t
  | k > k' =
    case lookup k' l of
      Leaf -> t
      lt -> zig lt t
  | otherwise = 
    case lookup k' r of
      Leaf -> t
      rt -> zag t rt

-- | Locates the i^{th} element in BST order without splaying it.
(!!) :: (Ord k) => SplayTree k v -> Int -> (k,v)
(!!) Leaf _ = error "index out of bounds"
(!!) (SplayTree k v d l r) n =
  if n > d
  then error "index out of bounds"
  else 
    let l' = size l in
      if n == l'
      then (k,v)
      else if n <= l'
           then l !! n
           else r !! (n - l')

-- | Splays the i^{th} element in BST order
splay :: (Ord k) => SplayTree k v -> Int -> SplayTree k v
splay Leaf _ = error "index out of bounds"
splay t@(SplayTree _ _ d l r) n =
  if n > d
  then error "index out of bounds"
  else 
    let l' = size l in
      if n == l'
      then t
      else if n <= l'
           then case splay l n of
                  Leaf -> error "index out of bounds"
                  lt -> zig lt t
           else case splay r (n - l') of
                  Leaf -> error "index out of bounds"
                  rt -> zag t rt

-- | /O(1)/. zig rotates its first argument up
zig :: (Ord k) => SplayTree k v -> SplayTree k v -> SplayTree k v
zig Leaf _ = error "tree corruption"
zig _ Leaf = error "tree corruption"
zig (SplayTree k1 v1 _ l1 r1) (SplayTree k v d _ r) =
  SplayTree k1 v1 d l1 (SplayTree k v (d - size l1 - 1) r1 r)

-- | /O(1)/. zig rotates its second argument up
zag :: (Ord k) => SplayTree k v -> SplayTree k v -> SplayTree k v
zag Leaf _ = error "tree corruption"
zag _ Leaf = error "tree corruption"
zag (SplayTree k v d l _) (SplayTree k1 v1 _ l1 r1) =
  SplayTree k1 v1 d (SplayTree k v (d - size r1 - 1) l l1) r1

-- | /Amortized O(lg n)/. Given a splay tree and a key-value pair, 'insert' places the the pair into the tree in BST order. This function is unsatisfying.
insert :: (Ord k) => k -> v -> SplayTree k v -> SplayTree k v
insert k v t =
  case lookup k t of
    Leaf -> (SplayTree k v 0 Leaf Leaf)
    (SplayTree k1 v1 d l r) ->
        if k1 < k
        then SplayTree k v (d + 1) (SplayTree k1 v1 (d - size r + 1) l Leaf) r
        else SplayTree k v (d + 1) l (SplayTree k1 v1 (d - size l + 1) Leaf r)

-- | /O(1)/. 'head' returns the key-value pair of the root.
head :: (Ord k) => SplayTree k v -> (k,v)
head Leaf = error "head of empty tree"
head (SplayTree k v _ _ _) = (k,v)

-- | /Amortized O(lg n)/. 'tail' removes the root of the tree and merges its subtrees
tail :: (Ord k) => SplayTree k v -> SplayTree k v
tail Leaf = error "tail of empty tree"
tail (SplayTree _ _ _ Leaf r) = r
tail (SplayTree _ _ _ l Leaf) = l
tail (SplayTree _ _ _ l r)    = 
  case splayRight l of
    (SplayTree k v d l1 Leaf) -> (SplayTree k v (d + size r) l1 r)
    _ -> error "splay tree corruption"

delete :: (Ord k) => k -> SplayTree k v -> SplayTree k v
delete _ Leaf = Leaf
delete k t = 
  case lookup k t of
    t'@(SplayTree k1 _ _ _ _) -> 
      if k == k1
      then tail t'
      else t'
    Leaf -> error "splay tree corruption"


splayRight :: (Ord k) => SplayTree k v -> SplayTree k v
splayRight Leaf = Leaf
splayRight h@(SplayTree _ _ _ _ Leaf) = h
splayRight (SplayTree k1 v1 d1 l1 (SplayTree k2 v2 _ l2 r2)) = 
  splayRight (SplayTree k2 v2 d1 (SplayTree k1 v1 (d1 - size r2) l1 l2) r2)

splayLeft :: (Ord k) => SplayTree k v -> SplayTree k v
splayLeft Leaf = Leaf
splayLeft h@(SplayTree _ _ _ Leaf _) = h
splayLeft (SplayTree k1 v1 d1 (SplayTree k2 v2 _ l2 r2) r1) = 
  splayLeft (SplayTree k2 v2 d1 l2 (SplayTree k1 v1 (d1 - size l2) r2 r1))

-- | /O(n lg n)/. Constructs a splay tree from an unsorted list of key-value pairs.
fromList :: (Ord k) => [(k,v)] -> SplayTree k v
fromList [] = Leaf
fromList l = foldl (\ acc (k,v) -> insert k v acc) Leaf l

-- | /O(n lg n)/. Constructs a splay tree from a list of key-value pairs sorted in ascending order.
fromAscList :: (Ord k) => [(k,v)] -> SplayTree k v
fromAscList = fromList

-- | /O(n lg n)/. Converts a splay tree into a list of key-value pairs with no constraint on ordering.
toList :: (Ord k) => SplayTree k v -> [(k,v)]
toList = toAscList

-- | /O(n lg n)/. 'toAscList' converts a splay tree to a list of key-value pairs sorted in ascending order.
toAscList :: (Ord k) => SplayTree k v -> [(k,v)]
toAscList h@(SplayTree _ _ _ Leaf _) = head h : toAscList (tail h)
toAscList Leaf = []
toAscList h = toAscList $ splayLeft h
