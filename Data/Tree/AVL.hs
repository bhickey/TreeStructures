--
-- Copyright (c) 2009 Brendan Hickey - http://bhickey.net
-- Simplified BSD License (see http://www.opensource.org/licenses/bsd-license.php)
--

module Data.Tree.AVL
--(AVLTree, head, tail, singleton, empty, null, fromList, fromAscList, fromList, toAscList, insert, lookup, (!!), size) 
where

import Prelude hiding (head, tail, (!!))

data AVLTree k v =
    Leaf
  | AVLTree k v Int Int (AVLTree k v) (AVLTree k v) deriving (Ord, Eq)

singleton :: (Ord k) => (k,v) -> AVLTree k v
singleton (k,v) = AVLTree k v 1 0 Leaf Leaf

empty :: (Ord k) => AVLTree k v
empty = Leaf

null :: AVLTree k v -> Bool
null Leaf = True
null _    = False

size :: AVLTree k v -> Int
size Leaf = 0
size (AVLTree _ _ s _ _ _) = s

height :: AVLTree k v -> Int
height Leaf = 0
height (AVLTree _ _ _ h _ _) = h

maxHeight :: AVLTree k v -> AVLTree k v -> Int
maxHeight a b = max (height a) (height b)

(!!) :: (Ord k) => AVLTree k v -> Int -> (k,v)
(!!) Leaf _ = error "index out of bounds"
(!!) (AVLTree k v d _ l r) n =
  if n > d
  then error "index out of bounds"
  else 
    let l' = size l in
      if n == l'
      then (k,v)
      else if n <= l'
           then l !! n
           else r !! (n - l' - 1)

insert :: (Ord k) => AVLTree k v -> (k,v) -> AVLTree k v
insert Leaf (k,v) = singleton (k,v)
insert t@(AVLTree k1 v1 s h l r) (k,v) =
 if k <= k1
 then balanceLeft  (\ x y -> x < y) (insert l (k,v)) t
 else balanceRight (\ x y -> x < y) t (insert r (k,v))

remove :: (Ord k) => AVLTree k v -> k -> AVLTree k v
remove Leaf _ = Leaf
remove t@(AVLTree k1 _ _ _ Leaf Leaf) k = if k == k1 then Leaf else t

remove t@(AVLTree k1 v1 s h l r) k =
 if k == k1
 then (remove' l)
 else if k < k1
      then balanceLeft  (\ x y -> x < y + 1) (remove l k) t
      else balanceRight (\ x y -> x < y + 1) t (remove r k)

remove' :: (Ord k) => AVLTree k v -> AVLTree k v
remove' t = t

balanceLeft  :: (Ord k) => (Int -> Int -> Bool) -> AVLTree k v -> AVLTree k v -> AVLTree k v
balanceLeft f l@(AVLTree kc vc sc hc lc rc) p@(AVLTree kp vp sp hp _ rp) =
  if f hc (height rp)
  then (AVLTree kp vp (1 + size rp + size l) (1 + maxHeight l rp) l rp)
  else let child = (AVLTree kp vp (1 + size rc + size rp) (1 + maxHeight rc rp) rc rp) in
         (AVLTree kc vc (2 + size lc + size rc + size rp) (1 + maxHeight lc child) lc child)

balanceRight :: (Ord k) => (Int -> Int -> Bool) -> AVLTree k v -> AVLTree k v -> AVLTree k v
balanceRight f p@(AVLTree kp vp sp hp lp _) r@(AVLTree kc vc sc hc lc rc) =
  if f hc (height lp)
  then (AVLTree kp vp (1 + size lp + size rc) (1 + maxHeight lp r) lp rc)
  else let child = (AVLTree kp vp (1 + size rc + size lp) (1 + maxHeight lp lc) lp lc) in
         (AVLTree kc vc (2 + size lc + size rc + size lp) (1 + maxHeight child rc) child rc)

head :: (Ord k) => AVLTree k v -> (k,v)
head Leaf = error "took the head of an empty tree"
head (AVLTree k v _ _ _ _) = (k,v)

left :: (Ord k) => AVLTree k v -> AVLTree k v
left Leaf = error "cannot traverse a leaf"
left (AVLTree _ _ _ _ l _) = l

right :: (Ord k) => AVLTree k v -> AVLTree k v
right Leaf = right Leaf
right (AVLTree _ _ _ _ _ r) = r

promotePrevious :: (Ord k) => AVLTree k v -> AVLTree k v
promotePrevious Leaf = error "unable to promote previous of Leaf"
promotePrevious l@(AVLTree _ _ _ _ Leaf r) = promoteNext l
promotePrevious l@(AVLTree _ _ _ _ t _) = snd $ getRight t

promoteNext :: (Ord k) => AVLTree k v -> AVLTree k v
promoteNext Leaf = error "unable to promote previous of Leaf"
promoteNext (AVLTree _ _ _ _ _ Leaf) = Leaf
promoteNext (AVLTree _ _ _ _ _ t) = snd $ getLeft t

getRight :: (Ord k) => AVLTree k v -> ((k,v), AVLTree k v)
getRight t@(AVLTree k v s h Leaf Leaf) = ((k,v), Leaf)
getRight t@(AVLTree k v s h l Leaf) = ((k,v), l)
getRight t@(AVLTree k v s h l r) = 
  case getRight r of
    (p, t2) -> (p, balanceRight (\ x y -> x + 1 > y) t t2)

getLeft :: (Ord k) => AVLTree k v -> ((k,v), AVLTree k v)
getLeft t@(AVLTree k v s h Leaf Leaf) = ((k,v), Leaf)
getLeft t@(AVLTree k v s h Leaf r) = ((k,v), r)
getLeft t@(AVLTree k v s h l r) = 
  case getLeft r of
    (p, t2) -> (p, balanceLeft (\ x y -> x + 1 > y) t2 t)

fromList :: (Ord k) => [(k,v)] -> AVLTree k v
fromList [] = Leaf
fromList (hd:[]) = singleton hd
fromList (hd:tl) = insert (fromList tl) hd
