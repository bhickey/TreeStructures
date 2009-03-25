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
  | AVLTree k v Int Int (AVLTree k v) (AVLTree k v) deriving (Ord, Eq, Show)

singleton :: (Ord k) => (k,v) -> AVLTree k v
singleton (k,v) = AVLTree k v 1 1 Leaf Leaf

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

findHeight :: AVLTree k v -> AVLTree k v -> Int
findHeight a b = 1 + max (height a) (height b)

findSize :: AVLTree k v -> AVLTree k v -> Int
findSize a b = 1 + size a + size b

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
  then let l' = insert l (k,v) in
    balance (AVLTree k1 v1 (s + 1) (findHeight l' r) l' r)
  else let r' = insert r (k,v) in
    balance (AVLTree k1 v1 (s + 1) (findHeight l r') l r')

{-
remove :: (Ord k) => AVLTree k v -> k -> AVLTree k v
remove Leaf _ = Leaf
remove t@(AVLTree k1 _ _ _ Leaf Leaf) k = if k == k1 then Leaf else t
remove t@(AVLTree k1 v1 s h l r) k =
 if k == k1
 then (remove' l)
 else if k < k1
      then balance (AVLTree k1 v1 0 0 (remove l k) r)
      else balance (AVLTree k1 v1 0 0 l (remove r k))
 
remove' :: (Ord k) => AVLTree k v -> AVLTree k v
remove' t = t
-}

balance :: (Ord k) => AVLTree k v -> AVLTree k v
balance Leaf = Leaf
balance t@(AVLTree k v _ _ l r) =
  if (abs $ (height l) - (height r)) < 2
    then (AVLTree k v (findSize l r) (findHeight l r) l r) -- this avoids the nonsense of tracking this information pre-balance
  else if (height l) < (height r)
       then case r of
              Leaf -> error "cannot promote a leaf" -- this should never happen
              (AVLTree k1 v1 _ _ l1 r1) -> 
                let child = (AVLTree k v (findSize l l1) (findHeight l l1) l l1) in
                  (AVLTree k1 v1 (findSize child r1) (findHeight child r1) child r1)  
        else case l of
              Leaf -> error "cannot promote a leaf"
              (AVLTree k1 v1 _ _ l1 r1) -> 
                let child = (AVLTree k v (findSize r1 r) (findHeight r1 r) r1 r) in
                  (AVLTree k1 v1 (findSize l1 child) (findHeight l1 child) l1 child)

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
    (p, t2) -> (p, balance (AVLTree k v 0 0 l t2))

getLeft :: (Ord k) => AVLTree k v -> ((k,v), AVLTree k v)
getLeft t@(AVLTree k v s h Leaf Leaf) = ((k,v), Leaf)
getLeft t@(AVLTree k v s h Leaf r) = ((k,v), r)
getLeft t@(AVLTree k v s h l r) = 
  case getLeft r of
    (p, t2) -> (p, (AVLTree k v 0 0 t2 r))

fromList :: (Ord k) => [(k,v)] -> AVLTree k v
fromList [] = Leaf
fromList (hd:[]) = singleton hd
fromList (hd:tl) = insert (fromList tl) hd
