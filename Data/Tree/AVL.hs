--
-- Copyright (c) 2009 Brendan Hickey - http://bhickey.net
-- Simplified BSD License (see http://www.opensource.org/licenses/bsd-license.php)
--

module Data.Tree.AVL
(AVLTree, head, tail, singleton, empty, null, fromList, insert, delete, lookup, (!!), size) 
where

import Prelude hiding (head, tail, (!!), lookup, null)
import Data.Maybe
import Control.Monad.ST

data AVLTree k v =
    Leaf
  | AVLTree k v Int Int (AVLTree k v) (AVLTree k v) deriving (Ord, Eq, Show)

singleton :: (Ord k) => k -> v -> AVLTree k v
singleton k v = AVLTree k v 1 1 Leaf Leaf

empty :: (Ord k) => AVLTree k v
empty = Leaf

null :: AVLTree k v -> Bool
null Leaf = True
null _    = False

head :: (Ord k) => AVLTree k v -> v
head Leaf = error "took the head of an empty tree"
head (AVLTree _ v _ _ _ _) = v

tail :: (Ord k) => AVLTree k v -> AVLTree k v
tail Leaf = error "took the tail of an empty tree"
tail t@(AVLTree k _ _ _ _ _) = delete k t

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

balance :: (Ord k) => AVLTree k v -> AVLTree k v
balance Leaf = Leaf
balance t@(AVLTree k v _ _ l r) =
  if (abs $ (height l) - (height r)) < 2
    then t
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

lookup :: (Ord k) => k -> AVLTree k v -> Maybe v
lookup _ Leaf = Nothing
lookup k' t@(AVLTree k v _ _ l r) =
  if k == k'
  then Just v
  else if k < k'
       then lookup k' l
       else lookup k' r

insert :: (Ord k) => k -> v -> AVLTree k v -> AVLTree k v
insert k v Leaf = singleton k v
insert k v t@(AVLTree k1 v1 s h l r) =
  if k <= k1
  then let l' = insert k v l in
    balance (AVLTree k1 v1 (s + 1) (findHeight l' r) l' r)
  else let r' = insert k v r in
    balance (AVLTree k1 v1 (s + 1) (findHeight l r') l r')

delete :: (Ord k) => k -> AVLTree k v -> AVLTree k v
delete _ Leaf = Leaf
delete k t@(AVLTree k1 _ _ _ Leaf Leaf) = if k == k1 then Leaf else t
delete k t@(AVLTree k1 v1 _ _ l r) =
 if k == k1
 then case t of
        (AVLTree _ _ _ _ Leaf r) ->
          let ((k', v'), r') = getLeft r in
            balance (AVLTree k' v' (findSize Leaf r') (findHeight Leaf r') Leaf r')
        (AVLTree _ _ _ _ l r ) ->
          let ((k', v'), l') = getRight l in
            balance (AVLTree k' v' (findSize l' r) (findHeight l' r) l' r)
 else if k < k1
      then let l' = delete k l in
             balance (AVLTree k1 v1 (findSize l' r) (findHeight l' r) l' r)
      else let r' = delete k r in
             balance (AVLTree k1 v1 (findSize l r') (findHeight l r') l r')

getRight :: (Ord k) => AVLTree k v -> ((k,v), AVLTree k v)
getRight t@(AVLTree k v s h Leaf Leaf) = ((k,v), Leaf)
getRight t@(AVLTree k v s h l Leaf) = ((k,v), l)
getRight t@(AVLTree k v s h l r) = 
  case getRight r of
    (p, t2) -> (p, balance (AVLTree k v (findSize l t2) (findHeight l t2) l t2))

getLeft :: (Ord k) => AVLTree k v -> ((k,v), AVLTree k v)
getLeft t@(AVLTree k v s h Leaf Leaf) = ((k,v), Leaf)
getLeft t@(AVLTree k v s h Leaf r) = ((k,v), r)
getLeft t@(AVLTree k v s h l r) = 
  case getLeft r of
    (p, t2) -> (p, (AVLTree k v (findSize r t2) (findHeight r t2) t2 r))

fromList :: (Ord k) => [(k,v)] -> AVLTree k v
fromList [] = Leaf
fromList ((k,v):[]) = singleton k v
fromList ((k,v):tl) = insert k v (fromList tl)

-- TODO implement an instance of foldable so that this can be concisely defined
toList :: (Ord k) => AVLTree k v -> [(k,v)]
toList Leaf = []
toList t@(AVLTree k v _ _ l r) = (toList l) ++ (k,v):(toList r)
