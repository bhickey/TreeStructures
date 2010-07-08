import Data.Char
import Data.List 
import Test.QuickCheck
import Text.Printf
import qualified Data.Heap.Binary as B
import qualified Data.Heap.Binomial as N
import qualified Data.Heap.Pairing as P
import qualified Data.Heap.Skew as S
import qualified Data.Tree.AVL as A
import qualified Data.Tree.Splay as Y

binary_fromListToList s = (B.toList . B.fromList) s == sort s
  where _ = s :: [Int]

binomial_fromListToList s = (N.toList . N.fromList) s == sort s
  where _ = s :: [Int]

pair_fromListToList s = (P.toList . P.fromList) s == sort s
  where _ = s :: [Int]

skew_fromListToList s = (S.toList . S.fromList) s == sort s
  where _ = s :: [Int]

avl_fromListToList s = (map fst . A.toList . A.fromList) s == (map fst . sort) s
  where _ = s :: [(Int,Int)]

splay_fromListToList s = (map fst . Y.toList . Y.fromList) s == (map fst . sort) s
  where _ = s :: [(Int,Int)]



tests = [
    ("Heap.Binary:   toList.fromList/sort", quickCheck binary_fromListToList),
    ("Heap.Binomial: toList.fromList/sort", quickCheck binomial_fromListToList),
    ("Heap.Pairing:  toList.fromList/sort", quickCheck pair_fromListToList),
    ("Heap.Skew:     toList.fromList/sort", quickCheck skew_fromListToList),
    ("Tree.AVL:      toList.fromList/sort", quickCheck avl_fromListToList),
    ("Tree.Splay:    toList.fromList/sort", quickCheck splay_fromListToList)]

main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests
