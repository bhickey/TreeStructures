import Char
import List 
import Test.QuickCheck
import Text.Printf
import qualified Data.Heap.Binary as B
import qualified Data.Heap.Binomial as N
import qualified Data.Heap.Skew as S
import qualified Data.Tree.AVL as A
import qualified Data.Tree.Splay as P

binary_fromListToList s = (B.toList . B.fromList) s == sort s
  where _ = s :: [Int]

binomial_fromListToList s = (N.toList . N.fromList) s == sort s
  where _ = s :: [Int]

skew_fromListToList s = (S.toList . S.fromList) s == sort s
  where _ = s :: [Int]

avl_fromListToList s = (map fst . A.toList . A.fromList) s == (map fst . sort) s
  where _ = s :: [(Int,Int)]

splay_fromListToList s = (map fst . P.toList . P.fromList) s == (map fst . sort) s
  where _ = s :: [(Int,Int)]



tests = [
    ("Heap.Binary:   toList.fromList/sort", test binary_fromListToList),
    ("Heap.Binomial: toList.fromList/sort", test binomial_fromListToList),
    ("Heap.Skew:     toList.fromList/sort", test skew_fromListToList),
    ("Tree.AVL:      toList.fromList/sort", test avl_fromListToList),
    ("Tree.Splay:    toList.fromList/sort", test splay_fromListToList)]

main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests
 
instance Arbitrary Char where
    arbitrary     = choose ('\0', '\128')
    coarbitrary c = variant (ord c `rem` 4)
