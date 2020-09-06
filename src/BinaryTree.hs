import Data.List (unfoldr)
import Prelude hiding (lookup)

data Tree a b
  = Leaf
  | Node a b (Tree a b) (Tree a b)
  deriving (Show)

insert :: (Ord a) => Tree a b -> a -> b -> Tree a b
insert Leaf k v = Node k v Leaf Leaf
insert (Node k' v' l r) k v
  | k < k' = Node k' v' (insert l k v) r
  | k > k' = Node k' v' l (insert r k v)
  | otherwise = Node k v l r

instance (Ord a) => Monoid (Tree a b) where
  mempty = Leaf

instance (Ord a) => Semigroup (Tree a b) where
  Leaf <> Leaf = Leaf
  Leaf <> t = t
  t <> Leaf = t
  l <> (Node k2 v2 l2 r2) = ((insert l k2 v2) <> l2) <> r2

delete :: (Ord a) => Tree a b -> a -> Tree a b
delete Leaf _ = Leaf
delete (Node k' v' l r) k
  | k < k' = Node k' v' (delete l k) r
  | k > k' = Node k' v' l (delete r k)
  | otherwise = l <> r

lookup :: (Ord a) => Tree a b -> a -> Maybe b
lookup Leaf _ = Nothing
lookup (Node k' v' l r) k
  | k < k' = lookup l k
  | k > k' = lookup r k
  | otherwise = Just v'

pop :: (Ord a) => Tree a b -> a -> Maybe (b, Tree a b)
pop t k = lookup t k >>= \v -> Just (v, delete t k)

toList :: Tree a b -> [(a, b)]
toList Leaf = []
toList (Node k v l r) = toList l ++ [(k, v)] ++ toList r

main :: IO ()
main = do
  print tree
  print $ toList tree
  mapM_ (print . toList . delete tree) ks
  mapM_ (print . lookup tree) [-1 .. 8]
  print $ unfoldr (`pop` 4) tree
  where
    tree = foldl (uncurry . insert) Leaf (zip ks ['a' ..])
    ks = [4, 7, 2, 8, 5, -1 :: Int]
