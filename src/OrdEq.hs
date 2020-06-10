data Tree a
  = Leaf
  | Node Int (Tree a) a (Tree a)

instance (Ord a) => Ord (Tree a) where
  compare Leaf Leaf = EQ
  compare Node {} Leaf = GT
  compare Leaf Node {} = LT
  compare (Node l _ _ _) (Node r _ _ _) = compare l r

{-# INLINE (.:) #-}
(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(f .: g) x y = f $ g x y

instance (Ord a) => Eq (Tree a) where
  (==) = (== EQ) .: compare

main :: IO ()
main = do
  print $ Leaf == (Leaf :: Tree Char)
  print $ Node 0 Leaf 'a' Leaf == Node 0 Leaf 'b' Leaf
  print $ Node 0 Leaf 'a' Leaf == Node 1 Leaf 'a' Leaf
