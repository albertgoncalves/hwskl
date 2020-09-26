import Data.List (unfoldr)

{- NOTE: See `http://typeocaml.com/2015/03/12/heap-leftist-tree/`. -}

data Tree a
  = Leaf
  | Node a Weight (Tree a) (Tree a)
  deriving (Show)

{- NOTE: `Weight` captures the distance between the `Node` and the right-most
 - `Leaf`.
 -}

newtype Weight = Weight {getWeight :: Word}
  deriving (Eq, Ord, Show)

instance Num Weight where
  l + r = Weight $ getWeight l + getWeight r
  l * r = Weight $ getWeight l * getWeight r
  l - r = Weight $ getWeight l - getWeight r
  abs = Weight . abs . getWeight
  negate = Weight . negate . getWeight
  signum = Weight . signum . getWeight
  fromInteger = Weight . fromIntegral

class Heap a where
  cmp :: a -> a -> Bool

weight :: Tree a -> Weight
weight Leaf = 0
weight (Node _ w _ _) = w

merge :: Heap a => Tree a -> Tree a -> Tree a
merge l Leaf = l
merge Leaf r = r
merge t1@(Node k1 _ l r) t2@(Node k2 _ _ _)
  | k2 `cmp` k1 = merge t2 t1
  | wl < wr' = Node k1 (wl + 1) r' l
  | otherwise = Node k1 (wr' + 1) l r'
  where
    r' = merge r t2
    wl = weight l
    wr' = weight r'

pop :: Heap a => Tree a -> Maybe (a, Tree a)
pop Leaf = Nothing
pop (Node x _ l r) = Just (x, merge l r)

node :: a -> Tree a
node x = Node x 1 Leaf Leaf

fromList :: Heap a => [a] -> Tree a
fromList = foldr (merge . node) Leaf

instance Heap Int where
  cmp = (<)

main :: IO ()
main = do
  print $ unfoldr pop $ fromList [5, 3, 8, 1, -3, 6, 7, 0 :: Int]
  print $ fromList [2, 5, 3, 8, 1 :: Int]
