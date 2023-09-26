import Data.List (unfoldr)

{- NOTE: See `http://typeocaml.com/2015/03/12/heap-leftist-tree/`. -}

data Heap k v
  = Leaf
  | Node Int k v (Heap k v) (Heap k v)
  deriving (Show)

weight :: Heap k v -> Int
weight Leaf = 0
weight (Node w _ _ _ _) = w

merge :: (Ord k) => Heap k v -> Heap k v -> Heap k v
merge l Leaf = l
merge Leaf r = r
merge t1@(Node _ k1 v1 l r) t2@(Node _ k2 _ _ _)
  | k2 < k1 = merge t2 t1
  | wl < wr' = Node (wl + 1) k1 v1 r' l
  | otherwise = Node (wr' + 1) k1 v1 l r'
  where
    r' = merge r t2
    wl = weight l
    wr' = weight r'

pop :: (Ord k) => Heap k v -> Maybe ((k, v), Heap k v)
pop Leaf = Nothing
pop (Node _ n x l r) = Just ((n, x), merge l r)

node :: k -> v -> Heap k v
node n x = Node 1 n x Leaf Leaf

fromList :: (Ord k) => [(k, v)] -> Heap k v
fromList = foldr (merge . uncurry node) Leaf

main :: IO ()
main =
  print $
    unfoldr pop $
      fromList
        [ (5, 'a') :: (Int, Char),
          (3, 'b'),
          (8, 'c'),
          (1, 'd'),
          (-3, 'e'),
          (1, 'f')
        ]
