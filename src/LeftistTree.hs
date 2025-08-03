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
merge a Leaf = a
merge Leaf b = b
merge a@(Node _ i _ _ _) b@(Node _ j _ _ _)
  | j < i = merge b a
merge (Node _ k v l r) a@Node {}
  | weight l < weight b = Node (weight l + 1) k v b l
  | otherwise = Node (weight b + 1) k v l b
  where
    b = merge r a

pop :: (Ord k) => Heap k v -> Maybe ((k, v), Heap k v)
pop Leaf = Nothing
pop (Node _ k v l r) = Just ((k, v), merge l r)

node :: k -> v -> Heap k v
node k v = Node 1 k v Leaf Leaf

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
          (2, 'd'),
          (-3, 'e'),
          (1, 'f')
        ]
