import Prelude hiding (lookup)

data Rope a
  = Empty
  | Leaf a
  | Node Word (Rope a) (Rope a)
  deriving (Show)

getIndex :: Rope a -> Word
getIndex Empty = 0
getIndex (Leaf _) = 1
getIndex (Node i _ _) = i

instance Semigroup (Rope a) where
  l <> Empty = l
  Empty <> r = r
  l <> r = Node (getIndex l + getIndex r) l r

instance Monoid (Rope a) where
  mempty = Empty

toList :: Rope a -> [a]
toList Empty = []
toList (Leaf x) = [x]
toList (Node _ l r) = toList l ++ toList r

lookup :: Rope a -> Word -> Maybe a
lookup xs i = f xs (i + 1)
  where
    f (Leaf x) 1 = Just x
    f (Node _ l r) i'
      | j < i' = f r (i' - j)
      | otherwise = f l i'
      where
        j = getIndex l
    f _ _ = Nothing

insert :: Rope a -> a -> Word -> Rope a
insert Empty x _ = Leaf x
insert xs@(Leaf _) x 0 = Node 2 (Leaf x) xs
insert xs@(Leaf _) x 1 = Node 2 xs (Leaf x)
insert xs@(Leaf _) _ _ = xs
insert (Node j l r) x i
  | i < l' = Node j' (insert l x i) r
  | otherwise = Node j' l (insert r x $ i - l')
  where
    j' = j + 1
    l' = getIndex l

main :: IO ()
main = do
  print xs
  print $ lookup xs 0
  mapM_ (print . toList . insert xs 'f') [0 .. 5]
  where
    xs = insert (mconcat $ map Leaf "abde") 'c' 2
