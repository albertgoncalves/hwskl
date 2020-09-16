import Prelude hiding (lookup)

data Rope a
  = Empty
  | Single a
  | Append Word (Rope a) (Rope a)
  deriving (Eq, Show)

getIndex :: Rope a -> Word
getIndex Empty = 0
getIndex (Single _) = 1
getIndex (Append i _ _) = i

instance Semigroup (Rope a) where
  l <> Empty = l
  Empty <> r = r
  l <> r = Append (getIndex l + getIndex r) l r

instance Monoid (Rope a) where
  mempty = Empty

toList :: Rope a -> [a]
toList Empty = []
toList (Single x) = [x]
toList (Append _ l r) = toList l ++ toList r

lookup :: Rope a -> Word -> Maybe a
lookup xs i = f xs (i + 1)
  where
    f (Single x) 1 = Just x
    f (Append _ l r) i'
      | j < i' = f r (i' - j)
      | otherwise = f l i'
      where
        j = getIndex l
    f _ _ = Nothing

insert :: Rope a -> a -> Word -> Rope a
insert Empty x _ = Single x
insert xs@(Single _) x 0 = Append 2 (Single x) xs
insert xs@(Single _) x 1 = Append 2 xs (Single x)
insert xs@(Single _) _ _ = xs
insert (Append j l r) x i
  | i < l' = Append j' (insert l x i) r
  | otherwise = Append j' l (insert r x $ i - l')
  where
    j' = j + 1
    l' = getIndex l

main :: IO ()
main = do
  print xs
  print $ lookup xs 0
  mapM_ (print . toList) xs'
  where
    xs = mconcat $ map Single "abcde"
    xs' = map (insert xs 'f') [0 .. 5]
