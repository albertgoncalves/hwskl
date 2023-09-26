{-# LANGUAGE GeneralizedNewtypeDeriving #-}

data JoinList m a
  = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

tag :: (Monoid m) => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

instance (Monoid m) => Semigroup (JoinList m a) where
  l <> r = Append (tag l <> tag r) l r

instance (Monoid m) => Monoid (JoinList m a) where
  mempty = Empty

newtype Size = Size Int
  deriving (Eq, Ord, Show, Num)

class Sized a where
  size :: a -> Size

instance Sized Size where
  size = id

instance Semigroup Size where
  (<>) = (+)

instance Monoid Size where
  mempty = Size 0

getSize :: (Monoid b, Sized b) => JoinList b a -> Int
getSize = f . size . tag
  where
    f (Size n) = n

getIndex :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
getIndex _ Empty = Nothing
getIndex i jl
  | i < 0 = Nothing
  | getSize jl <= i = Nothing
getIndex _ (Single _ a) = Just a
getIndex i (Append _ l r)
  | i < left = getIndex i l
  | otherwise = getIndex (i - left) r
  where
    left = getSize l

getSingle :: Char -> JoinList Size Char
getSingle = Single 1

fromList :: String -> JoinList Size Char
fromList = mconcat . (getSingle <$>)

main :: IO ()
main = do
  print list
  print $ getIndex 0 list
  where
    list = fromList "abcde"
