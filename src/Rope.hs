import Prelude hiding (lookup)

data Rope a
  = Empty
  | Single Word a
  | Append Word (Rope a) (Rope a)
  deriving (Eq, Show)

getIndex :: Rope a -> Word
getIndex Empty = 0
getIndex (Single i _) = i
getIndex (Append i _ _) = i

instance Semigroup (Rope a) where
  l <> Empty = l
  Empty <> r = r
  l <> r = Append (getIndex l + getIndex r) l r

instance Monoid (Rope a) where
  mempty = Empty

lookup :: Word -> Rope a -> Maybe a
lookup _ Empty = Nothing
lookup i (Single j x)
  | i == j = Just x
  | otherwise = Nothing
lookup i (Append _ l r)
  | j < i = lookup (i - j) r
  | otherwise = lookup i l
  where
    j = getIndex l

main :: IO ()
main = do
  print xs
  print $ lookup 3 xs
  where
    xs = mconcat $ map (Single 1) "abcde"
