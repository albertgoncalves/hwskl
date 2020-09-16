import Control.Monad (foldM, (>=>))
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

insert :: Rope a -> a -> Word -> Maybe (Rope a)
insert Empty x _ = Just $ Leaf x
insert xs@(Leaf _) x 0 = Just $ Node 2 (Leaf x) xs
insert xs@(Leaf _) x 1 = Just $ Node 2 xs (Leaf x)
insert (Leaf _) _ _ = Nothing
insert (Node j l r) x i
  | i < l' = insert l x i >>= \xs -> Just $ Node j' xs r
  | otherwise = insert r x (i - l') >>= \xs -> Just $ Node j' l xs
  where
    j' = j + 1
    l' = getIndex l

main :: IO ()
main = do
  print xs
  maybe (return ()) (mapM_ print) $
    mapM ((\i -> xs >>= \xs' -> insert xs' ' ' i) >=> Just . toList) ns
  where
    xs =
      foldM
        (\xs' (x, i) -> insert xs' x i)
        (mconcat $ map Leaf "dc")
        [('a', 2), ('b', 2), ('e', 0), ('f', 0)]
    ns = [0 .. 6]
