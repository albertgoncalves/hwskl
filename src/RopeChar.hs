import Control.Monad (foldM, (>=>))
import Prelude hiding (lookup)

data Rope a
  = Leaf a
  | Node Word (Rope a) (Rope a)
  deriving (Show)

getIndex :: Rope a -> Word
getIndex (Leaf _) = 1
getIndex (Node i _ _) = i

toList :: Rope a -> [a]
toList (Leaf x) = [x]
toList (Node _ l r) = toList l ++ toList r

lookup :: Rope a -> Word -> Maybe a
lookup (Leaf x) 0 = Just x
lookup (Node _ l r) i'
  | j <= i' = lookup r (i' - j)
  | otherwise = lookup l i'
  where
    j = getIndex l
lookup _ _ = Nothing

insert :: Rope a -> a -> Word -> Maybe (Rope a)
insert xs@(Leaf _) x 0 = Just $ Node 2 (Leaf x) xs
insert xs@(Leaf _) x 1 = Just $ Node 2 xs (Leaf x)
insert (Leaf _) _ _ = Nothing
insert (Node j l r) x i
  | i < l' = insert l x i >>= \xs -> Just $ Node j' xs r
  | otherwise = insert r x (i - l') >>= Just . Node j' l
  where
    j' = j + 1
    l' = getIndex l

delete :: Rope a -> Word -> Maybe (Rope a)
delete (Leaf _) _ = Nothing
delete (Node _ (Leaf _) r) 0 = Just r
delete (Node _ l (Leaf _)) i
  | getIndex l == i = Just l
delete (Node j l r) i
  | i < l' = delete l i >>= \xs -> Just $ Node j' xs r
  | otherwise = delete r (i - l') >>= Just . Node j' l
  where
    j' = j - 1
    l' = getIndex l

main :: IO ()
main = do
  print xs
  print' $ mapM ((\i -> xs >>= \xs' -> insert xs' '-' i) >=> toList') ns
  print' $ mapM ((\i -> xs >>= \xs' -> delete xs' i) >=> toList') ns
  where
    toList' = Just . toList
    print' = maybe (return ()) (mapM_ print)
    xs =
      foldM
        (\xs' (x, i) -> insert xs' x i)
        (Leaf 'd')
        [('c', 0), ('b', 0), ('e', 3), ('a', 0), ('f', 5)]
    ns = [0 .. 5]
