import Data.Maybe (fromMaybe)
import Prelude hiding (concat)

data Rope
  = Leaf Char
  | Node Int Rope Rope
  deriving (Eq, Show)

weight :: Rope -> Int
weight (Leaf _) = 1
weight (Node n _ _) = n

concat :: Rope -> Rope -> Rope
concat l r = Node (weight l + weight r) l r

toString :: Rope -> String
toString (Leaf x) = [x]
toString (Node _ l r) = toString l ++ toString r

ping :: Rope -> Int -> Char
ping (Leaf x) _ = x
ping (Node _ l r) i
  | i < n = ping l i
  | otherwise = ping r $ i - n
  where
    n = weight l

push :: Rope -> Char -> Rope
push t = concat t . Leaf

split :: Rope -> Int -> (Maybe Rope, Maybe Rope)
split t@(Leaf _) i
  | i < 1 = (Nothing, Just t)
  | otherwise = (Just t, Nothing)
split (Node _ l r) i
  | i < n = case split l i of
    (Nothing, Nothing) -> (Nothing, Nothing)
    (Nothing, Just r') -> (Nothing, Just $ concat r' r)
    (l', Just r') -> (l', Just $ concat r' r)
    (l', Nothing) -> (l', Just r)
  | otherwise = case split r (i - n) of
    (Nothing, Nothing) -> (Nothing, Nothing)
    (Just l', Nothing) -> (Just $ concat l l', Nothing)
    (Just l', r') -> (Just $ concat l l', r')
    (Nothing, r') -> (Just l, r')
  where
    n = weight l

insert :: Rope -> Rope -> Int -> Rope
insert t t' i = case split t i of
  (Just l, Just r) -> concat l $ concat t' r
  (Just l, Nothing) -> concat l t'
  (Nothing, Just r) -> concat t' r
  _ -> t'

delete :: Rope -> Int -> Int -> Rope
delete t a b = case split t i of
  (Just l, Just r) -> maybe l (concat l) $ snd $ split r j
  (Nothing, Just r) -> fromMaybe t $ snd $ split r j
  (Just l, Nothing) -> l
  _ -> t
  where
    (i, j) = if a < b then (a, b - a) else (b, a - b)

slice :: Rope -> Int -> Int -> String
slice t a b = maybe "" toString $ snd (split t i) >>= \r -> fst $ split r j
  where
    (i, j) = if a < b then (a, b - a) else (b, a - b)

rope :: String -> Maybe Rope
rope [] = Nothing
rope [x] = Just $ Leaf x
rope xs = case (rope l, rope r) of
  (Just l', Just r') -> Just $ concat l' r'
  (Just l', Nothing) -> Just l'
  (Nothing, Just r') -> Just r'
  _ -> Nothing
  where
    (l, r) = splitAt (length xs `div` 2) xs

balance :: Rope -> Rope
balance t@(Leaf _) = t
balance t@(Node n _ _) = case split t $ n `div` 2 of
  (Just l, Just r) -> concat (balance l) (balance r)
  (Just l, Nothing) -> balance l
  (Nothing, Just r) -> balance r
  _ -> t

main :: IO ()
main = do
  print $ rope "abcde"
  print t
  print $ balance t
  print $ Just t == rope "abcde"
  print $ Just (balance t) == rope "abcde"
  print' (ping t) [-1 .. 5]
  print' (toString . insert t (concat (Leaf '-') (Leaf '-'))) [-1 .. 6]
  print' (toString . delete t 3) [0 .. 5]
  print' (slice t 3) [0 .. 5]
  where
    t = foldl push (Leaf 'a') "bcde"
    print' f = mapM_ (print . f)
