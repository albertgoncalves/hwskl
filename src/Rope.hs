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

balance :: Rope -> Rope
balance t@(Leaf _) = t
balance t@(Node n _ _) = case split t $ n `div` 2 of
  (Just l, Just r) -> concat (balance l) (balance r)
  (Just l, Nothing) -> balance l
  (Nothing, Just r) -> balance r
  _ -> t

insert :: Rope -> Rope -> Int -> Rope
insert t t' i = case split t i of
  (Just l, Just r) -> concat l $ concat t' r
  (Just l, Nothing) -> concat l t'
  (Nothing, Just r) -> concat t' r
  _ -> t'

indices :: Int -> Int -> (Int, Int)
indices i j = if i < j then (i', j' - i') else (j', i' - j')
  where
    (i', j') = (max i 0, max j 0)

delete :: Rope -> Int -> Int -> Rope
delete t i j = case split t i' of
  (Just l, Just r) -> maybe l (concat l) $ snd $ split r j'
  (Nothing, Just r) -> fromMaybe t $ snd $ split r j'
  (Just l, Nothing) -> l
  _ -> t
  where
    (i', j') = indices i j

slice :: Rope -> Int -> Int -> String
slice t i j = maybe "" toString $ snd (split t i') >>= \r -> fst $ split r j'
  where
    (i', j') = indices i j

test :: [(String, Bool)] -> IO ()
test =
  ( \(results, labels) -> do
      putStrLn results
      if labels == ""
        then return ()
        else putStrLn labels
  )
    . mconcat
    . map (\(label, bool) -> if bool then (".", "") else ("!", '\n' : label))

main :: IO ()
main = do
  test
    [ ("00", foldl push (Leaf 'a') "bcde" == t),
      ("01", rope "abcde" /= Just t),
      ("02", rope "abcde" == Just t'),
      ("03", rope "abcde" == Just (balance (foldl push (Leaf 'a') "bcde"))),
      ("04", (toString <$> rope "abcde") == Just "abcde"),
      ("05", toString (foldl push (Leaf 'a') "bcde") == "abcde"),
      ("06", map (ping t') [-1 .. 5] == "aabcdee"),
      ("07", map (toString . insert t u) n == a),
      ("08", map (toString . insert t' u) n == a),
      ("09", map (toString . delete t 3) n == b),
      ("10", map (toString . delete t' 3) n == b),
      ("11", map (slice t 3) n == c),
      ("12", map (slice t' 3) n == c)
    ]
  where
    t =
      Node
        5
        (Node 4 (Node 3 (Node 2 (Leaf 'a') (Leaf 'b')) (Leaf 'c')) (Leaf 'd'))
        (Leaf 'e')
    t' =
      Node
        5
        (Node 2 (Leaf 'a') (Leaf 'b'))
        (Node 3 (Leaf 'c') (Node 2 (Leaf 'd') (Leaf 'e')))
    u = Node 2 (Leaf '-') (Leaf '-')
    n = [-1 .. 6]
    a =
      [ "--abcde",
        "--abcde",
        "a--bcde",
        "ab--cde",
        "abc--de",
        "abcd--e",
        "abcde--",
        "abcde--"
      ]
    b =
      [ "de",
        "de",
        "ade",
        "abde",
        "abcde",
        "abce",
        "abc",
        "abc"
      ]
    c =
      [ "abc",
        "abc",
        "bc",
        "c",
        "",
        "d",
        "de",
        "de"
      ]
