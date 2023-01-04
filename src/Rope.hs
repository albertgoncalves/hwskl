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

toRope :: String -> Maybe Rope
toRope [] = Nothing
toRope [x] = Just $ Leaf x
toRope xs = case (toRope l, toRope r) of
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
      (l', Just r') -> (l', Just $ concat r' r)
      (l', Nothing) -> (l', Just r)
  | otherwise = case split r $ i - n of
      (Nothing, Nothing) -> (Nothing, Nothing)
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
    i' = max i 0
    j' = max j 0

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

main :: IO ()
main =
  test $
    zip
      (show <$> [0 :: Int ..])
      [ foldl push (Leaf 'a') "bcde" == t,
        t' /= t,
        t' == balance t,
        toRope "abcde" /= Just t,
        toRope "abcde" == Just t',
        toRope "abcde" == Just (balance t),
        (toString <$> toRope "abcde") == Just "abcde",
        toString (foldl push (Leaf 'a') "bcde") == "abcde",
        map (ping t) n == "aabcdeee",
        map (ping t') n == "aabcdeee",
        map (toString . insert t u) n == i,
        map (toString . insert t' u) n == i,
        map (toString . delete t 3) n == d,
        map (toString . delete t' 3) n == d,
        map (slice t 3) n == s,
        map (slice t' 3) n == s
      ]
  where
    test :: [(String, Bool)] -> IO ()
    test =
      ( \(s', s'') ->
          putStrLn s' >> if s'' == "" then return () else putStrLn s''
      )
        . mconcat
        . map (\(s', b) -> if b then (".", "") else ("!", '\n' : s'))
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
    n = [-1 .. 6] :: [Int]
    i =
      [ "--abcde",
        "--abcde",
        "a--bcde",
        "ab--cde",
        "abc--de",
        "abcd--e",
        "abcde--",
        "abcde--"
      ]
    d = ["de", "de", "ade", "abde", "abcde", "abce", "abc", "abc"]
    s = ["abc", "abc", "bc", "c", "", "d", "de", "de"]
