{-# LANGUAGE OverloadedStrings #-}

import Control.Monad ((<=<))
import Data.Text (Text, length, splitAt)
import Prelude hiding (concat, length, splitAt)

data Rope
  = Leaf Text
  | Node Word Rope Rope
  deriving (Show)

weight :: Rope -> Word
weight (Leaf x) = fromIntegral $ length x
weight (Node i _ _) = i

concat :: Rope -> Rope -> Rope
concat l (Leaf "") = l
concat (Leaf "") r = r
concat l r = Node (weight l + weight r) l r

toText :: Rope -> Text
toText (Leaf x) = x
toText (Node _ l r) = toText l <> toText r

ping :: Rope -> Word -> Maybe Text
ping (Leaf "") _ = Nothing
ping xs@(Leaf x) i
  | i < weight xs = Just x
  | otherwise = Nothing
ping (Node _ l r) i
  | i < j = ping l i
  | otherwise = ping r (i - j)
  where
    j = weight l

split :: Rope -> Word -> (Maybe Rope, Maybe Rope)
split (Leaf "") _ = (Nothing, Nothing)
split xs@(Leaf x) i
  | i == k = (Just xs, Nothing)
  | i < k = (Just $ Leaf l, Just $ Leaf r)
  | otherwise = (Nothing, Nothing)
  where
    k = weight xs
    (l, r) = splitAt (fromIntegral i) x
split (Node _ l r) i
  | i == j = (Just l, Just r)
  | i < j =
    case split l i of
      (Just l', Just r') -> (Just l', Just $ concat r' r)
      (Just l', Nothing) -> (Just l', Just r)
      _ -> (Nothing, Nothing)
  | otherwise =
    case split r (i - j) of
      (Just l', Just r') -> (Just $ concat l l', Just r')
      (Just l', Nothing) -> (Just $ concat l l', Nothing)
      _ -> (Nothing, Nothing)
  where
    j = weight l

insert :: Rope -> Word -> Text -> Maybe Rope
insert _ _ "" = Nothing
insert xs i x
  | k < i = Nothing
  | otherwise = case split xs i of
    (Just l, Just r) -> Just $ concat l $ concat (Leaf x) r
    (Just l, Nothing) -> Just $ concat l (Leaf x)
    _ -> Nothing
  where
    k = weight xs

delete :: Rope -> Word -> Word -> Maybe Rope
delete xs i j
  | ((i == 0) && (j == k)) || (k < j) || (j <= i) = Nothing
  | otherwise = do
    case split xs j of
      (_, Just r) -> do
        l <- fst $ split xs i
        Just $ concat l r
      (_, Nothing) -> fst $ split xs i
  where
    k = weight xs

slice :: Rope -> Word -> Word -> Maybe Rope
slice xs i j
  | (k < j) || (j <= i) = Nothing
  | (i == 0) && (k == j) = Just xs
  | otherwise = fst (split xs j) >>= \l -> snd $ split l i
  where
    k = weight xs

balance :: Rope -> Maybe Rope
balance (Leaf "") = Nothing
balance x@(Leaf _) = Just x
balance xs@(Node i _ _) =
  case split xs n of
    (Just l, Just r) ->
      case (balance l, balance r) of
        (Just l', Just r') -> Just $ concat l' r'
        (Just l', Nothing) -> Just l'
        _ -> Nothing
    (Just l, Nothing) -> balance l
    _ -> Nothing
  where
    n = i `div` 2

threshold :: Word
threshold = 10

compress :: Rope -> Maybe Rope
compress (Leaf "") = Nothing
compress x@(Leaf _) = Just x
compress (Node i l r)
  | i < threshold = Just $ Leaf $ toText l <> toText r
  | otherwise = case (compress l, compress r) of
    (Just l', Just r') -> Just $ concat l' r'
    (Just l', Nothing) -> Just l'
    _ -> Nothing

main :: IO ()
main = do
  print xs
  print $ balance xs
  print $ (compress <=< balance) xs
  mapM_ (\i -> print (i, ping xs i)) [0 .. 15]
  mapM_
    (print . fmap toText)
    [ insert xs 10 " ! ",
      slice xs 8 11,
      slice xs 0 15,
      slice xs 0 16,
      delete xs 8 11,
      delete xs 1 15,
      delete xs 0 14,
      delete xs 0 15
    ]
  where
    xs =
      foldl
        (\xs' x -> concat xs' $ Leaf x)
        (Leaf "foo")
        [" bar", " baz", " qux"]
