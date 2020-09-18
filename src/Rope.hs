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
concat l r = Node (weight l + weight r) l r

toText :: Rope -> Text
toText (Leaf x) = x
toText (Node _ l r) = toText l <> toText r

ping :: Rope -> Word -> Maybe Text
ping xs@(Leaf x) i
  | i < weight xs = Just x
  | otherwise = Nothing
ping (Node _ l r) i
  | i < j = ping l i
  | otherwise = ping r (i - j)
  where
    j = weight l

split :: Rope -> Word -> (Maybe Rope, Maybe Rope)
split xs@(Leaf x) i
  | i == j = (Just xs, Nothing)
  | i < j = (Just $ Leaf l, Just $ Leaf r)
  | otherwise = (Nothing, Nothing)
  where
    j = weight xs
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
insert xs i x =
  case split xs i of
    (Just l, Just r) -> Just $ concat l $ concat (Leaf x) r
    (Just l, Nothing) -> Just $ concat l (Leaf x)
    _ -> Nothing

delete :: Rope -> Word -> Word -> Maybe Rope
delete xs i j
  | j <= i = Nothing
  | otherwise = do
    r <- snd $ split xs j
    l <- fst $ split xs i
    Just $ concat l r

slice :: Rope -> Word -> Word -> Maybe Rope
slice xs i j
  | j <= i = Nothing
  | otherwise = do
    l <- fst $ split xs j
    r <- snd $ split l i
    Just r

balance :: Rope -> Maybe Rope
balance x@(Leaf _) = Just x
balance xs@(Node i _ _) = do
  l' <- l >>= balance
  r' <- r >>= balance
  Just $ concat l' r'
  where
    n = i `div` 2
    (l, r) = split xs n

threshold :: Word
threshold = 10

compress :: Rope -> Maybe Rope
compress x@(Leaf _) = Just x
compress (Node i l r)
  | i < threshold = Just $ Leaf $ toText l <> toText r
  | otherwise = do
    l' <- compress l
    r' <- compress r
    Just $ concat l' r'

main :: IO ()
main = do
  print xs
  print $ balance xs
  print $ (compress <=< balance) xs
  mapM_ (\i -> print (i, ping xs i)) [0 .. 15]
  mapM_
    (print . fmap toText)
    [insert xs 10 " ! ", delete xs 8 11, slice xs 8 11]
  where
    xs =
      foldl
        (\xs' x -> concat xs' $ Leaf $ " " <> x)
        (Leaf "foo")
        ["bar", "baz", "qux"]
