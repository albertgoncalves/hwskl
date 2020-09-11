import Data.List (partition)

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x : xs) = quickSort a ++ [x] ++ quickSort b
  where
    (a, b) = partition (< x) xs

quickSelect :: Ord a => [a] -> Int -> Maybe a
quickSelect [] _ = Nothing
quickSelect (x : xs) k
  | k < l = quickSelect a k
  | k > l = quickSelect b (k - l - 1)
  | otherwise = Just x
  where
    (a, b) = partition (< x) xs
    l = length a

main :: IO ()
main = do
  print $ quickSort xs
  mapM_ (print . quickSelect xs) [0, n `div` 2, n - 1]
  where
    xs = [2, 1, 6, 4, 8, 7, 10 :: Int]
    n = length xs
