import Data.List (partition)

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x : xs) = quickSort a ++ [x] ++ quickSort b
  where
    (a, b) = partition (<= x) xs

main :: IO ()
main = print $ quickSort [2, 1, 6, 4, 8, 7, 10 :: Int]
