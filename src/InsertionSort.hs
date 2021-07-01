insert :: Ord a => a -> [a] -> [a]
insert x' [] = [x']
insert x' xs'@(x : xs)
  | x' < x = x' : xs'
  | otherwise = x : insert x' xs

insertionSort :: Ord a => [a] -> [a]
insertionSort = foldr insert []

main :: IO ()
main = print $ insertionSort [2, 5, 1, 7, 0, 6, 8, 9, 4, 3 :: Int]
