split :: [a] -> ([a], [a])
split [] = ([], [])
split [x] = ([x], [])
split (l:r:xs) = (l:ls, r:rs)
  where
    (ls, rs) = split xs

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge xs'@(x : xs) ys'@(y : ys)
  | x < y = x : merge xs ys'
  | otherwise = y : merge xs' ys

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [a] = [a]
mergeSort xs = merge (mergeSort ls) (mergeSort rs)
  where
    (ls, rs) = split xs

main :: IO ()
main = print $ mergeSort [5, 7, 4, 8, 2, 1 :: Int]
