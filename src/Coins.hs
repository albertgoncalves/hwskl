import Data.Array (Array, array, listArray, (!))

-- NOTE: See `https://www.geeksforgeeks.org/coin-change-dp-7/`.

changeRec :: [Int] -> Int -> Int
changeRec _ 0 = 1
changeRec _ n
  | n < 0 = 0
changeRec [] _ = 0
changeRec xs'@(x : xs) n = changeRec xs n + changeRec xs' (n - x)

changeRec' :: Array Int Int -> Int -> Int -> Int
changeRec' xs i n
  | n == 0 = 1
  | (n < 0) || (i <= 0) = 0
  | otherwise = changeRec' xs (i - 1) n + changeRec' xs i (n - xs ! (i - 1))

changeTable :: [Int] -> Int -> Int
changeTable xs n = t ! (l, n)
  where
    l = length xs
    xs' = listArray (0, l - 1) xs

    t :: Array (Int, Int) Int
    t = array ((0, 0), (l, n)) [((j, i), f j i) | j <- [0 .. l], i <- [0 .. n]]

    f :: Int -> Int -> Int
    f 0 _ = 0
    f _ 0 = 1
    f j i = t ! (j - 1, i) + (if i' < 0 then 0 else t ! (j, i'))
      where
        i' = i - (xs' ! (j - 1))

main :: IO ()
main =
  mapM_
    (\f -> print $ f 8)
    [ changeRec xs,
      changeRec' (listArray (0, n - 1) xs) n,
      changeTable xs
    ]
  where
    xs = [1, 5, 10 :: Int]
    n = length xs
