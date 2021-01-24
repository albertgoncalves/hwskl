import Data.List (sort)

-- NOTE: See `https://www.youtube.com/watch?v=LjrCckaHjB0`.
dp :: [Int] -> [Int]
dp xs = 1 : map f (tail xs)
  where
    f x =
      sum $
        map snd $
          takeWhile (\(x', _) -> x' < x) $
            dropWhile (\(x', _) -> 3 < (x - x')) $
              zip xs $ dp xs

main :: IO ()
main =
  print $
    last $
      dp $ sort $ 0 : 22 : [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4 :: Int]
