import Data.Array (Array, bounds, listArray, (!))
import Data.List (sort)

-- NOTE: See `https://adventofcode.com/2020/day/10`.
-- NOTE: See `https://www.youtube.com/watch?v=LjrCckaHjB0&lc=UgyTL_x6MXoIItvsuUB4AaABAg`.

dpList :: [Int] -> [Int]
dpList xs = 1 : map f (tail xs)
  where
    f x =
      sum $
        map snd $
          takeWhile ((< x) . fst) $
            dropWhile ((3 <) . (x -) . fst) $
              zip xs $ dpList xs

dpArray :: Array Int Int -> Array Int Int
dpArray xs = listArray (bounds xs) (1 : map f [1 ..])
  where
    f i =
      sum
        . map (dpArray xs !)
        . takeWhile (\j -> (xs ! i) - (xs ! j) <= 3)
        $ [i - 1, i - 2 .. 0]

main :: IO ()
main = do
  mapM_
    (\f -> print $ f $ sort $ 0 : maximum xs + 3 : xs)
    [ last . dpList,
      (! n) . dpArray . listArray (0, n)
    ]
  where
    xs = [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4 :: Int]
    n = (length xs + 2) - 1
