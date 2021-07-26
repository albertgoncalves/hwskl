import Data.Array (Array, bounds, listArray, (!))
import Data.List (sort)

-- NOTE: See `https://www.youtube.com/watch?v=LjrCckaHjB0`.
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
  print $ last $ dpList xs
  print $ (! n) $ dpArray $ listArray (0, n) xs
  where
    xs = sort [0, 22, 16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4 :: Int]
    n = length xs - 1
