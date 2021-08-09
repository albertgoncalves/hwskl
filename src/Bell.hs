import Data.Array (Array, array, (!))

-- NOTE: See `https://www.geeksforgeeks.org/bell-numbers-number-of-ways-to-partition-a-set/`.
-- NOTE: See `https://oeis.org/A000110`.

bell :: Int -> Int
bell n = table ! (n, 0)
  where
    table :: Array (Int, Int) Int
    table =
      array
        ((0, 0), (n + 1, n + 1))
        [((i, j), f i j) | i <- [0 .. n], j <- [0 .. i]]

    f :: Int -> Int -> Int
    f 0 0 = 1
    f i 0 = table ! (i - 1, i - 1)
    f i j = (table ! (i - 1, j - 1)) + (table ! (i, j - 1))

main :: IO ()
main = print $ bell 20 == 51724158235372
