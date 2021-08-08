import Data.Array (Array, array, elems, (!))

-- NOTE: See `https://www.geeksforgeeks.org/golomb-sequence/`.

golomb :: Int -> [Int]
golomb n = elems table
  where
    table :: Array Int Int
    table = array (1, n) [f i | i <- [1 .. n]]

    f :: Int -> (Int, Int)
    f 1 = (1, 1)
    f i = (i, 1 + (table ! (i - (table ! (table ! (i - 1))))))

main :: IO ()
main = print $ golomb 11
