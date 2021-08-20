import Data.Array (Array, array, (!))
import Data.Word (Word64)

-- NOTE: See `https://www.geeksforgeeks.org/friends-pairing-problem/`.

tabular :: Word64 -> Word64
tabular n = table ! n
  where
    table :: Array Word64 Word64
    table = array (0, n) [(i, f i) | i <- [0 .. n]]

    f :: Word64 -> Word64
    f i
      | i <= 2 = i
      | otherwise = (table ! (i - 1)) + ((i - 1) * (table ! (i - 2)))

bruteForce :: Word64 -> Word64
bruteForce n
  | n <= 2 = n
  | otherwise = bruteForce (n - 1) + ((n - 1) * bruteForce (n - 2))

main :: IO ()
main = mapM_ (\f -> print $ f 32) [tabular, bruteForce]
