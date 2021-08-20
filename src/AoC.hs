import Data.Char (ord)

-- NOTE: See `https://adventofcode.com/2018/day/5`.
solve :: String -> String -> String
solve [] ys = reverse ys
solve (x : xs) (y : ys)
  | abs (ord x - ord y) == 32 = solve xs ys
solve (x : xs) ys = solve xs (x : ys)

main :: IO ()
main = print $ length $ solve "dabAcCaCBAcCcaDA" []
