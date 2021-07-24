import Data.List (subsequences)

{- NOTE: See `https://codeforces.com/gym/102646/problem/D`. -}
{- NOTE: $ 5 4
 -         5 9 10 3 2
 -         10 10 5 5
 -       > 215
 -}
{- NOTE: $ 7 4
 -         1 9 3 8 19 3 2
 -         50 1 9 3
 -       > 638
 -}

combinations :: Int -> [a] -> [[a]]
combinations n = filter ((== n) . length) . subsequences

solve :: [[Integer]] -> Integer
solve ([_, k] : a : b : _) =
  maximum $ map (sum . zipWith (*) b) (combinations (fromInteger k) a)
solve _ = undefined

main :: IO ()
main = interact $ show . solve . map (map read . words) . lines
