import Data.List (subsequences)

{- NOTE: See `https://codeforces.com/gym/102646/problem/D`. -}

combinations :: Int -> [a] -> [[a]]
combinations n = filter ((== n) . length) . subsequences

solve :: [[Integer]] -> Integer
solve ([_, k] : a : b : _) =
  maximum $ map (sum . zipWith (*) b) (combinations (fromInteger k) a)
solve _ = undefined

main :: IO ()
main = interact (show . solve . map (map read . words) . lines)
