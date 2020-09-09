import Data.List (subsequences)

combinations :: Int -> [a] -> [[a]]
combinations n = filter ((== n) . length) . subsequences

solve :: [[Integer]] -> Integer
solve ([_, k] : a : b : _) =
  maximum $ map (sum . zipWith (*) b) (combinations (fromInteger k) a)
solve _ = undefined

main :: IO ()
main = interact (show . solve . map (map read . words) . lines)
