{-# LANGUAGE Strict #-}

import Data.List (subsequences)
import Data.Map.Strict (Map, empty, insert, lookup)
import Prelude hiding (lookup)

type Memo = Map ([Int], [Int]) (Maybe Int)

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
{- NOTE: $ 8 5
 -         11 9 7 -3 2 0 8 1
 -         1 4 3 -5 10
 -       > 163
 -}

combinations :: Int -> [a] -> [[a]]
combinations n = filter ((== n) . length) . subsequences

bruteForce :: [Int] -> [Int] -> Int
bruteForce bs = maximum . map (sum . zipWith (*) bs) . combinations k
  where
    k = length bs

recursive :: Memo -> [Int] -> [Int] -> (Memo, Maybe Int)
recursive m _ [] = (m, Just 0)
recursive m [] _ = (m, Nothing)
recursive m0 as'@(a : as) bs'@(b : bs)
  | length as' < length bs' = (m0, Nothing)
  | otherwise =
    case lookup k m0 of
      Just x -> (m0, x)
      Nothing ->
        let (m1, x1) = recursive m0 as bs'
            (m2, x2) = fmap ((a * b) +) <$> recursive m1 as bs
            x =
              case (x1, x2) of
                (Just x1', Just x2') -> Just $ max x1' x2'
                (x'@(Just _), Nothing) -> x'
                (Nothing, x') -> x'
         in (insert k x m2, x)
  where
    k = (as', bs')

parse :: String -> ([Int], [Int])
parse = f . map (map read . words) . lines
  where
    f (_ : as : bs : _) = (as, bs)
    f _ = undefined

inject :: [Int] -> [Int] -> (Int, Int)
inject as bs = (bruteForce bs as, snd $ recursive empty as bs)

main :: IO ()
main = interact $ show . uncurry inject . parse
