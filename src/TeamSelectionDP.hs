{-# LANGUAGE FlexibleContexts #-}

import Control.Monad (when)
import Control.Monad.ST (runST)
import Data.Array (listArray, (!))
import Data.Array.Base (unsafeFreezeSTUArray)
import Data.Array.MArray (MArray, newArray, readArray, writeArray)
import Data.Int (Int64)

-- NOTE: See `https://codeforces.com/gym/102646/problem/D`.
-- NOTE: See `https://codeforces.com/blog/entry/80150`.

{- NOTE: $ 5 4
 -         5 9 10 3 2
 -         10 10 5 5
 -       > 215
 - NOTE: $ 7 4
 -         1 9 3 8 19 3 2
 -         50 1 9 3
 -       > 638
 -}

solve :: [[Int64]] -> Int64
solve (_ : a : b : _) =
  runST $ do
    table <- newArray ((0, 0), (n, k)) minBound
    writeArray table (0, 0) 0
    mapM_ (f table) [(i, j) | i <- [0 .. (n - 1)], j <- [0 .. k]]
    x <- readArray table (n, k)
    _ <- unsafeFreezeSTUArray table
    return x
  where
    n = length a
    k = length b
    a' = listArray (0, n - 1) a
    b' = listArray (0, k - 1) b

    f :: (MArray a Int64 m) => a (Int, Int) Int64 -> (Int, Int) -> m ()
    f table (i, j) = do
      l <- readArray table (i + 1, j)
      r <- readArray table (i, j)
      writeArray table (i + 1, j) $ max l r
      when (j < k) $ do
        l' <- readArray table (i + 1, j + 1)
        r' <- readArray table (i, j)
        writeArray table (i + 1, j + 1) (max l' (r' + ((a' ! i) * (b' ! j))))
solve _ = undefined

main :: IO ()
main = interact $ show . solve . map (map read . words) . lines
