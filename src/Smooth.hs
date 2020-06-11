import Data.List (groupBy)
import Data.Maybe (isNothing)

smooth :: (Real a, Fractional b) => [Maybe a] -> [b]
smooth = f [] 0.0 0
  where
    f :: (Real a, Fractional b) => [b] -> b -> Int -> [Maybe a] -> [b]
    f ys _ 0 [] = ys
    f ys x n [] = ys ++ f' x n
    f ys x n (Nothing : xs) = f ys x (n + 1) xs
    f ys x n (Just x' : xs) = f (ys ++ f' x n) (realToFrac x') 1 xs
    f' :: (Fractional a) => a -> Int -> [a]
    f' x n = replicate n $ x / fromIntegral n

smooth' :: (Real a, Fractional b) => [Maybe a] -> [b]
smooth' = concatMap f . groupBy (const isNothing)
  where
    f :: (Real a, Fractional b) => [Maybe a] -> [b]
    f xs@(Just x : _) = replicate n $ realToFrac x / fromIntegral n
      where
        n = length xs
    f _ = []

smoothIvan :: (Real a, Fractional b) => [Maybe a] -> [b]
smoothIvan [] = []
smoothIvan (x : xs) = replicate n (x' / fromIntegral n) ++ smoothIvan rest
  where
    (nulls, rest) = span null xs
    n = length nulls + 1
    x' = maybe 0 realToFrac x

main :: IO ()
main = mapM_ (\f -> print (f xs :: [Float])) [smooth, smooth', smoothIvan]
  where
    xs = [Just 2, Nothing, Nothing, Nothing, Just 1] :: [Maybe Int]
