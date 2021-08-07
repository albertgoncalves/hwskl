import Data.Array (Array, elems, listArray, (!))

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)

fibTco :: Int -> Int
fibTco = f 0 1
  where
    f :: Int -> Int -> Int -> Int
    f x _ n
      | n <= 0 = x
    f a b n = f (a + b) a (n - 1)

fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibsArray :: Int -> Array Int Int
fibsArray n = loop
  where
    loop :: Array Int Int
    loop = listArray (0, n - 1) $ 0 : 1 : map f [2 ..]
    f i = (loop ! (i - 2)) + (loop ! (i - 1))

main :: IO ()
main = do
  print $ fib 10
  print $ fibTco 10
  print $ take 11 fibs
  print $ elems $ fibsArray 11
