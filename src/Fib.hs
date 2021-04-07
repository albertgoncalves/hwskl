fib :: Word -> Word
fib 0 = 0
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)

fibTco :: Word -> Word
fibTco = f 0 1
  where
    f :: Word -> Word -> Word -> Word
    f x _ n
      | n <= 0 = x
    f a b n = f (a + b) a (n - 1)

fibs :: [Word]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

main :: IO ()
main = do
  print $ fib 5
  print $ fibTco 5
  print $ take 6 fibs
