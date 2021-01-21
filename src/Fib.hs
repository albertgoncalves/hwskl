fib :: Word -> Word
fib 0 = 0
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)

fibs :: [Word]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

main :: IO ()
main = do
  print $ fib 5
  print $ take 6 fibs
