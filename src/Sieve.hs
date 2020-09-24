sieve :: [Word] -> [Word]
sieve [] = []
sieve (x : xs) = x : filter ((0 /=) . (`mod` x)) (sieve xs)

main :: IO ()
main = print $ take 16 $ sieve [2 :: Word ..]
