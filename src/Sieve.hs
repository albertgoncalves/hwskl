sieve :: [Int] -> [Int]
sieve (x : xs) = x : sieve (filter ((/= 0) . (`mod` x)) xs)
sieve [] = []

primes :: [Int]
primes = sieve [2 ..]

main :: IO ()
main = print $ take 16 primes
