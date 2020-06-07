xor :: [Bool] -> Bool
xor = odd . foldl (flip $ (+) . f) 0
  where
    f :: Bool -> Int
    f True = 1
    f False = 0

{- NOTE: Can we implement `map` in terms of `foldr`? Sure. -}
map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

main :: IO ()
main = do
  print $ map' (+ 1) [1 .. 5 :: Int]
  print $ xor [False, True, False]
  print $ xor [False, True, False, False, True]
