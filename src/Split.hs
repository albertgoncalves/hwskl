data Note = Note Int Bool

instance Show Note where
  show (Note n True) = show n ++ "~"
  show (Note n False) = show n

{-
 -  |-----k-----|
 -  |--n0--|
 -         |-x1-|
 -              |--x2-|
 -         |----x0----|
 -  |--------n1-------|
 -}

split :: Int -> Int -> [Int] -> [Note]
split _ _ [] = []
split k n0 (x0 : xs)
  | k <= n0 = undefined
  | n1 < k = Note x0 False : split k n1 xs
  | x2 == 0 = Note x1 False : split k 0 xs
  | otherwise = Note x1 True : split k 0 (x2 : xs)
  where
    n1 = n0 + x0
    x1 = x0 - x2
    x2 = n1 - k

main :: IO ()
main =
  putStrLn $
    unwords $
      map show $
        split 4 0 [4, 3, 2, 3, 4, 2, 3, 2, 4, 2, 3, 4]
