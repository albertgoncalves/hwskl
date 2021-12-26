import Data.Array (Array, array, listArray, (!))
import Data.Map (Map, empty, insert, lookup)
import Prelude hiding (lookup)

type Memo a = Map ([a], [a]) Int

lcs :: (Eq a, Ord a) => Memo a -> [a] -> [a] -> (Memo a, Int)
lcs m [] _ = (m, 0)
lcs m _ [] = (m, 0)
lcs m ls'@(l : ls) rs'@(r : rs) =
  case lookup k m of
    Just v -> (m, v)
    Nothing ->
      if l == r
        then
          let (m0, v0) = lcs m ls rs
              v0' = v0 + 1
           in (insert k v0' m0, v0')
        else
          let (m1, v1) = lcs m ls' rs
              (m2, v2) = lcs m1 ls rs'
              v1' = max v1 v2
           in (insert k v1' m2, v1')
  where
    k = (ls', rs')

lcs' :: (Eq a) => [a] -> [a] -> Int
lcs' ls rs
  | nL == 0 = 0
  | nR == 0 = 0
  | otherwise = t ! (nL, nR)
  where
    nL = length ls
    nR = length rs

    l = listArray (1, nL) ls
    r = listArray (1, nR) rs

    t :: Array (Int, Int) Int
    t =
      array
        ((0, 0), (nL, nR))
        [((j, i), f j i) | j <- [0 .. nL], i <- [0 .. nR]]

    f :: Int -> Int -> Int
    f 0 _ = 0
    f _ 0 = 0
    f j i
      | (l ! j) == (r ! i) = t ! (j - 1, i - 1) + 1
      | otherwise = max (t ! (j, i - 1)) (t ! (j - 1, i))

main :: IO ()
main = do
  mapM_
    ( \f ->
        print $
          f
            "the quick brown fox jumps over the lazy dog"
            "pack my box with five dozen liquor jugs"
            == 16
    )
    [\xs -> snd . lcs empty xs, lcs']
