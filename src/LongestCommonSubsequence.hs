import Data.Map (Map, empty, insert, lookup)
import Prelude hiding (lookup)

lcs ::
  (Eq a, Ord a, Show a) =>
  Map ([a], [a]) Int ->
  [a] ->
  [a] ->
  (Map ([a], [a]) Int, Int)
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

main :: IO ()
main =
  print $
    test
      "the quick brown fox jumps over the lazy dog"
      "pack my box with five dozen liquor jugs"
      16
  where
    test :: String -> String -> Int -> Bool
    test a b x = snd (lcs empty a b) == x
