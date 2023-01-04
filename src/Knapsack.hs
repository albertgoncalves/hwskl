import Data.Array (Array, listArray, (!))
import Data.Map (Map, empty, insert, lookup)
import Prelude hiding (lookup)

type Memo = Map (Int, Int) Int

knapsackRec :: Array Int Int -> Array Int Int -> Int -> Int -> Int
knapsackRec vs ws n w
  | (n == 0) || (w == 0) = 0
  | w < (ws ! (n - 1)) = knapsackRec vs ws (n - 1) w
  | otherwise =
      max
        ((vs ! (n - 1)) + knapsackRec vs ws (n - 1) (w - (ws ! (n - 1))))
        (knapsackRec vs ws (n - 1) w)

knapsackMemo ::
  Memo ->
  Array Int Int ->
  Array Int Int ->
  Int ->
  Int ->
  (Memo, Int)
knapsackMemo m vs ws n w =
  case lookup k m of
    Just v -> (m, v)
    Nothing | (n == 0) || (w == 0) -> (insert k 0 m, 0)
    Nothing
      | w < (ws ! (n - 1)) ->
          let (m0, v) = knapsackMemo m vs ws (n - 1) w
           in (insert k v m0, v)
    Nothing ->
      let (m0, v0) = knapsackMemo m vs ws (n - 1) (w - (ws ! (n - 1)))
          (m1, v1) = knapsackMemo m0 vs ws (n - 1) w
          v2 = max ((vs ! (n - 1)) + v0) v1
       in (insert k v2 m1, v2)
  where
    k = (n, w)

main :: IO ()
main =
  mapM_
    (\f -> print $ f (listArray (0, n') vs) (listArray (0, n') ws) n w)
    [knapsackRec, \x y z -> snd . knapsackMemo empty x y z]
  where
    vs = [321, 942, 339, 31, 463, 149, 50, 745, 395, 377 :: Int]
    ws = [288, 853, 291, 102, 529, 90, 132, 717, 417, 285 :: Int]
    w = 2000 :: Int
    n = length vs
    n' = n - 1
