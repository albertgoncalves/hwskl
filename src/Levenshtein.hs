import Data.Map.Strict (Map, empty, insert, lookup, size)
import Prelude hiding (lookup)

distance ::
  Map (String, String) Int ->
  String ->
  String ->
  (Map (String, String) Int, Int)
distance m as@[] bs = case lookup (as, bs) m of
  Just x -> (m, x)
  Nothing -> let x = length bs in (insert (as, bs) x m, x)
distance m as bs@[] = case lookup (as, bs) m of
  Just x -> (m, x)
  Nothing -> let x = length as in (insert (as, bs) x m, x)
distance m as'@(a : as) bs'@(b : bs) = case lookup (as', bs') m of
  Just x -> (m, x)
  Nothing -> (insert (as', bs') x m3, x)
    where
      (m1, delete) = distance m as' bs
      (m2, insert') = distance m1 as bs'
      (m3, replace) = distance m2 as bs
      x = minimum [delete + 1, insert' + 1, replace + if a /= b then 1 else 0]

test :: Map (String, String) Int -> String -> String -> Int -> (Bool, Int)
test m a b x = (x == x', size m')
  where
    (m', x') = distance m a b

main :: IO ()
main =
  mapM_
    print
    [ test empty "foobar" "" 6,
      test empty "sitting" "kitten" 3,
      test empty "flaw" "lawn" 2,
      test empty "saturday" "sunday" 3,
      test empty "gumbo" "gambol" 2,
      test empty "book" "back" 2,
      test empty "edward" "edwin" 3,
      test
        empty
        "the quick brown fox jumps over the lazy dog"
        "pack my box with five dozen liquor jugs"
        33
    ]
