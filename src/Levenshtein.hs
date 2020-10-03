import Data.Map.Strict (Map, empty, insert, lookup, size)
import Prelude hiding (lookup)

type Memo = Map (String, String) Int

distance :: Memo -> String -> String -> (Memo, Int)
distance m as bs = case lookup (as, bs) m of
  Just x -> (m, x)
  Nothing -> (insert (as, bs) x m', x)
    where
      (m', x) = case (as, bs) of
        ([], _) -> (m, length bs)
        (_, []) -> (m, length as)
        (a : as', b : bs') ->
          if a == b
            then (m0, x0)
            else
              let (m1, x1) = distance m0 as bs'
               in let (m2, x2) = distance m1 as' bs
                   in (m2, 1 + minimum [x0, x1, x2])
          where
            (m0, x0) = distance m as' bs'

test :: Memo -> String -> String -> Int -> (Bool, Int)
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
      test empty "what is a sentence" "this is another thing" 13,
      test
        empty
        "the quick brown fox jumps over the lazy dog"
        "pack my box with five dozen liquor jugs"
        33
    ]
