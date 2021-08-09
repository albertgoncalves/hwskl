import Data.Array (Array, array, elems, listArray, (!))

distance :: String -> String -> Int
distance a b
  | n == 0 = m
  | m == 0 = n
  | otherwise = last $ elems table
  where
    n = length a
    m = length b
    a' = listArray (0, n - 1) a
    b' = listArray (0, m - 1) b

    table :: Array (Int, Int) Int
    table =
      array ((0, 0), (n, m)) [((i, j), f i j) | i <- [0 .. n], j <- [0 .. m]]

    f :: Int -> Int -> Int
    f i 0 = i
    f 0 j = j
    f i j
      | (a' ! (i - 1)) == (b' ! (j - 1)) = table ! (i - 1, j - 1)
      | otherwise =
        1 + minimum (map (table !) [(i, j - 1), (i - 1, j), (i - 1, j - 1)])

main :: IO ()
main =
  mapM_
    print
    [ test "foobar" "" 6,
      test "" "foobar" 6,
      test "sitting" "kitten" 3,
      test "flaw" "lawn" 2,
      test "saturday" "sunday" 3,
      test "gumbo" "gambol" 2,
      test "book" "back" 2,
      test "edward" "edwin" 3,
      test "what is a sentence" "this is another thing" 13,
      test
        "the quick brown fox jumps over the lazy dog"
        "pack my box with five dozen liquor jugs"
        33
    ]
  where
    test :: String -> String -> Int -> Bool
    test a b x = distance a b == x
