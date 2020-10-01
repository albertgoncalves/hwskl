distance :: String -> String -> Word
distance [] b = fromIntegral $ length b
distance a [] = fromIntegral $ length a
distance as'@(a : as) bs'@(b : bs)
  | a == b = distance as bs
  | otherwise =
    minimum
      [ distance as' bs,
        distance as bs',
        distance as bs
      ]
      + 1

test :: String -> String -> Word -> Bool
test a b = (==) (distance a b)

main :: IO ()
main = do
  print $ test "foobar" "" 6
  print $ test "sitting" "kitten" 3
  print $ test "flaw" "lawn" 2
  print $ test "saturday" "sunday" 3
  print $ test "gumbo" "gambol" 2
  print $ test "book" "back" 2
  print $ test "edward" "edwin" 3
