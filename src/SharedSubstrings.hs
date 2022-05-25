import Data.List (intersect)

sharedSubstrings :: String -> String -> Bool
sharedSubstrings = (not .) . (null .) . intersect

main :: IO ()
main = do
  print $ sharedSubstrings "hello" "world"
  print $ sharedSubstrings "hi" "world"
