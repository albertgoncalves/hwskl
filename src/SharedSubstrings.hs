import Data.List (intersect)

infix 8 .:

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f .: g) x y = f $ g x y

sharedSubstrings :: String -> String -> Bool
sharedSubstrings = not . null .: intersect

main :: IO ()
main = do
  print $ sharedSubstrings "hello" "world"
  print $ sharedSubstrings "hi" "world"
