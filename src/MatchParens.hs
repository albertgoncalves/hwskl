solve :: [Char] -> Bool
solve = null . foldr f []
  where
    f :: Char -> [Char] -> [Char]
    f '(' (')' : xs) = xs
    f '{' ('}' : xs) = xs
    f '[' (']' : xs) = xs
    f x xs = x : xs

main :: IO ()
main =
  mapM_
    (print . solve)
    [ "()",
      "()[]{}",
      "([{}])",
      "(([{}])",
      "(((",
      "(]",
      "([)]"
    ]
