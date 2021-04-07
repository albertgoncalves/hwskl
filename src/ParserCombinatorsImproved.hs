{-# LANGUAGE OverloadedStrings #-}

-- NOTE: See `https://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf`.
-- NOTE: See `https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/parsec-paper-letter.pdf`.
-- NOTE: See `https://www.researchgate.net/publication/2534571_Parsec_Direct_Style_Monadic_Parser_Combinators_For_The_Real_World`.

import Data.Char (isDigit, ord)
import Data.List (foldl')
import Data.Text (Text, null, uncons)
import Prelude hiding (null)

data Input = Input
  { pos :: Int,
    rest :: Text
  }
  deriving (Eq, Show)

type Reply a = Either Int (a, Input)

data Consumed a
  = Consumed (Reply a)
  | Empty (Reply a)
  deriving (Show)

newtype Parser a = Parser
  { parse :: Input -> Consumed a
  }

return' :: a -> Parser a
return' x = Parser $ \i -> Empty (Right (x, i))

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \i -> case parse p i of
  Consumed (Right (x, i')) -> case parse (f x) i' of
    Consumed r -> Consumed r
    Empty r -> Consumed r
  Consumed (Left i'') -> Consumed (Left i'')
  Empty (Right (x, i')) -> parse (f x) i'
  Empty (Left i'') -> Empty (Left i'')

-- NOTE: If `p` succeeds without consuming `i` the second alternative is
-- favored _if_ it consumes `i`. This implements the "longest match" rule.
or' :: Parser a -> Parser a -> Parser a
or' p1 p2 = Parser $ \i -> case parse p1 i of
  Empty (Left _) -> parse p2 i
  Empty ok ->
    case parse p2 i of
      Empty _ -> Empty ok
      c -> c
  c -> c

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \i ->
  case uncons $ rest i of
    Nothing -> Empty $ Left $ pos i
    Just (x, rest')
      | f x -> Consumed (Right (x, Input (pos i + 1) rest'))
      | otherwise -> Empty $ Left $ pos i

many1 :: Parser a -> Parser [a]
many1 p =
  p `bind` \x ->
    (many1 p `or'` return' []) `bind` \xs ->
      return' (x : xs)

many :: Parser a -> Parser [a]
many p = many1 p `or'` return' []

sepby1 :: Parser a -> Parser b -> Parser [a]
sepby1 p sep =
  p `bind` \x ->
    many
      ( sep `bind` \_ ->
          p `bind` return'
      )
      `bind` \xs ->
        return' (x : xs)

sepby :: Parser a -> Parser b -> Parser [a]
sepby p sep = sepby1 p sep `or'` return' []

char :: Char -> Parser Char
char = satisfy . (==)

string :: String -> Parser String
string [] = return' []
string (x : xs) =
  char x `bind` \x' ->
    string xs `bind` \xs' ->
      return' (x' : xs')

digit0 :: Int
digit0 = ord '0'

nat :: Parser Int
nat =
  many1 (satisfy isDigit)
    `bind` ( return'
               . foldl' (\a b -> (a * 10) + b) 0
               . map (\x -> ord x - digit0)
           )

int' :: Parser Int
int' =
  ( char '-' `bind` \_ ->
      nat `bind` \x ->
        return' (- x)
  )
    `or'` nat

end :: Parser ()
end = Parser $ \i ->
  if null $ rest i
    then Empty $ Right ((), i)
    else Empty $ Left (pos i)

main :: IO ()
main = do
  print $ parse (sepby1 (string "foo") (string ", ")) $ Input 0 "foo, foo, "
  print $ parse int' $ Input 0 "-1234"
  print $
    parse (int' `bind` \x -> end `bind` \_ -> return' x) $ Input 0 "-1234a"
