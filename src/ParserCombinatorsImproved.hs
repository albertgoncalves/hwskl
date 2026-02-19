{-# LANGUAGE OverloadedStrings #-}

-- NOTE: See `https://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf`.
-- NOTE: See `https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/parsec-paper-letter.pdf`.
-- NOTE: See `https://www.researchgate.net/publication/2534571_Parsec_Direct_Style_Monadic_Parser_Combinators_For_The_Real_World`.

import Control.Applicative (Alternative, empty, (<|>))
import Data.Bifunctor (bimap, first)
import Data.Char (isDigit, ord)
import Data.Semigroup (Min (..))
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

type Pos a = (Min Int, a)

instance Functor Consumed where
  fmap f (Consumed x) = Consumed $ first f <$> x
  fmap f (Empty x) = Empty $ first f <$> x

instance Functor Parser where
  fmap f p = Parser $ fmap f . parse p

instance Applicative Parser where
  pure x = Parser $ \i -> Empty (Right (x, i))
  p1 <*> p2 = Parser $ \i ->
    case parse p1 i of
      Consumed (Right (f, i')) ->
        case parse p2 i' of
          Empty x' -> Consumed $ first f <$> x'
          c -> f <$> c
      Consumed (Left n) -> Consumed $ Left n
      Empty (Right (f, i')) -> f <$> parse p2 i'
      Empty (Left n) -> Empty $ Left n

instance Monad Parser where
  p >>= f = Parser $ \i -> case parse p i of
    Consumed (Right (x, i')) -> case parse (f x) i' of
      Consumed x' -> Consumed x'
      Empty x' -> Consumed x'
    Consumed (Left n) -> Consumed (Left n)
    Empty (Right (x, i')) -> parse (f x) i'
    Empty (Left n) -> Empty (Left n)

instance Alternative Parser where
  empty = Parser $ \i -> Empty $ Left $ pos i

  -- NOTE: If `p` succeeds without consuming `i` the second alternative is
  -- favored _if_ it consumes `i`. This implements the "longest match" rule.
  p1 <|> p2 = Parser $ \i -> case parse p1 i of
    Empty (Left _) -> parse p2 i
    Empty ok ->
      case parse p2 i of
        Empty _ -> Empty ok
        c -> c
    c -> c

satisfy :: (Char -> Bool) -> Parser (Pos Char)
satisfy f = Parser $ \i ->
  case uncons $ rest i of
    Just (x, rest')
      | f x ->
          let p = pos i + 1
           in Consumed (Right ((Min p, x), Input p rest'))
      | otherwise -> Empty $ Left $ pos i
    Nothing -> Empty $ Left $ pos i

end :: Parser ()
end = Parser $ \i ->
  if null $ rest i
    then Empty $ Right ((), i)
    else Empty $ Left (pos i)

many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> (many1 p <|> pure [])

many :: Parser a -> Parser [a]
many p = many1 p <|> pure []

sepby1 :: Parser a -> Parser b -> Parser [a]
sepby1 p sep = (:) <$> p <*> many (sep *> p)

sepby :: Parser a -> Parser b -> Parser [a]
sepby p sep = p `sepby1` sep <|> pure []

char :: Char -> Parser (Pos Char)
char = satisfy . (==)

string :: String -> Parser (Pos String)
string = (sequenceA <$>) . traverse char

main :: IO ()
main = do
  mapM_
    (print . parse (string "foo" `sepby1` string ", ") . Input 0)
    ["foo, foo", "foo, foo, "]
  mapM_ (print . parse (int' <* end) . Input 0) ["-1234", "-1234a"]
  mapM_
    (print . parse (many ints) . Input 0)
    [ "[1,2,3][1,2][1,2][1,2,3]",
      "[1,2,3][1,2][1,2][1,2,3,]"
    ]
  where
    digit0 :: Int
    digit0 = ord '0'

    nat :: Parser (Pos Int)
    nat =
      bimap mconcat (foldl' (\b a -> (b * 10) + (ord a - digit0)) 0) . unzip
        <$> many1 (satisfy isDigit)

    int' :: Parser (Pos Int)
    int' = ((negate <$>) <$> (char '-' *> nat)) <|> nat

    ints :: Parser [Pos Int]
    ints = char '[' *> int' `sepby` char ',' <* char ']'
