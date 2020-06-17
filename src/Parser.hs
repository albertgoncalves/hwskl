import Control.Applicative ((<|>), Alternative, empty, many, some)
import Control.Monad ((>=>))
import Data.Char (isDigit, isSpace)

newtype Parser a
  = Parser
      { runParser :: String -> Maybe (String, a)
      }

instance Functor Parser where
  fmap f (Parser p) = Parser $ fmap (fmap f) . p

instance Applicative Parser where
  pure x = Parser $ \s -> Just (s, x)
  (Parser p1) <*> (Parser p2) = Parser $ p1 >=> \(s, f) -> fmap (fmap f) (p2 s)

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \s -> p1 s <|> p2 s

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser f'
  where
    f' (x : xs)
      | f x = Just (xs, x)
    f' _ = Nothing

anySpace :: Parser String
anySpace = many $ satisfy isSpace

data Expr
  = Lit Int
  | Add Expr Expr
  | Sub Expr Expr
  deriving (Show)

lit :: Parser Expr
lit = Lit . read <$> some (satisfy isDigit)

binOp :: (Expr -> Expr -> Expr) -> Char -> Parser Expr
binOp f x = f <$> term <*> (anySpace *> satisfy (== x) *> anySpace *> term)

add :: Parser Expr
add = binOp Add '+'

sub :: Parser Expr
sub = binOp Sub '-'

parens :: Parser Expr
parens = satisfy (== '(') *> anySpace *> expr <* anySpace <* satisfy (== ')')

term :: Parser Expr
term = lit <|> parens

expr :: Parser Expr
expr = add <|> sub <|> term

end :: Parser ()
end = Parser $ \s -> if null s then Just (s, ()) else Nothing

main :: IO ()
main = print $ runParser (expr <* end) "(1 + (2 - 3))"
