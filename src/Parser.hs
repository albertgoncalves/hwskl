import Control.Applicative ((<|>), Alternative, empty, many, some)
import Control.Monad ((>=>))
import Data.Char (isDigit, isSpace)

newtype Parser a = Parser {runParser :: String -> [(String, a)]}

instance Functor Parser where
  fmap f (Parser p) = Parser $ fmap (fmap f) . p

instance Applicative Parser where
  pure x = Parser $ \s -> pure (s, x)
  (Parser p1) <*> (Parser p2) = Parser $ p1 >=> \(s, f) -> fmap (fmap f) (p2 s)

instance Alternative Parser where
  empty = Parser $ const empty
  (Parser p1) <|> (Parser p2) = Parser $ \s -> p1 s <|> p2 s

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser f'
  where
    f' (x : xs)
      | f x = pure (xs, x)
    f' _ = empty

anySpace :: Parser String
anySpace = many $ satisfy isSpace

data Expr
  = Lit Int
  | Add Expr Expr
  | Sub Expr Expr
  deriving (Show)

term :: Parser Expr
term =
  Lit . read <$> some (satisfy isDigit)
    <|> satisfy (== '(') *> anySpace *> expr <* anySpace <* satisfy (== ')')

binOp :: (Expr -> Expr -> Expr) -> Char -> Parser Expr
binOp f x = f <$> term <*> (anySpace *> satisfy (== x) *> anySpace *> term)

expr :: Parser Expr
expr = binOp Add '+' <|> binOp Sub '-' <|> term

end :: Parser ()
end = Parser $ \s -> if null s then pure (s, ()) else empty

main :: IO ()
main = print $ runParser (expr <* end) "(1 + (2 - 3))"
