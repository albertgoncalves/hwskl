import Control.Applicative (Alternative, empty, many, some, (<|>))
import Control.Monad ((>=>))
import Data.Char (isAlpha, isSpace)

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

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep item = (:) <$> item <*> many (sep *> item) <|> pure []

anySpace :: Parser String
anySpace = many $ satisfy isSpace

data SExpr
  = Atom String
  | Expr [SExpr]
  deriving (Show)

atom :: Parser SExpr
atom = Atom <$> some (satisfy isAlpha)

expr :: Parser SExpr
expr =
  Expr
    <$> (satisfy (== '(') *> sepBy anySpace (atom <|> expr) <* satisfy (== ')'))

end :: Parser ()
end = Parser $ \s -> if null s then pure (s, ()) else empty

main :: IO ()
main = mapM_ print $ runParser (expr <* end) "(g c (f a b))"
