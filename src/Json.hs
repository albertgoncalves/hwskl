import Control.Applicative ((<|>), Alternative, empty, many, some)
import Control.Monad ((>=>))
import Data.Char (isDigit, isSpace)

{- NOTE: See `https://www.youtube.com/watch?v=N9RUqGYuGfw`. -}

newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}

{- NOTE:
 -  $ ghci
 -  > fmap (fmap (+ 1)) (Just (1, 2))
 -  Just (1,3)
 -}
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

matchString :: String -> Parser String
matchString = traverse $ satisfy . (==)

doubleQuote :: Parser Char
doubleQuote = satisfy (== '"')

stringLiteral :: Parser String
stringLiteral = doubleQuote *> many (satisfy (/= '"')) <* doubleQuote

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep item = (:) <$> item <*> many (sep *> item) <|> pure []

anySpace :: Parser String
anySpace = many $ satisfy isSpace

comma :: Parser Char
comma = anySpace *> satisfy (== ',') <* anySpace

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Int
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show)

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ matchString "null"

jsonBool :: Parser JsonValue
jsonBool =
  JsonBool <$> (True <$ matchString "true" <|> False <$ matchString "false")

jsonNumber :: Parser JsonValue
jsonNumber = JsonNumber . read <$> some (satisfy isDigit)

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (lBracket *> sepBy comma jsonValue <* rBracket)
  where
    lBracket = satisfy (== '[') *> anySpace
    rBracket = anySpace <* satisfy (== ']')

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> (lBrace *> sepBy comma pair <* rBrace)
  where
    pair =
      (,)
        <$> stringLiteral
        <*> (anySpace *> satisfy (== ':') *> anySpace *> jsonValue)
    lBrace = satisfy (== '{') *> anySpace
    rBrace = anySpace <* satisfy (== '}')

jsonValue :: Parser JsonValue
jsonValue =
  jsonNull
    <|> jsonBool
    <|> jsonNumber
    <|> jsonString
    <|> jsonArray
    <|> jsonObject

main :: IO ()
main =
  mapM_
    (print . runParser jsonValue)
    [ "",
      "null",
      "tru",
      "true",
      "fals",
      "false",
      "123",
      "\"",
      "\"\"",
      "\"foobar",
      "\"foobar\"",
      "1, 2, 3",
      "[1, 2, 3",
      "[1, 2, 3]",
      "[[1, 2, 3], null, [true, false], \"foobar\"]",
      "[ [ 1 , 2 , 3 ] , null , [ true , false ] , \"foobar\" ]",
      "{}",
      "{\"key1\": \"value1\", \"key2\": null}",
      "{ \"key1\" : \"value1\" , \"key2\" : null }"
    ]
