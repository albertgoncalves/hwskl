import Control.Applicative ((<|>), Alternative, empty, many)
import Data.Char (isDigit, isSpace)

{- NOTE: See `https://www.youtube.com/watch?v=N9RUqGYuGfw`. -}

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Int
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show)

newtype Parser a
  = Parser
      { runParser :: String -> Maybe (String, a)
      }

instance Functor Parser where
  fmap f (Parser a) = Parser $ \xs -> do
    (xs', x) <- a xs
    Just (xs', f x)

instance Applicative Parser where
  pure x = Parser $ \xs -> Just (xs, x)
  (Parser f) <*> (Parser a) = Parser $ \xs -> do
    (xs', f') <- f xs
    (xs'', a') <- a xs'
    Just (xs'', f' a')

instance Alternative Parser where
  empty = Parser (const Nothing)
  (Parser p1) <|> (Parser p2) = Parser $ \xs -> p1 xs <|> p2 xs

parseChar :: Char -> Parser Char
parseChar x = Parser f
  where
    f (y : ys)
      | x == y = Just (ys, y)
    f _ = Nothing

parseString :: String -> Parser String
parseString = traverse parseChar

parseSpan :: (Char -> Bool) -> Parser String
parseSpan f = Parser $ \xs ->
  case span f xs of
    ("", _) -> Nothing
    (xs', ys) -> Just (ys, xs')

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ parseString "null"

jsonBool :: Parser JsonValue
jsonBool =
  JsonBool True <$ parseString "true" <|> JsonBool False <$ parseString "false"

jsonNumber :: Parser JsonValue
jsonNumber = JsonNumber . read <$> parseSpan isDigit

parseAnyString :: Parser String
parseAnyString =
  parseChar '"' *> (parseSpan (/= '"') <|> pure "") <* parseChar '"'

jsonString :: Parser JsonValue
jsonString = JsonString <$> parseAnyString

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep item = (:) <$> item <*> many (sep *> item) <|> pure []

parseAnySpace :: Parser String
parseAnySpace = parseSpan isSpace <|> pure ""

parseCommas :: Parser Char
parseCommas = parseAnySpace *> parseChar ',' <* parseAnySpace

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (lBracket *> sepBy parseCommas jsonValue <* rBracket)
  where
    lBracket = parseChar '[' *> parseAnySpace
    rBracket = parseAnySpace <* parseChar ']'

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> (lBrace *> sepBy parseCommas pair <* rBrace)
  where
    lBrace = parseChar '{' *> parseAnySpace
    rBrace = parseAnySpace <* parseChar '}'
    pair =
      (\key _ value -> (key, value))
        <$> parseAnyString
        <*> (parseAnySpace *> parseChar ':' <* parseAnySpace)
        <*> jsonValue

jsonValue :: Parser JsonValue
jsonValue =
  jsonNull
    <|> jsonBool
    <|> jsonNumber
    <|> jsonString
    <|> jsonArray
    <|> jsonObject

main :: IO ()
main = do
  mapM_
    (print . runParser jsonValue)
    [ "",
      "null",
      "true",
      "false",
      "123",
      "\"\"",
      "\"foobar\"",
      "[[1, 2, 3], null, [true, false], \"foobar\"]",
      "[ [ 1, 2, 3 ], null, [ true, false ], \"foobar\" ]",
      "{\"key1\": \"value1\", \"key2\": null}",
      "{ \"key1\" : \"value1\" , \"key2\" : null }"
    ]
