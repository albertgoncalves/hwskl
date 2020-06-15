import Control.Applicative ((<|>), Alternative, empty, many, some)
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

parseCondition :: (Char -> Bool) -> Parser Char
parseCondition f = Parser f'
  where
    f' (x : xs)
      | f x = Just (xs, x)
    f' _ = Nothing

parseChar :: Char -> Parser Char
parseChar = parseCondition . (==)

parseString :: String -> Parser String
parseString = traverse parseChar

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ parseString "null"

jsonBool :: Parser JsonValue
jsonBool =
  JsonBool <$> (True <$ parseString "true" <|> False <$ parseString "false")

jsonNumber :: Parser JsonValue
jsonNumber = JsonNumber . read <$> some (parseCondition isDigit)

anyStringLiteral :: Parser String
anyStringLiteral =
  parseChar '"' *> many (parseCondition (/= '"')) <* parseChar '"'

jsonString :: Parser JsonValue
jsonString = JsonString <$> anyStringLiteral

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep item = (:) <$> item <*> many (sep *> item) <|> pure []

anySpace :: Parser String
anySpace = many $ parseCondition isSpace

comma :: Parser Char
comma = anySpace *> parseChar ',' <* anySpace

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (lBracket *> sepBy comma jsonValue <* rBracket)
  where
    lBracket = parseChar '[' *> anySpace
    rBracket = anySpace <* parseChar ']'

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> (lBrace *> sepBy comma pair <* rBrace)
  where
    lBrace = parseChar '{' *> anySpace
    rBrace = anySpace <* parseChar '}'
    pair =
      (\key _ value -> (key, value))
        <$> anyStringLiteral
        <*> (anySpace *> parseChar ':' <* anySpace)
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
