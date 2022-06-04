import Data.Char (isAlphaNum, isDigit, isLower, isSpace)

-- NOTE: See `https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html`.

data UnOp
  = UnOpMinus
  | UnOpBang
  | UnOpTilda
  deriving (Show)

data BinOp
  = BinOpAdd
  | BinOpSub
  | BinOpMul
  | BinOpDiv
  | BinOpEq
  deriving (Show)

data Ast
  = AstUnOp UnOp Ast
  | AstBinOp BinOp Ast Ast
  | AstInt Int
  | AstVar String
  | AstAccess Ast [Ast]
  | AstCall Ast [Ast]
  deriving (Show)

data Token
  = TokenEnd
  | TokenLParen
  | TokenRParen
  | TokenLBracket
  | TokenRBracket
  | TokenComma
  | TokenAdd
  | TokenMul
  | TokenSub
  | TokenInt Int
  | TokenIdent String
  deriving (Show)

tokenize :: String -> [Token]
tokenize [] = []
tokenize ('(' : xs) = TokenLParen : tokenize xs
tokenize (')' : xs) = TokenRParen : tokenize xs
tokenize ('[' : xs) = TokenLBracket : tokenize xs
tokenize (']' : xs) = TokenRBracket : tokenize xs
tokenize (',' : xs) = TokenComma : tokenize xs
tokenize ('+' : xs) = TokenAdd : tokenize xs
tokenize ('*' : xs) = TokenMul : tokenize xs
tokenize ('-' : xs) = TokenSub : tokenize xs
tokenize (x : xs)
  | isDigit x =
    let (as, bs) = span isDigit xs in TokenInt (read $ x : as) : tokenize bs
  | isLower x =
    let (as, bs) = span isAlphaNum xs in TokenIdent (x : as) : tokenize bs
  | isSpace x = tokenize $ dropWhile isSpace xs
  | otherwise = undefined

precPrefix :: Token -> Either Token (Int, UnOp)
precPrefix TokenSub = Right (9, UnOpMinus)
precPrefix x = Left x

precInfix :: Token -> Either Token (Int, Int, BinOp)
precInfix TokenAdd = Right (5, 6, BinOpAdd)
precInfix TokenMul = Right (7, 8, BinOpMul)
precInfix x = Left x

precParen :: Int
precParen = 11

precBracket :: Int
precBracket = 11

parseLeft :: [Token] -> Either Token (Ast, [Token])
parseLeft [] = Left TokenEnd
parseLeft (TokenLParen : xs0) = do
  r <- parse xs0 0
  case r of
    (expr, TokenRParen : xs1) -> Right (expr, xs1)
    (_, x : _) -> Left x
    _ -> Left TokenEnd
parseLeft (TokenIdent x : xs) = Right (AstVar x, xs)
parseLeft (TokenInt x : xs) = Right (AstInt x, xs)
parseLeft (x : xs0) = do
  (prec, op) <- precPrefix x
  (expr, xs1) <- parse xs0 prec
  return (AstUnOp op expr, xs1)

parseArgs :: [Token] -> Either Token ([Ast], [Token])
parseArgs [] = Left TokenEnd
parseArgs xs0 = do
  r <- parse xs0 0
  case r of
    (arg, TokenComma : xs1) -> do
      (args, xs2) <- parseArgs xs1
      return (arg : args, xs2)
    (arg, xs1) -> Right ([arg], xs1)

parseRight :: Ast -> [Token] -> Int -> Either Token (Ast, [Token])
parseRight expr [] _ = Right (expr, [])
parseRight expr (TokenLParen : TokenRParen : xs) prec =
  parseRight (AstCall expr []) xs prec
parseRight expr xs0'@(TokenLParen : xs0) prec =
  if precParen < prec
    then Right (expr, xs0')
    else do
      r <- parseArgs xs0
      case r of
        (args, TokenRParen : xs1) ->
          parseRight (AstCall expr args) xs1 prec
        (_, x : _) -> Left x
        (_, []) -> Left TokenEnd
parseRight expr xs0'@(TokenLBracket : xs0) prec =
  if precBracket < prec
    then Right (expr, xs0')
    else do
      r <- parseArgs xs0
      case r of
        (args, TokenRBracket : xs1) ->
          parseRight (AstAccess expr args) xs1 prec
        (_, x : _) -> Left x
        (_, []) -> Left TokenEnd
parseRight left xs0'@(x : xs0) prec =
  case precInfix x of
    Right (precInfixL, precInfixR, op) ->
      if precInfixL < prec
        then Right (left, xs0')
        else do
          (right, xs1) <- parse xs0 precInfixR
          parseRight (AstBinOp op left right) xs1 prec
    Left _ -> Right (left, xs0')

parse :: [Token] -> Int -> Either Token (Ast, [Token])
parse xs0 prec0 = do
  r0 <- parseLeft xs0
  case r0 of
    (expr, []) -> Right (expr, [])
    (left, xs1) -> parseRight left xs1 prec0

main :: IO ()
main = print $ parse (tokenize "f0()(f1(), x)[0] + -1 * -2") 0