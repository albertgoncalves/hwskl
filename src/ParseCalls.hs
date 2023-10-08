import Data.Char (isAlpha, isDigit, isSpace)
import Data.List (intercalate)
import Text.ParserCombinators.ReadP
  ( ReadP,
    chainl1,
    char,
    eof,
    get,
    look,
    munch,
    munch1,
    pfail,
    readP_to_S,
    sepBy,
    (<++),
  )
import Text.Printf (printf)

data Expr
  = ExprCall Expr [Expr]
  | ExprIdent String
  | ExprInt Int
  | ExprTuple [Expr]

instance Show Expr where
  show (ExprCall func []) = printf "%s()" (show func)
  show (ExprCall func args) =
    printf "%s(%s)" (show func) (intercalate ", " $ map show args)
  show (ExprIdent ident) = ident
  show (ExprInt int) = show int
  show (ExprTuple exprs) = printf "(%s)" $ intercalate ", " $ map show exprs

choice :: [ReadP a] -> ReadP a
choice [] = pfail
choice [p] = p
choice (p : ps) = p <++ choice ps

space :: ReadP ()
space = do
  rest <- look
  case rest of
    ('#' : _) -> get *> munch (/= '\n') *> ((char '\n' *> space) <++ eof)
    (c : _) | isSpace c -> get *> space
    _ -> return ()

token :: ReadP a -> ReadP a
token = (space *>)

tokenChar :: Char -> ReadP Char
tokenChar = token . char

parens :: ReadP a -> ReadP a
parens p = tokenChar '(' *> anyParens p <* tokenChar ')'

anyParens :: ReadP a -> ReadP a
anyParens p = p <++ parens p

exprIdent :: ReadP Expr
exprIdent = ExprIdent <$> token (munch1 isAlpha)

exprInt :: ReadP Expr
exprInt = ExprInt . read <$> token (munch1 isDigit)

exprCall :: ReadP Expr
exprCall =
  foldl ExprCall
    <$> exprCallFunc
    <*> (tokenChar '(' *> ((: []) <$> exprCallArgs) <* tokenChar ')')
      `chainl1` pure (++)
  where
    exprCallFunc :: ReadP Expr
    exprCallFunc = exprIdent <++ parens expr

    exprCallArgs :: ReadP [Expr]
    exprCallArgs = expr `sepBy` tokenChar ','

exprTuple :: ReadP Expr
exprTuple = do
  _ <- tokenChar '('
  e <- expr
  es <- (tokenChar ',' *> ((: []) <$> expr)) `chainl1` pure (++)
  _ <- tokenChar ')'
  return $ ExprTuple $ e : es

expr :: ReadP Expr
expr = choice [exprCall, exprTuple, exprIdent, exprInt, parens expr]

main :: IO ()
main =
  mapM_ (print . fst) $
    readP_to_S
      (expr <* token eof)
      " f #\n ( x , y ) ( z , ((0 , 1)) , 2 #\n ) #"
