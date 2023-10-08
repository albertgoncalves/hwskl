import Data.Char (isAlpha, isDigit, isSpace)
import Text.ParserCombinators.ReadP
  ( ReadP,
    chainl1,
    char,
    eof,
    get,
    look,
    munch,
    munch1,
    readP_to_S,
    sepBy,
    (<++),
  )
import Text.Printf (printf)

data Expr
  = ExprCall Expr [Expr]
  | ExprIdent String
  | ExprInt Int

instance Show Expr where
  show (ExprCall func []) = printf "(%s)" (show func)
  show (ExprCall func args) =
    printf "(%s %s)" (show func) (unwords $ map show args)
  show (ExprIdent ident) = ident
  show (ExprInt int) = show int

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

exprAtom :: ReadP Expr
exprAtom = token $ exprIdent <++ exprInt
  where
    exprIdent :: ReadP Expr
    exprIdent = ExprIdent <$> munch1 isAlpha

    exprInt :: ReadP Expr
    exprInt = ExprInt . read <$> munch1 isDigit

exprCall :: ReadP Expr
exprCall =
  foldl ExprCall
    <$> exprCallFunc
    <*> chainl1
      (tokenChar '(' *> ((: []) <$> exprCallArgs) <* tokenChar ')')
      (pure (++))
  where
    exprCallFunc :: ReadP Expr
    exprCallFunc = exprAtom <++ parens expr

    exprCallArgs :: ReadP [Expr]
    exprCallArgs = expr `sepBy` tokenChar ','

expr :: ReadP Expr
expr = exprCall <++ exprAtom <++ parens expr

main :: IO ()
main =
  mapM_ (print . fst) $
    readP_to_S (expr <* token eof) " f #\n ( x , y ) ( z , 0 , 1 #\n ) #"
