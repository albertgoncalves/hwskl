import Data.Char (isAlpha, isDigit, isSpace)
import Data.List (intercalate)
import Text.ParserCombinators.ReadP
  ( ReadP,
    char,
    eof,
    get,
    look,
    many1,
    munch,
    munch1,
    pfail,
    readP_to_S,
    sepBy,
    (<++),
  )
import Text.Printf (printf)

data Expr
  = ExprAccess Expr Int
  | ExprBox
  | ExprCall Expr [Expr]
  | ExprIdent String
  | ExprInt Int
  | ExprTuple [Expr]

instance Show Expr where
  show (ExprAccess expr offset) = printf "%s[%d]" (show expr) offset
  show ExprBox = "[]"
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

parse :: String -> [(Expr, String)]
parse = readP_to_S (expr <* token eof)
  where
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

    int :: ReadP Int
    int = read <$> token (munch1 isDigit)

    exprInt :: ReadP Expr
    exprInt = ExprInt <$> int

    exprCallArgs :: ReadP (Either Int [Expr])
    exprCallArgs =
      ( tokenChar '('
          *> (Right <$> (expr `sepBy` tokenChar ','))
          <* tokenChar ')'
      )
        <++ (tokenChar '[' *> (Left <$> int) <* tokenChar ']')

    exprCall :: ReadP Expr
    exprCall =
      foldl (flip $ either (flip ExprAccess) (flip ExprCall))
        <$> exprAtom
        <*> many1 exprCallArgs

    exprTuple :: ReadP Expr
    exprTuple =
      ExprTuple
        <$> ( (:)
                <$> (tokenChar '(' *> expr)
                <*> (many1 (tokenChar ',' *> expr) <* tokenChar ')')
            )

    exprBox :: ReadP Expr
    exprBox = ExprBox <$ tokenChar '[' <* tokenChar ']'

    exprAtom :: ReadP Expr
    exprAtom = choice [exprTuple, exprIdent, exprInt, exprBox, parens expr]

    expr :: ReadP Expr
    expr = exprCall <++ exprAtom

main :: IO ()
main =
  mapM_ (print . fst) $
    parse
      " f #\n ( x , y()[1](), [ ] ( 1 ) ) ( z , ((0 , 1)) , 2 #\n ) #"
