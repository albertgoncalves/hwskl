{-# LANGUAGE LambdaCase #-}

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
  = ExprAccess Expr Int
  | ExprCall Expr [Expr]
  | ExprIdent String
  | ExprInt Int
  | ExprTuple [Expr]

data Postfix
  = PostfixBrace Int
  | PostfixParen [Expr]

instance Show Expr where
  show (ExprAccess expr offset) = printf "%s[%d]" (show expr) offset
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

    exprCall :: ReadP Expr
    exprCall =
      foldl
        ( \exprLeft -> \case
            PostfixBrace offset -> ExprAccess exprLeft offset
            PostfixParen exprs -> ExprCall exprLeft exprs
        )
        <$> exprCallFunc
        <*> ((: []) <$> exprCallArgs) `chainl1` pure (++)
      where
        exprCallFunc :: ReadP Expr
        exprCallFunc = exprIdent <++ parens expr

        exprCallArgs :: ReadP Postfix
        exprCallArgs =
          choice
            [ tokenChar '(' *> exprCallParen <* tokenChar ')',
              tokenChar '[' *> exprCallBrace <* tokenChar ']'
            ]

        exprCallParen :: ReadP Postfix
        exprCallParen = PostfixParen <$> (expr `sepBy` tokenChar ',')

        exprCallBrace :: ReadP Postfix
        exprCallBrace = PostfixBrace <$> int

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
    parse
      " f #\n ( x , y()[1]() ) ( z , ((0 , 1)) , 2 #\n ) #"
