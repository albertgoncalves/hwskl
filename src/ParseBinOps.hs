import Data.Char (isDigit)
import Text.ParserCombinators.ReadP
  ( ReadP,
    chainl1,
    char,
    eof,
    munch1,
    option,
    readP_to_S,
    skipSpaces,
    string,
    (<++),
  )
import Text.Printf (printf)

data Expr
  = ExprBinOp Op Expr Expr
  | ExprInt Int

data Op
  = OpAdd
  | OpMul
  | OpSub

instance Show Expr where
  show (ExprBinOp op left right) =
    printf "(%s %s %s)" (show left) (show op) (show right)
  show (ExprInt int) = show int

instance Show Op where
  show OpAdd = "+"
  show OpMul = "*"
  show OpSub = "-"

parens :: ReadP a -> ReadP a
parens p = token (char '(') *> anyParens p <* token (char ')')

anyParens :: ReadP a -> ReadP a
anyParens p = p <++ parens p

token :: ReadP a -> ReadP a
token = (skipSpaces *>)

exprInt :: ReadP Expr
exprInt =
  ExprInt . read
    <$> token ((++) <$> option "" (string "-") <*> munch1 isDigit)

exprBinOpAddSub :: ReadP Expr
exprBinOpAddSub = chainl1 exprBinOpMul $ add <++ sub
  where
    add = ExprBinOp OpAdd <$ token (char '+')
    sub = ExprBinOp OpSub <$ token (char '-')

exprBinOpMul :: ReadP Expr
exprBinOpMul = chainl1 exprAtom $ ExprBinOp OpMul <$ token (char '*')

exprAtom :: ReadP Expr
exprAtom = exprInt <++ parens expr

expr :: ReadP Expr
expr = exprBinOpAddSub <++ exprAtom

main :: IO ()
main = mapM_ (print . fst) $ readP_to_S (expr <* token eof) "1 - -2 * 3 + 4"
