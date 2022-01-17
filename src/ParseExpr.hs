import Control.Applicative ((<|>))
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.List (intercalate)
import Text.ParserCombinators.ReadP
  ( ReadP,
    char,
    eof,
    munch,
    munch1,
    option,
    readP_to_S,
    satisfy,
    sepBy,
    skipSpaces,
    string,
  )
import Text.Printf (printf)

data UnOp
  = UnOpBang
  | UnOpTilda

data BinOp
  = BinOpAdd
  | BinOpSub
  | BinOpMul
  | BinOpDiv

data Ast
  = AstInt Int
  | AstUnOp UnOp Ast
  | AstBinOp BinOp Ast Ast
  | AstVar String
  | AstCall String [Ast]

instance Show UnOp where
  show UnOpBang = "!"
  show UnOpTilda = "~"

instance Show BinOp where
  show BinOpAdd = "+"
  show BinOpSub = "-"
  show BinOpMul = "*"
  show BinOpDiv = "/"

instance Show Ast where
  show (AstInt x) = show x
  show (AstUnOp op x) = printf "(%s%s)" (show op) (show x)
  show (AstBinOp op l r) = printf "(%s %s %s)" (show l) (show op) (show r)
  show (AstVar x) = x
  show (AstCall x xs) = printf "%s(%s)" x $ intercalate ", " $ map show xs

integer :: ReadP Ast
integer =
  (AstInt .) . (read .) . (++) <$> option "" (string "-") <*> munch1 isDigit

spaces :: ReadP a -> ReadP a
spaces p = skipSpaces *> p <* skipSpaces

parens :: ReadP a -> ReadP a
parens p = char '(' *> spaces p <* char ')'

unOp :: ReadP Ast
unOp = parens $ foldr1 (<|>) $ map f [(UnOpBang, '!'), (UnOpTilda, '~')]
  where
    f (op, c) = AstUnOp op <$> (char c *> expr)

binOp :: ReadP Ast
binOp =
  parens $
    foldr1 (<|>) $
      map
        f
        [ (BinOpAdd, '+'),
          (BinOpSub, '-'),
          (BinOpMul, '*'),
          (BinOpDiv, '/')
        ]
  where
    f (op, s) = AstBinOp op <$> expr <*> (string s *> expr)

ident :: ReadP String
ident = (:) <$> satisfy isAlpha <*> munch f
  where
    f x = isAlphaNum x || x == '_'

var :: ReadP Ast
var = AstVar <$> ident

call :: ReadP Ast
call = AstCall <$> ident <*> parens (sepBy expr $ char ',')

expr :: ReadP Ast
expr = spaces $ foldr1 (<|>) [integer, var, call, unOp, binOp, parens expr]

parse :: String -> [Ast]
parse = map fst . filter (null . snd) . readP_to_S (expr <* eof)

main :: IO ()
main =
  mapM_ print $
    parse "(~ ( f0(-1, y   ,\n 0) * ( ( !((\n~-4 ) /(x_))) + 1) )\n)"
