import Control.Exception (assert)
import Data.Char (isLower, isSpace, isUpper)

data Type
  = Var Char
  | Term Char
  deriving (Show, Eq)

data Expr
  = Atom Type
  | Func (Char, [Expr])
  deriving (Show, Eq)

skipSpace :: String -> String
skipSpace (x : xs)
  | isSpace x = skipSpace xs
skipSpace xs = xs

getFunc :: String -> Maybe (Expr, String)
getFunc [] = Nothing
getFunc (x : xs)
  | isLower x = f [] (skipSpace xs)
  where
    f _ [] = Nothing
    f ys (')' : xs') = Just (Func (x, reverse ys), skipSpace xs')
    f ys xs' = getExpr (skipSpace xs') >>= \(y, xs'') -> f (y : ys) xs''
getFunc _ = Nothing

getExpr :: String -> Maybe (Expr, String)
getExpr [] = Nothing
getExpr ('(' : xs) = getFunc $ skipSpace xs
getExpr (x : xs)
  | isLower x = Just (Atom (Var x), skipSpace xs)
  | isUpper x = Just (Atom (Term x), skipSpace xs)
getExpr _ = Nothing

parse :: String -> Maybe Expr
parse [] = Nothing
parse xs = getExpr (skipSpace xs) >>= f
  where
    f (expr, []) = Just expr
    f _ = Nothing

main :: IO ()
main = assert (parse " ( f A b ) " == expected) $ print "Ok!"
  where
    expected = Just (Func ('f', [Atom (Term 'A'), Atom (Var 'b')]))
