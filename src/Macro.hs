import Data.List (intercalate)
import qualified Data.Map as M
import Text.Printf (printf)

data Expr
  = ExprVar String
  | ExprCall Bool Expr [Expr]
  | ExprScope Scope

data Stmt
  = StmtLet String Expr

data Func = Func String [String] Scope

data Scope = Scope [Stmt] Expr

instance Show Expr where
  show (ExprVar var) = var
  show (ExprCall flag expr args) =
    printf
      "%s%s(%s)"
      (if flag then "#" else "")
      (show expr)
      (intercalate ", " $ map show args)
  show (ExprScope scope) = show scope

instance Show Stmt where
  show (StmtLet var expr) = printf "%s = %s" var (show expr)

instance Show Func where
  show (Func label args scope) =
    printf "%s(%s) %s" label (intercalate ", " args) (show scope)

instance Show Scope where
  show (Scope stmts expr) =
    printf "{ %s }" (intercalate "; " $ map show stmts ++ [show expr])

intoMap :: [Func] -> M.Map String ([String], Expr)
intoMap =
  M.fromList
    . map (\(Func label args scope) -> (label, (args, ExprScope scope)))

intoFunc :: String -> [String] -> Expr -> Func
intoFunc label args (ExprScope scope) = Func label args scope
intoFunc label args expr = Func label args (Scope [] expr)

resolveFunc :: M.Map String ([String], Expr) -> Func -> Func
resolveFunc funcLabels (Func label args scope) =
  intoFunc label args $ resolveExpr funcLabels $ ExprScope scope

resolveStmt :: M.Map String ([String], Expr) -> Stmt -> Stmt
resolveStmt funcLabels (StmtLet var expr) =
  StmtLet var $ resolveExpr funcLabels expr

{-

  f(a, b) { #g(a, b) }
  g(a, b) { #f(a, b) }

---

  f(a, b) { #f(a, b) }
  g(a, b) { #g(a, b) }

-}

-- NOTE: Current implementation can fall into an infinite loop. We need to
-- detect cycles!
resolveExpr :: M.Map String ([String], Expr) -> Expr -> Expr
resolveExpr funcLabels (ExprCall True (ExprVar var) callArgs) =
  if length callArgs == length funcArgs
    then
      ExprScope $
        Scope (zipWith StmtLet funcArgs resolvedCallArgs) resolvedExpr
    else undefined
  where
    (funcArgs, expr) = (M.!) funcLabels var
    resolvedExpr = resolveExpr funcLabels expr
    resolvedCallArgs = map (resolveExpr funcLabels) callArgs
resolveExpr funcLabels (ExprScope (Scope stmts expr)) =
  ExprScope $
    Scope (map (resolveStmt funcLabels) stmts) $ resolveExpr funcLabels expr
resolveExpr _ expr = expr

main :: IO ()
main = do
  mapM_ print $ funcs ++ map (resolveFunc $ intoMap funcs) funcs
  where
    funcs =
      [ Func "f" ["a", "b"] (Scope [] (ExprVar "a")),
        Func
          "g"
          ["x", "y"]
          ( Scope
              [ StmtLet
                  "z"
                  (ExprCall True (ExprVar "f") [ExprVar "x", ExprVar "y"])
              ]
              (ExprVar "z")
          ),
        Func
          "h"
          ["x", "y"]
          ( Scope
              [ StmtLet
                  "z"
                  (ExprCall False (ExprVar "f") [ExprVar "x", ExprVar "y"])
              ]
              (ExprVar "z")
          )
      ]
