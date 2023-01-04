import Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Set as S
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

resolveFunc :: S.Set String -> M.Map String ([String], Expr) -> Func -> Func
resolveFunc visited funcLabels (Func label args scope) =
  intoFunc label args $
    resolveExpr (S.insert label visited) funcLabels $
      ExprScope scope

resolveStmt :: S.Set String -> M.Map String ([String], Expr) -> Stmt -> Stmt
resolveStmt visited funcLabels (StmtLet var expr) =
  StmtLet var $ resolveExpr visited funcLabels expr

resolveExpr :: S.Set String -> M.Map String ([String], Expr) -> Expr -> Expr
resolveExpr visited funcLabels (ExprCall True (ExprVar var) callArgs)
  | S.member var visited = error "Cycle detected"
  | length callArgs /= length funcArgs = error "Incorrect number of arguments"
  | otherwise =
      case zipWith StmtLet funcArgs resolvedCallArgs of
        [] -> resolvedExpr
        resolvedStmts -> ExprScope $ Scope resolvedStmts resolvedExpr
  where
    (funcArgs, expr) = (M.!) funcLabels var
    resolvedExpr = resolveExpr visited funcLabels expr
    resolvedCallArgs = map (resolveExpr visited funcLabels) callArgs
resolveExpr _ _ (ExprCall True expr _) =
  error $ printf "Unable to resolve `%s`" (show expr)
resolveExpr _ _ expr@(ExprCall False _ _) = expr
resolveExpr visited funcLabels (ExprScope (Scope stmts expr)) =
  case map (resolveStmt visited funcLabels) stmts of
    [] -> resolvedExpr
    resolvedStmts -> ExprScope $ Scope resolvedStmts resolvedExpr
  where
    resolvedExpr = resolveExpr visited funcLabels expr
resolveExpr _ _ expr@(ExprVar _) = expr

main :: IO ()
main = do
  mapM_ print funcs
  putChar '\n'
  mapM_ (print . resolveFunc S.empty (intoMap funcs)) funcs
  where
    funcs =
      [ Func
          "f"
          ["a", "b"]
          (Scope [] (ExprCall False (ExprVar "g") [ExprVar "a", ExprVar "b"])),
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
          []
          ( Scope
              []
              ( ExprCall
                  True
                  (ExprVar "f")
                  [ ExprScope
                      ( Scope
                          [StmtLet "v" (ExprCall False (ExprVar "i") [])]
                          (ExprVar "v")
                      ),
                    ExprVar "u"
                  ]
              )
          ),
        Func
          "i"
          []
          (Scope [] (ExprCall True (ExprVar "h") []))
      ]
