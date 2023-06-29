import Data.Bifunctor (first)
import Data.List (intercalate, unfoldr)
import Text.Printf (printf)

data Inst
  = InstRet
  | InstLabel String
  | InstJmp String
  | InstJz String
  | InstLoad String
  | InstStore String
  | InstPush Int
  | InstEq
  | InstLt
  | InstAnd
  | InstAdd

instance Show Inst where
  show InstRet = "        ret"
  show (InstLabel label) = printf "    %s:" label
  show (InstJmp label) = printf "        jmp   %s" label
  show (InstJz label) = printf "        jz    %s" label
  show (InstLoad var) = printf "        load  %s" var
  show (InstStore var) = printf "        store %s" var
  show (InstPush int) = printf "        push  %d" int
  show InstEq = "        eq"
  show InstLt = "        lt"
  show InstAnd = "        and"
  show InstAdd = "        add"

data Expr
  = ExprIdent String
  | ExprInt Int
  | ExprFunc String [Expr]

instance Show Expr where
  show (ExprIdent ident) = ident
  show (ExprInt int) = show int
  show (ExprFunc label exprs) =
    printf "%s(%s)" label $ intercalate ", " $ map show exprs

intoExprs2 :: [Inst] -> Maybe ((Expr, Expr), [Inst])
intoExprs2 insts0 =
  case intoExpr insts0 of
    Just (expr1, insts1) ->
      case intoExpr insts1 of
        Just (expr2, insts2) -> Just ((expr1, expr2), insts2)
        Nothing -> undefined
    Nothing -> undefined

intoExpr :: [Inst] -> Maybe (Expr, [Inst])
intoExpr [] = Nothing
intoExpr (InstRet : insts) = Just (ExprFunc "ret" [], insts)
intoExpr (InstLabel label : insts) =
  Just (ExprFunc "label" [ExprIdent label], insts)
intoExpr (InstJmp label : insts) =
  Just (ExprFunc "jmp" [ExprIdent label], insts)
intoExpr (InstJz label : insts) =
  first (\expr -> ExprFunc "jz" [ExprIdent label, expr]) <$> intoExpr insts
intoExpr (InstLoad var : insts) = Just (ExprFunc "load" [ExprIdent var], insts)
intoExpr (InstStore var : insts) =
  Just (ExprFunc "store" [ExprIdent var], insts)
intoExpr (InstPush int : insts) = Just (ExprInt int, insts)
intoExpr (InstEq : insts) =
  first (\(expr0, expr1) -> ExprFunc "eq" [expr1, expr0]) <$> intoExprs2 insts
intoExpr (InstLt : insts) =
  first (\(expr0, expr1) -> ExprFunc "lt" [expr1, expr0]) <$> intoExprs2 insts
intoExpr (InstAnd : insts) =
  first (\(expr0, expr1) -> ExprFunc "and" [expr1, expr0]) <$> intoExprs2 insts
intoExpr (InstAdd : insts) =
  first (\(expr0, expr1) -> ExprFunc "add" [expr1, expr0]) <$> intoExprs2 insts

main :: IO ()
main = do
  putChar '\n'
  mapM_ print insts
  putChar '\n'
  mapM_ print $ reverse $ unfoldr intoExpr $ reverse insts
  where
    insts :: [Inst]
    insts =
      [ InstLabel "while_start",
        InstLoad "x",
        InstPush 1000,
        InstLt,
        InstJz "while_end",
        InstLoad "x",
        InstPush 1,
        InstAnd,
        InstPush 0,
        InstEq,
        InstJz "if_else",
        InstLoad "x",
        InstPush 29,
        InstAdd,
        InstStore "x",
        InstJmp "if_end",
        InstLabel "if_else",
        InstLoad "x",
        InstPush $ -3,
        InstAdd,
        InstStore "x",
        InstLabel "if_end",
        InstJmp "while_start",
        InstLabel "while_end",
        InstRet
      ]
