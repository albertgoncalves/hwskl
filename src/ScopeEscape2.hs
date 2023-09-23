import Control.Monad.State (State, evalState, gets, modify)
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Text.Printf (printf)

data Expr
  = ExprAccess Expr Int
  | ExprAlloc [Expr]
  | ExprCall Expr [Expr]
  | ExprFunc Func
  | ExprIdent String
  | ExprInt Int

data Stmt
  = StmtDecl String Expr
  | StmtSet Expr Expr
  | StmtVoid Expr

data Func = Func [(String, Type)] [String] [Stmt] Expr

data Type
  = TypeFunc [Type] [Type] Type
  | TypeInt
  | TypePointer [Type]
  | TypeUnknown
  deriving (Eq)

data Scope = Scope
  { scopeGlobals :: M.Map String Type,
    scopeLocals :: M.Map String Type,
    scopeOrphans :: S.Set String
  }

instance Show Expr where
  show = showExpr 0

instance Show Stmt where
  show = showStmt 0

instance Show Func where
  show = showFunc 0

indent :: Int -> String
indent = (`replicate` ' ')

showExpr :: Int -> Expr -> String
showExpr n (ExprAccess pointer offset) =
  printf "%s[%d]" (showExpr n pointer) offset
showExpr n (ExprAlloc exprs) =
  printf "[%s]" $ intercalate ", " $ map (showExpr n) exprs
showExpr n (ExprCall func args) =
  printf "%s(%s)" (showExpr n func) $ intercalate ", " $ map (showExpr n) args
showExpr n (ExprFunc func) = showFunc n func
showExpr _ (ExprIdent ident) = ident
showExpr _ (ExprInt int) = show int

showStmt :: Int -> Stmt -> String
showStmt n (StmtDecl ident value) =
  printf "%s := %s" ident (showExpr n value)
showStmt n (StmtSet target value) =
  printf "%s = %s" (showExpr n target) (showExpr n value)
showStmt n (StmtVoid expr) = showExpr n expr

showFunc :: Int -> Func -> String
showFunc n0 (Func args [] stmts returnExpr) =
  printf
    "\\(%s) {\n%s%s}"
    (intercalate ", " $ map fst args)
    ( unlines $
        map (printf "%s%s" $ indent n1) $
          map (showStmt n1) stmts ++ [showExpr n1 returnExpr]
    )
    (indent n0)
  where
    n1 = n0 + 4
showFunc n (Func args captures stmts returnExpr) =
  showExpr n $
    ExprAlloc $
      ExprFunc (Func args [] stmts returnExpr)
        : map ExprIdent captures

walkExpr :: Expr -> State Scope Expr
walkExpr (ExprAccess pointer offset) =
  (`ExprAccess` offset) <$> walkExpr pointer
walkExpr (ExprAlloc exprs) = ExprAlloc <$> mapM walkExpr exprs
walkExpr (ExprCall func0 args0) = do
  globals <- gets scopeGlobals
  locals <- gets scopeLocals
  func1 <- walkExpr func0
  args1 <- mapM walkExpr args0
  return $ case typeExpr globals locals func1 of
    TypeFunc _ captures _
      | not $ null captures ->
          ExprCall (ExprAccess func1 0) $
            args1 ++ map (ExprAccess func1) [1 .. length captures]
    _ -> ExprCall func1 args1
walkExpr (ExprFunc func) = ExprFunc <$> walkFunc func
walkExpr expr@(ExprIdent ident) = do
  globals <- gets scopeGlobals
  locals <- gets scopeLocals
  if S.member ident $
    S.union
      (S.fromList $ M.keys globals)
      (S.fromList $ M.keys locals)
    then return expr
    else do
      modify $ \scope ->
        scope {scopeOrphans = S.insert ident $ scopeOrphans scope}
      return $ ExprAccess expr 0
walkExpr expr@(ExprInt _) = return expr

walkStmt :: Stmt -> State Scope Stmt
walkStmt (StmtDecl ident value0) = do
  value1 <- walkExpr value0
  locals <- gets scopeLocals
  if M.member ident locals
    then undefined
    else do
      modify $
        \scope ->
          scope
            { scopeLocals =
                M.insert
                  ident
                  (typeExpr (scopeGlobals scope) (scopeLocals scope) value1)
                  (scopeLocals scope)
            }
      return $ StmtDecl ident value1
walkStmt (StmtSet target value) =
  StmtSet <$> walkExpr target <*> walkExpr value
walkStmt (StmtVoid expr) = StmtVoid <$> walkExpr expr

walkFunc :: Func -> State Scope Func
walkFunc (Func args [] stmts0 returnExpr0) = do
  locals0 <- gets scopeLocals
  orphans0 <- gets scopeOrphans
  modify $
    \scope -> scope {scopeLocals = M.fromList args, scopeOrphans = S.empty}
  stmts1 <- mapM walkStmt stmts0
  returnExpr1 <- walkExpr returnExpr0
  locals1 <- gets $ S.fromList . M.keys . scopeLocals
  stmts2 <-
    gets $
      allocStmts stmts1
        . (\scope -> S.intersection (scopeOrphans scope) locals1)
  orphans2 <- gets $ \scope -> S.difference (scopeOrphans scope) locals1
  modify $
    \scope ->
      scope
        { scopeLocals = locals0,
          scopeOrphans = S.union orphans0 orphans2
        }
  return $ Func args (S.toList orphans2) stmts2 returnExpr1
walkFunc _ = undefined

typeExpr :: M.Map String Type -> M.Map String Type -> Expr -> Type
typeExpr globals locals (ExprAccess pointer offset) =
  case typeExpr globals locals pointer of
    TypePointer types -> types !! offset
    _ -> undefined
typeExpr globals locals (ExprAlloc exprs) =
  TypePointer $ map (typeExpr globals locals) exprs
typeExpr globals locals (ExprCall func args) =
  case typeExpr globals locals func of
    TypeFunc argTypes _ returnType
      | map (typeExpr globals locals) args == argTypes -> returnType
    _ -> undefined
typeExpr globals locals (ExprFunc (Func args captures _ returnExpr)) =
  TypeFunc
    (map snd args)
    (map (typeExpr globals locals . ExprIdent) captures)
    (typeExpr globals locals returnExpr)
typeExpr globals locals (ExprIdent ident) =
  case M.lookup ident locals of
    Just type0 -> type0
    Nothing -> fromMaybe undefined (M.lookup ident globals)
typeExpr _ _ (ExprInt _) = TypeInt

allocStmts :: [Stmt] -> S.Set String -> [Stmt]
allocStmts [] _ = []
allocStmts stmts idents
  | S.null idents = stmts
allocStmts (stmt@(StmtDecl ident value) : stmts) idents
  | S.member ident idents =
      StmtDecl ident (ExprAlloc [value])
        : allocStmts stmts (S.delete ident idents)
  | otherwise = stmt : allocStmts stmts idents
allocStmts (stmt : stmts) idents = stmt : allocStmts stmts idents

main :: IO ()
main = do
  putChar '\n'
  print fnMain
  print $
    evalState (walkFunc fnMain) $
      Scope
        ( M.fromList
            [ ("print", TypeUnknown),
              ("+", TypeFunc [TypeInt, TypeInt] [] TypeInt)
            ]
        )
        M.empty
        S.empty
  where
    fnMain =
      Func
        []
        []
        [ StmtDecl "counter" $ ExprFunc fnCounter,
          StmtDecl "c" $ ExprCall (ExprIdent "counter") [ExprInt 0],
          StmtVoid $ ExprCall (ExprIdent "c") [ExprInt 1],
          StmtVoid $
            ExprCall
              (ExprIdent "print")
              [ExprCall (ExprIdent "c") [ExprInt 1]]
        ]
        (ExprInt 0)

    fnIncr =
      Func
        [("m", TypeInt)]
        []
        [ StmtSet (ExprIdent "x") $
            ExprCall (ExprIdent "+") [ExprIdent "x", ExprIdent "m"]
        ]
        (ExprIdent "x")

    fnCounter =
      Func
        [("n", TypeInt)]
        []
        [StmtDecl "x" $ ExprIdent "n"]
        (ExprFunc fnIncr)
