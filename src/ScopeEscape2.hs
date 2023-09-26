import Control.Monad.State (State, get, gets, modify, runState)
import Data.Bifunctor (first)
import Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Printf (printf)

data Expr
  = ExprCall Expr [Expr]
  | ExprDeref Expr Int
  | ExprFunc Func
  | ExprIdent String
  | ExprInt Int

data Stmt
  = StmtDecl String Expr
  | StmtSet Expr Expr
  | StmtVoid Expr

data Func = Func [String] [String] [Stmt] Expr

data Type
  = TypeFunc [Type] [Type] Type
  | TypeInt
  | TypePointer Type
  | TypeVoid

instance Show Expr where
  show = showExpr 0

instance Show Stmt where
  show = showStmt 0

instance Show Func where
  show = showFunc 0

indent :: Int -> String
indent = (`replicate` ' ')

showExpr :: Int -> Expr -> String
showExpr n (ExprCall func args) =
  printf "%s(%s)" (showExpr n func) $ intercalate ", " $ map (showExpr n) args
showExpr n (ExprDeref pointer offset) =
  printf "%s[%d]" (showExpr n pointer) offset
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
showFunc n0 (Func args captures stmts returnExpr) =
  printf
    "\\(%s) |%s| {\n%s%s}"
    (intercalate ", " args)
    (intercalate ", " captures)
    ( unlines $
        map (printf "%s%s" $ indent n1) $
          map (showStmt n1) stmts ++ [showExpr n1 returnExpr]
    )
    (indent n0)
  where
    n1 = n0 + 4

liftExpr :: Expr -> State [(String, Func)] Expr
liftExpr (ExprCall func args) =
  ExprCall <$> liftExpr func <*> mapM liftExpr args
liftExpr (ExprDeref pointer offset) = (`ExprDeref` offset) <$> liftExpr pointer
liftExpr (ExprFunc func0) = do
  func1 <- liftFunc func0
  ident <- gets $ printf "__fn_%d__" . length
  modify ((ident, func1) :)
  return $ ExprIdent ident
liftExpr expr = return expr

liftStmt :: Stmt -> State [(String, Func)] Stmt
liftStmt (StmtDecl ident value) = StmtDecl ident <$> liftExpr value
liftStmt (StmtSet target value) =
  StmtSet <$> liftExpr target <*> liftExpr value
liftStmt (StmtVoid expr) = StmtVoid <$> liftExpr expr

liftFunc :: Func -> State [(String, Func)] Func
liftFunc (Func args captures stmts returnExpr) =
  Func args captures <$> mapM liftStmt stmts <*> liftExpr returnExpr

findOrphansExpr :: M.Map String Func -> S.Set String -> Expr -> S.Set String
findOrphansExpr globals locals (ExprCall func args) =
  S.unions $ map (findOrphansExpr globals locals) $ func : args
findOrphansExpr globals locals (ExprDeref pointer _) =
  findOrphansExpr globals locals pointer
findOrphansExpr _ _ (ExprFunc _) = undefined
findOrphansExpr globals locals (ExprIdent ident)
  | S.member ident locals = S.empty
  | otherwise =
      case M.lookup ident globals of
        Just (Func _ captures _ _) -> S.difference (S.fromList captures) locals
        Nothing -> S.singleton ident
findOrphansExpr _ _ _ = S.empty

findOrphansStmt ::
  M.Map String Func -> Stmt -> State (S.Set String) (S.Set String)
findOrphansStmt globals (StmtDecl ident value) = do
  locals <- get
  modify $ S.insert ident
  return $ findOrphansExpr globals locals value
findOrphansStmt globals (StmtSet target value) =
  gets $ \locals ->
    S.unions $ map (findOrphansExpr globals locals) [target, value]
findOrphansStmt globals (StmtVoid expr) =
  gets $ \locals -> findOrphansExpr globals locals expr

findOrphansFunc :: S.Set String -> M.Map String Func -> Func -> Func
findOrphansFunc intrinsics globals (Func args [] stmts returnExpr) =
  Func args captures stmts returnExpr
  where
    (orphans, locals) =
      first S.unions $
        runState (mapM (findOrphansStmt globals) stmts) $
          S.union intrinsics $
            S.fromList args
    captures =
      S.toList $ S.union orphans $ findOrphansExpr globals locals returnExpr
findOrphansFunc _ _ _ = undefined

main :: IO ()
main = do
  putChar '\n'
  print fnMain

  putChar '\n'
  mapM_ print $
    M.toList $
      foldr
        ( \(ident, func1) globals ->
            M.insert
              ident
              (findOrphansFunc (S.fromList $ M.keys intrinsics) globals func1)
              globals
        )
        M.empty
        funcs1
  where
    intrinsics =
      M.fromList
        [ ("+", TypeFunc [TypeInt, TypeInt] [] TypeInt),
          ("print", TypeFunc [TypeInt] [] TypeVoid)
        ]

    funcs1 = ("main", func0) : funcs0
    (func0, funcs0) = runState (liftFunc fnMain) []

    -- fnMain =
    --   Func [] [] [StmtDecl "x" $ ExprInt 0] $
    --     ExprCall
    --       ( ExprFunc $
    --           Func [] [] [] $
    --             ExprCall (ExprFunc $ Func [] [] [] $ ExprIdent "x") []
    --       )
    --       []

    fnMain =
      Func
        []
        []
        [ StmtDecl "k" $ ExprInt 0,
          StmtDecl "counter" $ ExprFunc fnCounter,
          StmtDecl "c" $ ExprCall (ExprIdent "counter") [ExprInt 0],
          StmtVoid $ ExprCall (ExprIdent "c") [ExprInt 1],
          StmtVoid $
            ExprCall
              ( ExprFunc $
                  Func
                    []
                    []
                    [ StmtVoid $
                        ExprCall
                          (ExprIdent "print")
                          [ExprCall (ExprIdent "c") [ExprInt 1]]
                    ]
                    (ExprInt 0)
              )
              []
        ]
        ( ExprCall
            ( ExprFunc $
                Func
                  ["c"]
                  []
                  [ StmtVoid $
                      ExprCall
                        (ExprIdent "print")
                        [ExprCall (ExprIdent "c") [ExprInt 1]]
                  ]
                  (ExprInt 0)
            )
            [ExprIdent "c"]
        )

    fnIncr =
      Func
        ["m"]
        []
        [ StmtSet (ExprIdent "x") $
            ExprCall (ExprIdent "+") [ExprIdent "x", ExprIdent "k"],
          StmtSet (ExprIdent "x") $
            ExprCall (ExprIdent "+") [ExprIdent "x", ExprIdent "m"]
        ]
        (ExprIdent "x")

    fnCounter =
      Func
        ["n"]
        []
        [StmtDecl "x" $ ExprIdent "n"]
        (ExprFunc fnIncr)
