import Control.Exception (assert)
import Data.List (elemIndex, intersect, nub)
import Test.HUnit (Test (..), runTestTTAndExit, (~?=))

data Expr
  = ExprInt Int
  | ExprVar Var
  | ExprFunc [Var] [Stmt] (Maybe (Frees, [Var]))
  | ExprCall Expr [Expr]
  | ExprArray [Expr]
  | ExprAccess Expr Int
  deriving (Eq, Show)

data Stmt
  = StmtVoid Expr
  | StmtDecl Var Expr
  | StmtSet Expr Expr
  | StmtReturn (Maybe Expr)
  deriving (Eq, Show)

data Var = Var String Bool
  deriving (Show)

instance Eq Var where
  Var a _ == Var b _ = a == b

instance Ord Var where
  Var a _ <= Var b _ = a <= b

newtype Frees = Frees [Var]
  deriving (Eq, Show)

instance Monoid Frees where
  mempty = Frees mempty

instance Semigroup Frees where
  (Frees a) <> (Frees b) = Frees $ nub $ a ++ b

newtype Env = Env [Var]
  deriving (Eq, Show)

instance Monoid Env where
  mempty = Env mempty

instance Semigroup Env where
  (Env _) <> (Env b) = Env b

getEscapes :: Env -> Frees -> (Frees, [Var])
getEscapes (Env env) (Frees frees) = (Frees [var | var <- frees, var `notElem` escapes], escapes)
  where
    escapes = env `intersect` frees

collectExpr :: Env -> Expr -> (Frees, Expr)
collectExpr _ expr@(ExprInt _) = (mempty, expr)
collectExpr (Env env) expr@(ExprVar var) = (Frees [var | var `notElem` env], expr)
collectExpr _ (ExprFunc args stmts0 Nothing) =
  (frees2, ExprFunc args stmts1 $ Just (frees2, escapes2))
  where
    (env, (frees1, stmts1)) = collectStmts (Env args) stmts0
    (frees2, escapes2) = getEscapes env frees1
collectExpr _ (ExprFunc _ _ (Just _)) = undefined
collectExpr env (ExprCall func0 args0) = (frees1 <> frees2, ExprCall func1 args2)
  where
    (frees1, func1) = collectExpr env func0
    (frees2, args2) = collectExprs env args0
collectExpr env (ExprArray exprs) = ExprArray <$> collectExprs env exprs
collectExpr env (ExprAccess expr index) = (`ExprAccess` index) <$> collectExpr env expr

collectExprs :: Env -> [Expr] -> (Frees, [Expr])
collectExprs = mapM . collectExpr

collectStmt :: Env -> Stmt -> (Env, (Frees, Stmt))
collectStmt env (StmtVoid expr) = (env, StmtVoid <$> collectExpr env expr)
collectStmt (Env env) (StmtDecl var expr) =
  assert (var `notElem` env) (Env (var : env), StmtDecl var <$> collectExpr (Env env) expr)
collectStmt env (StmtSet to0 from0) = (env, (Frees (frees1 <> frees2), StmtSet to2 from1))
  where
    (Frees frees1, from1) = collectExpr env from0
    (Frees frees2, to2) = collectExpr env to0
collectStmt env stmt@(StmtReturn Nothing) = (env, (mempty, stmt))
collectStmt env (StmtReturn (Just expr)) = (env, StmtReturn . Just <$> collectExpr env expr)

collectStmts :: Env -> [Stmt] -> (Env, (Frees, [Stmt]))
collectStmts env =
  foldl' (\prev@(env, _) stmt -> prev <> (((: []) <$>) <$> collectStmt env stmt)) (env, mempty)

convertExpr :: Frees -> Expr -> Expr
convertExpr _ expr@(ExprInt _) = expr
convertExpr (Frees frees) expr@(ExprVar var) =
  case var `elemIndex` frees of
    Just i -> ExprAccess (ExprAccess (ExprVar $ Var "env" True) i) 0
    Nothing -> expr
convertExpr _ (ExprFunc _ _ Nothing) = undefined
convertExpr (Frees parentFrees) (ExprFunc args stmts (Just (Frees childFrees, escapes))) =
  ExprArray
    [ ExprFunc
        (Var "env" True : args)
        (map (convertStmt (Frees childFrees) escapes) stmts)
        $ Just (Frees childFrees, escapes),
      ExprArray $
        map
          ( \var ->
              case var `elemIndex` parentFrees of
                Just i -> ExprAccess (ExprVar $ Var "env" True) i
                Nothing -> ExprVar var
          )
          childFrees
    ]
convertExpr frees (ExprCall func args) =
  ExprCall (convertExpr frees func) (map (convertExpr frees) args)
convertExpr frees (ExprArray exprs) = ExprArray $ map (convertExpr frees) exprs
convertExpr frees (ExprAccess expr index) = ExprAccess (convertExpr frees expr) index

convertStmt :: Frees -> [Var] -> Stmt -> Stmt
convertStmt frees _ (StmtVoid expr) = StmtVoid $ convertExpr frees expr
convertStmt frees escapes (StmtDecl var expr0) =
  StmtDecl var $
    if var `elem` escapes
      then ExprArray [expr1]
      else expr1
  where
    expr1 = convertExpr frees expr0
convertStmt frees _ (StmtSet to from) = StmtSet (convertExpr frees to) (convertExpr frees from)
convertStmt _ _ stmt@(StmtReturn Nothing) = stmt
convertStmt frees _ (StmtReturn (Just expr)) = StmtReturn $ Just $ convertExpr frees expr

varF :: String -> Var
varF = (`Var` False)

varT :: String -> Var
varT = (`Var` True)

exprVar :: String -> Bool -> Expr
exprVar = (ExprVar .) . Var

exprVarF :: String -> Expr
exprVarF = (`exprVar` False)

exprVarT :: String -> Expr
exprVarT = (`exprVar` True)

exprFunc :: [Var] -> [Stmt] -> Expr
exprFunc args stmts = ExprFunc args stmts Nothing

exprFunc0 :: [Stmt] -> Expr
exprFunc0 = exprFunc []

exprAccess2 :: Expr -> Int -> Int -> Expr
exprAccess2 = (ExprAccess .) . ExprAccess

stmtDecl :: String -> Bool -> Expr -> Stmt
stmtDecl = (StmtDecl .) . Var

stmtDeclF :: String -> Expr -> Stmt
stmtDeclF = (`stmtDecl` False)

stmtDeclT :: String -> Expr -> Stmt
stmtDeclT = (`stmtDecl` True)

stmtReturn :: Expr -> Stmt
stmtReturn = StmtReturn . Just

tests :: [Test]
tests =
  [ collectExprs mempty [] ~?= mempty,
    collectStmts mempty [] ~?= mempty,
    (~?=)
      (collectStmt mempty $ stmtDeclF "x" $ ExprInt 0)
      (Env [varF "x"], (mempty, stmtDeclF "x" $ ExprInt 0)),
    (~?=) (collectExpr mempty $ exprVarF "x") (Frees [varF "x"], exprVarF "x"),
    (~?=)
      (collectExpr mempty $ exprFunc0 [stmtReturn $ exprVarF "x"])
      (Frees [varF "x"], ExprFunc [] [stmtReturn $ exprVarF "x"] $ Just (Frees [varF "x"], [])),
    (~?=)
      (collectStmts mempty [stmtDeclF "x" $ ExprInt 0, stmtDeclT "y" $ exprVarT "z"])
      ( Env [varT "y", varF "x"],
        (Frees [varT "z"], [stmtDeclF "x" $ ExprInt 0, stmtDeclT "y" $ exprVarT "z"])
      ),
    (~?=)
      ( collectStmts
          mempty
          [stmtDeclF "x" $ ExprInt 0, stmtDeclF "f" $ exprFunc0 [stmtReturn $ exprVarF "x"]]
      )
      ( Env [varF "f", varF "x"],
        ( Frees [varF "x"],
          [ stmtDeclF "x" $ ExprInt 0,
            stmtDeclF "f" $ ExprFunc [] [stmtReturn $ exprVarF "x"] $ Just (Frees [varF "x"], [])
          ]
        )
      ),
    (~?=)
      (collectExpr mempty $ exprFunc0 [stmtReturn $ exprFunc0 [stmtReturn $ exprVarF "x"]])
      ( Frees [varF "x"],
        ExprFunc
          []
          [stmtReturn $ ExprFunc [] [stmtReturn $ exprVarF "x"] $ Just (Frees [varF "x"], [])]
          $ Just (Frees [varF "x"], [])
      ),
    (~?=)
      ( collectExpr
          mempty
          $ exprFunc0 [stmtDeclF "x" $ ExprInt 0, stmtReturn $ exprFunc0 [stmtReturn $ exprVarF "x"]]
      )
      ( mempty,
        ExprFunc
          []
          [ stmtDeclF "x" $ ExprInt 0,
            stmtReturn $ ExprFunc [] [stmtReturn $ exprVarF "x"] $ Just (Frees [varF "x"], [])
          ]
          $ Just (mempty, [varF "x"])
      ),
    (~?=)
      (collectStmts mempty [StmtVoid $ exprVarF "x", StmtVoid $ exprVarT "y"])
      (mempty, (Frees [varF "x", varT "y"], [StmtVoid $ exprVarF "x", StmtVoid $ exprVarT "y"])),
    (~?=)
      (collectStmts mempty [StmtVoid $ exprVarT "y", StmtVoid $ exprVarF "x"])
      (mempty, (Frees [varT "y", varF "x"], [StmtVoid $ exprVarT "y", StmtVoid $ exprVarF "x"])),
    (~?=)
      ( collectStmts
          mempty
          [StmtVoid $ exprVarF "x", StmtVoid $ exprVarT "y", StmtVoid $ exprVarF "x"]
      )
      ( mempty,
        ( Frees [varF "x", varT "y"],
          [StmtVoid $ exprVarF "x", StmtVoid $ exprVarT "y", StmtVoid $ exprVarF "x"]
        )
      ),
    (~?=)
      ( collectExpr
          mempty
          $ exprFunc
            [varF "a", varF "b"]
            [ stmtDeclF "x" $ exprVarF "a",
              stmtReturn $
                exprFunc0
                  [ StmtSet (exprVarF "x") $ ExprCall (exprVarF "+") [exprVarF "x", exprVarF "b"],
                    stmtReturn $ exprVarF "x"
                  ]
            ]
      )
      ( Frees [varF "+"],
        ExprFunc
          [varF "a", varF "b"]
          [ stmtDeclF "x" $ exprVarF "a",
            stmtReturn
              $ ExprFunc
                []
                [ StmtSet (exprVarF "x") $ ExprCall (exprVarF "+") [exprVarF "x", exprVarF "b"],
                  stmtReturn $ exprVarF "x"
                ]
              $ Just (Frees [varF "+", varF "x", varF "b"], [])
          ]
          $ Just (Frees [varF "+"], [varF "x", varF "b"])
      ),
    (~?=)
      ( collectExpr
          mempty
          $ exprFunc0 [stmtReturn $ ExprCall (exprVarF "+") [exprVarF "x", exprVarF "y"]]
      )
      ( Frees [varF "+", varF "x", varF "y"],
        ExprFunc
          []
          [stmtReturn $ ExprCall (exprVarF "+") [exprVarF "x", exprVarF "y"]]
          $ Just (Frees [varF "+", varF "x", varF "y"], [])
      ),
    (~?=)
      ( collectExpr
          mempty
          $ exprFunc0
            [ stmtDeclF "x" $ ExprInt 0,
              StmtVoid $
                exprFunc0
                  [ stmtDeclF "y" $ ExprInt 1,
                    stmtReturn $
                      exprFunc0 [stmtReturn $ ExprCall (exprVarF "+") [exprVarF "x", exprVarF "y"]]
                  ]
            ]
      )
      ( Frees [varF "+"],
        ExprFunc
          []
          [ stmtDeclF "x" $ ExprInt 0,
            StmtVoid
              $ ExprFunc
                []
                [ stmtDeclF "y" $ ExprInt 1,
                  stmtReturn $
                    ExprFunc [] [stmtReturn $ ExprCall (exprVarF "+") [exprVarF "x", exprVarF "y"]] $
                      Just (Frees [varF "+", varF "x", varF "y"], [])
                ]
              $ Just (Frees [varF "+", varF "x"], [varF "y"])
          ]
          $ Just (Frees [varF "+"], [varF "x"])
      ),
    (~?=)
      (convertExpr (Frees [varF "x", varF "y"]) $ exprVarF "x")
      (exprAccess2 (exprVarT "env") 0 0),
    (~?=)
      (convertExpr (Frees [varF "x", varF "y"]) $ exprVarF "y")
      (exprAccess2 (exprVarT "env") 1 0),
    (~?=)
      (convertStmt mempty [varF "x"] $ stmtDeclF "x" $ ExprInt 0)
      (stmtDeclF "x" $ ExprArray [ExprInt 0]),
    (~?=)
      ( convertExpr (Frees [varF "x"]) $
          ExprFunc [] [stmtReturn $ ExprCall (exprVarF "+") [exprVarF "x", exprVarF "y"]] $
            Just (Frees [varF "x", varF "y"], [])
      )
      ( ExprArray
          [ ExprFunc
              [varT "env"]
              [ stmtReturn $
                  ExprCall
                    (exprVarF "+")
                    [exprAccess2 (exprVarT "env") 0 0, exprAccess2 (exprVarT "env") 1 0]
              ]
              $ Just (Frees [varF "x", varF "y"], []),
            ExprArray [ExprAccess (exprVarT "env") 0, exprVarF "y"]
          ]
      )
  ]

main :: IO ()
main = runTestTTAndExit $ TestList tests
