import Control.Exception (assert)
import Data.List (intersect, nub)
import Test.HUnit (Test (..), runTestTTAndExit, (~?=))

data Expr
  = ExprInt Int
  | ExprVar Var
  | ExprFunc [Var] [Stmt] (Maybe (Frees, [Var]))
  | ExprCall Expr [Expr]
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

tests :: [Test]
tests =
  [ collectExprs mempty [] ~?= mempty,
    collectStmts mempty [] ~?= mempty,
    (~?=)
      (collectStmt mempty $ StmtDecl (Var "x" False) (ExprInt 0))
      (Env [Var "x" False], (mempty, StmtDecl (Var "x" False) (ExprInt 0))),
    (~?=)
      (collectExpr mempty $ ExprVar $ Var "x" False)
      (Frees [Var "x" False], ExprVar (Var "x" False)),
    (~?=)
      (collectExpr mempty $ ExprFunc [] [StmtReturn $ Just $ ExprVar $ Var "x" False] Nothing)
      ( Frees [Var "x" False],
        ExprFunc
          []
          [StmtReturn $ Just $ ExprVar $ Var "x" False]
          $ Just (Frees [Var "x" False], [])
      ),
    (~?=)
      ( collectStmts
          mempty
          [StmtDecl (Var "x" False) (ExprInt 0), StmtDecl (Var "y" True) (ExprVar $ Var "z" True)]
      )
      ( Env [Var "y" True, Var "x" False],
        ( Frees [Var "z" True],
          [StmtDecl (Var "x" False) (ExprInt 0), StmtDecl (Var "y" True) (ExprVar $ Var "z" True)]
        )
      ),
    (~?=)
      ( collectStmts
          mempty
          [ StmtDecl (Var "x" False) (ExprInt 0),
            StmtDecl
              (Var "f" False)
              (ExprFunc [] [StmtReturn $ Just $ ExprVar $ Var "x" False] Nothing)
          ]
      )
      ( Env [Var "f" False, Var "x" False],
        ( Frees [Var "x" False],
          [ StmtDecl (Var "x" False) (ExprInt 0),
            StmtDecl
              (Var "f" False)
              ( ExprFunc
                  []
                  [StmtReturn $ Just $ ExprVar $ Var "x" False]
                  $ Just (Frees [Var "x" False], [])
              )
          ]
        )
      ),
    (~?=)
      ( collectExpr
          mempty
          $ ExprFunc
            []
            [StmtReturn $ Just $ ExprFunc [] [StmtReturn $ Just $ ExprVar $ Var "x" False] Nothing]
            Nothing
      )
      ( Frees [Var "x" False],
        ExprFunc
          []
          [ StmtReturn
              $ Just
              $ ExprFunc
                []
                [StmtReturn $ Just $ ExprVar $ Var "x" False]
              $ Just (Frees [Var "x" False], [])
          ]
          $ Just (Frees [Var "x" False], [])
      ),
    (~?=)
      ( collectExpr
          mempty
          $ ExprFunc
            []
            [ StmtDecl (Var "x" False) (ExprInt 0),
              StmtReturn $ Just $ ExprFunc [] [StmtReturn $ Just $ ExprVar $ Var "x" False] Nothing
            ]
            Nothing
      )
      ( mempty,
        ExprFunc
          []
          [ StmtDecl (Var "x" False) (ExprInt 0),
            StmtReturn
              $ Just
              $ ExprFunc
                []
                [StmtReturn $ Just $ ExprVar $ Var "x" False]
              $ Just (Frees [Var "x" False], [])
          ]
          $ Just (mempty, [Var "x" False])
      ),
    (~?=)
      (collectStmts mempty [StmtVoid $ ExprVar $ Var "x" False, StmtVoid $ ExprVar $ Var "y" True])
      ( mempty,
        ( Frees [Var "x" False, Var "y" True],
          [StmtVoid $ ExprVar $ Var "x" False, StmtVoid $ ExprVar $ Var "y" True]
        )
      ),
    (~?=)
      (collectStmts mempty [StmtVoid $ ExprVar $ Var "y" True, StmtVoid $ ExprVar $ Var "x" False])
      ( mempty,
        ( Frees [Var "y" True, Var "x" False],
          [StmtVoid $ ExprVar $ Var "y" True, StmtVoid $ ExprVar $ Var "x" False]
        )
      ),
    (~?=)
      ( collectStmts
          mempty
          [ StmtVoid $ ExprVar $ Var "x" False,
            StmtVoid $ ExprVar $ Var "y" True,
            StmtVoid $ ExprVar $ Var "x" False
          ]
      )
      ( mempty,
        ( Frees [Var "x" False, Var "y" True],
          [ StmtVoid $ ExprVar $ Var "x" False,
            StmtVoid $ ExprVar $ Var "y" True,
            StmtVoid $ ExprVar $ Var "x" False
          ]
        )
      ),
    (~?=)
      ( collectExpr
          mempty
          $ ExprFunc
            [Var "a" False, Var "b" False]
            [ StmtDecl (Var "x" False) (ExprVar $ Var "a" False),
              StmtReturn $
                Just $
                  ExprFunc
                    []
                    [ StmtSet (ExprVar $ Var "x" False) $
                        ExprCall
                          (ExprVar $ Var "+" False)
                          [ExprVar $ Var "x" False, ExprVar $ Var "b" False],
                      StmtReturn $ Just $ ExprVar $ Var "x" False
                    ]
                    Nothing
            ]
            Nothing
      )
      ( Frees [Var "+" False],
        ExprFunc
          [Var "a" False, Var "b" False]
          [ StmtDecl (Var "x" False) (ExprVar $ Var "a" False),
            StmtReturn
              $ Just
              $ ExprFunc
                []
                [ StmtSet (ExprVar $ Var "x" False) $
                    ExprCall
                      (ExprVar $ Var "+" False)
                      [ExprVar $ Var "x" False, ExprVar $ Var "b" False],
                  StmtReturn $ Just $ ExprVar $ Var "x" False
                ]
              $ Just (Frees [Var "+" False, Var "x" False, Var "b" False], [])
          ]
          $ Just (Frees [Var "+" False], [Var "x" False, Var "b" False])
      )
  ]

main :: IO ()
main = runTestTTAndExit $ TestList tests
