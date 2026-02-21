import Control.Exception (assert)
import Data.List (intersect, nub)
import Test.HUnit (Test (..), runTestTTAndExit, (~?=))

data Expr
  = ExprInt Int
  | ExprVar Var
  | ExprFunc [Var] [Stmt] Frees [Var]
  | ExprCall [Expr]
  deriving (Eq, Show)

data Stmt
  = StmtVoid Expr
  | StmtDecl Var Expr
  | StmtSet Expr Expr
  | StmtReturn (Maybe Expr)
  deriving (Eq, Show)

data Type
  = TypeInt
  | TypePointer Type
  deriving (Eq, Ord, Show)

data Var = Var String Type
  deriving (Eq, Ord, Show)

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
collectExpr _ (ExprFunc args stmts0 (Frees frees0) escapes0) =
  assert (null frees0 && null escapes0) (frees2, ExprFunc args stmts1 frees2 escapes2)
  where
    (env, (frees1, stmts1)) = collectStmts (Env args) stmts0
    (frees2, escapes2) = getEscapes env frees1
collectExpr env (ExprCall exprs) = ExprCall <$> collectExprs env exprs

collectExprs :: Env -> [Expr] -> (Frees, [Expr])
collectExprs = mapM . collectExpr

collectStmt :: Env -> Stmt -> (Env, (Frees, Stmt))
collectStmt env (StmtVoid expr) = (env, StmtVoid <$> collectExpr env expr)
collectStmt (Env env) (StmtDecl var expr) =
  (Env (var : env), StmtDecl var <$> collectExpr (Env env) expr)
collectStmt env (StmtSet to0 from0) = (env, (Frees (frees1 <> frees2), StmtSet to1 from2))
  where
    (Frees frees1, to1) = collectExpr env to0
    (Frees frees2, from2) = collectExpr env from0
collectStmt env stmt@(StmtReturn Nothing) = (env, (mempty, stmt))
collectStmt env (StmtReturn (Just expr)) = (env, StmtReturn . Just <$> collectExpr env expr)

collectStmts :: Env -> [Stmt] -> (Env, (Frees, [Stmt]))
collectStmts env =
  foldl' (\prev@(env, _) stmt -> prev <> (((: []) <$>) <$> collectStmt env stmt)) (env, mempty)

tests :: [Test]
tests =
  [ (~?=) (collectExprs mempty []) (mempty, []),
    (~?=) (collectStmts mempty []) (mempty, (mempty, [])),
    (~?=)
      (collectStmt mempty $ StmtDecl (Var "x" TypeInt) (ExprInt 0))
      (Env [Var "x" TypeInt], (mempty, StmtDecl (Var "x" TypeInt) (ExprInt 0))),
    (~?=)
      (collectExpr mempty $ ExprVar $ Var "x" TypeInt)
      (Frees [Var "x" TypeInt], ExprVar (Var "x" TypeInt)),
    (~?=)
      ( collectExpr
          mempty
          $ ExprFunc [] [StmtReturn $ Just $ ExprVar $ Var "x" TypeInt] mempty mempty
      )
      ( Frees [Var "x" TypeInt],
        ExprFunc [] [StmtReturn $ Just $ ExprVar $ Var "x" TypeInt] (Frees [Var "x" TypeInt]) mempty
      ),
    (~?=)
      ( collectStmts
          mempty
          [ StmtDecl (Var "x" TypeInt) (ExprInt 0),
            StmtDecl (Var "y" $ TypePointer TypeInt) (ExprVar $ Var "z" $ TypePointer TypeInt)
          ]
      )
      ( Env [Var "y" $ TypePointer TypeInt, Var "x" TypeInt],
        ( Frees [Var "z" $ TypePointer TypeInt],
          [ StmtDecl (Var "x" TypeInt) (ExprInt 0),
            StmtDecl (Var "y" $ TypePointer TypeInt) (ExprVar $ Var "z" $ TypePointer TypeInt)
          ]
        )
      ),
    (~?=)
      ( collectStmts
          mempty
          [ StmtDecl (Var "x" TypeInt) (ExprInt 0),
            StmtDecl
              (Var "f" TypeInt)
              (ExprFunc [] [StmtReturn $ Just $ ExprVar $ Var "x" TypeInt] mempty mempty)
          ]
      )
      ( Env [Var "f" TypeInt, Var "x" TypeInt],
        ( Frees [Var "x" TypeInt],
          [ StmtDecl (Var "x" TypeInt) (ExprInt 0),
            StmtDecl
              (Var "f" TypeInt)
              ( ExprFunc
                  []
                  [StmtReturn $ Just $ ExprVar $ Var "x" TypeInt]
                  (Frees [Var "x" TypeInt])
                  mempty
              )
          ]
        )
      ),
    (~?=)
      ( collectExpr
          mempty
          $ ExprFunc
            []
            [ StmtReturn $
                Just $
                  ExprFunc [] [StmtReturn $ Just $ ExprVar $ Var "x" TypeInt] mempty mempty
            ]
            mempty
            mempty
      )
      ( Frees [Var "x" TypeInt],
        ExprFunc
          []
          [ StmtReturn $
              Just $
                ExprFunc
                  []
                  [StmtReturn $ Just $ ExprVar $ Var "x" TypeInt]
                  (Frees [Var "x" TypeInt])
                  mempty
          ]
          (Frees [Var "x" TypeInt])
          mempty
      ),
    (~?=)
      ( collectExpr
          mempty
          $ ExprFunc
            []
            [ StmtDecl (Var "x" TypeInt) (ExprInt 0),
              StmtReturn $
                Just $
                  ExprFunc [] [StmtReturn $ Just $ ExprVar $ Var "x" TypeInt] mempty mempty
            ]
            mempty
            mempty
      )
      ( mempty,
        ExprFunc
          []
          [ StmtDecl (Var "x" TypeInt) (ExprInt 0),
            StmtReturn $
              Just $
                ExprFunc
                  []
                  [StmtReturn $ Just $ ExprVar $ Var "x" TypeInt]
                  (Frees [Var "x" TypeInt])
                  mempty
          ]
          mempty
          [Var "x" TypeInt]
      ),
    (~?=)
      ( collectStmts
          mempty
          [StmtVoid $ ExprVar $ Var "x" TypeInt, StmtVoid $ ExprVar $ Var "y" $ TypePointer TypeInt]
      )
      ( mempty,
        ( Frees [Var "x" TypeInt, Var "y" $ TypePointer TypeInt],
          [StmtVoid $ ExprVar $ Var "x" TypeInt, StmtVoid $ ExprVar $ Var "y" $ TypePointer TypeInt]
        )
      ),
    (~?=)
      ( collectStmts
          mempty
          [StmtVoid $ ExprVar $ Var "y" $ TypePointer TypeInt, StmtVoid $ ExprVar $ Var "x" TypeInt]
      )
      ( mempty,
        ( Frees [Var "y" $ TypePointer TypeInt, Var "x" TypeInt],
          [StmtVoid $ ExprVar $ Var "y" $ TypePointer TypeInt, StmtVoid $ ExprVar $ Var "x" TypeInt]
        )
      ),
    (~?=)
      ( collectStmts
          mempty
          [ StmtVoid $ ExprVar $ Var "x" TypeInt,
            StmtVoid $ ExprVar $ Var "y" $ TypePointer TypeInt,
            StmtVoid $ ExprVar $ Var "x" TypeInt
          ]
      )
      ( mempty,
        ( Frees [Var "x" TypeInt, Var "y" $ TypePointer TypeInt],
          [ StmtVoid $ ExprVar $ Var "x" TypeInt,
            StmtVoid $ ExprVar $ Var "y" $ TypePointer TypeInt,
            StmtVoid $ ExprVar $ Var "x" TypeInt
          ]
        )
      )
  ]

main :: IO ()
main = runTestTTAndExit $ TestList tests
