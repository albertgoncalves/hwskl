import Control.Exception (assert)
import Data.List (elemIndex, intersect, nub)
import Test.HUnit (Test (..), runTestTTAndExit, (~?=))

data Expr
  = ExprInt Int
  | ExprVar String
  | ExprFunc [String] [Stmt] (Maybe (Frees, [String]))
  | ExprCall Expr [Expr]
  | ExprArray [Expr]
  | ExprAccess Expr Int
  deriving (Eq, Show)

data Stmt
  = StmtVoid Expr
  | StmtDecl String Expr
  | StmtSet Expr Expr
  | StmtReturn (Maybe Expr)
  deriving (Eq, Show)

newtype Frees = Frees [String]
  deriving (Eq, Show)

instance Monoid Frees where
  mempty = Frees mempty

instance Semigroup Frees where
  (Frees a) <> (Frees b) = Frees $ nub $ a ++ b

newtype Env = Env [String]
  deriving (Eq, Show)

instance Monoid Env where
  mempty = Env mempty

instance Semigroup Env where
  (Env _) <> (Env b) = Env b

exprFunc :: [String] -> [Stmt] -> Expr
exprFunc args stmts = ExprFunc args stmts Nothing

exprFunc0 :: [Stmt] -> Expr
exprFunc0 = exprFunc []

exprAccess2 :: Expr -> Int -> Int -> Expr
exprAccess2 = (ExprAccess .) . ExprAccess

stmtReturn :: Expr -> Stmt
stmtReturn = StmtReturn . Just

getEscapes :: Env -> Frees -> (Frees, [String])
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

convertExpr :: Frees -> [String] -> Expr -> Expr
convertExpr _ _ expr@(ExprInt _) = expr
convertExpr (Frees frees) escapes expr@(ExprVar var) =
  case var `elemIndex` frees of
    Just i -> exprAccess2 (ExprVar "env") i 0
    Nothing ->
      if var `elem` escapes
        then ExprAccess expr 0
        else expr
convertExpr _ _ (ExprFunc _ _ Nothing) = undefined
convertExpr
  (Frees parentFrees)
  parentEscapes
  (ExprFunc args stmts (Just (Frees childFrees, childEscapes))) =
    ExprArray
      [ ExprFunc
          ("env" : args)
          ( [StmtDecl var $ ExprArray [ExprVar var] | var <- childEscapes, var `elem` args]
              ++ map (convertStmt (Frees childFrees) childEscapes) stmts
          )
          $ Just (Frees childFrees, childEscapes),
        ExprArray $
          map
            ( \var ->
                case var `elemIndex` parentFrees of
                  Just i -> ExprAccess (ExprVar "env") i
                  Nothing -> assert (var `elem` parentEscapes) $ ExprVar var
            )
            childFrees
      ]
convertExpr frees escapes (ExprCall func args) =
  ExprCall (convertExpr frees escapes func) (map (convertExpr frees escapes) args)
convertExpr frees escapes (ExprArray exprs) = ExprArray $ map (convertExpr frees escapes) exprs
convertExpr frees escapes (ExprAccess expr index) = ExprAccess (convertExpr frees escapes expr) index

convertStmt :: Frees -> [String] -> Stmt -> Stmt
convertStmt frees escapes (StmtVoid expr) = StmtVoid $ convertExpr frees escapes expr
convertStmt frees escapes (StmtDecl var expr0)
  | var `elem` escapes = StmtDecl var $ ExprArray [expr1]
  | otherwise = StmtDecl var expr1
  where
    expr1 = convertExpr frees escapes expr0
convertStmt frees escapes (StmtSet to from) =
  StmtSet (convertExpr frees escapes to) (convertExpr frees escapes from)
convertStmt _ _ stmt@(StmtReturn Nothing) = stmt
convertStmt frees escapes (StmtReturn (Just expr)) =
  StmtReturn $ Just $ convertExpr frees escapes expr

tests :: [Test]
tests =
  [ (~?=) (getEscapes mempty (Frees ["x", "y"])) (Frees ["x", "y"], []),
    (~?=) (getEscapes (Env ["x"]) (Frees ["x", "y"])) (Frees ["y"], ["x"]),
    (~?=) (getEscapes (Env ["x", "y"]) (Frees ["x", "y"])) (Frees [], ["x", "y"]),
    collectExprs mempty [] ~?= mempty,
    collectStmts mempty [] ~?= mempty,
    (~?=)
      (collectStmt mempty $ StmtDecl "x" $ ExprInt 0)
      (Env ["x"], (mempty, StmtDecl "x" $ ExprInt 0)),
    (~?=) (collectExpr mempty $ ExprVar "x") (Frees ["x"], ExprVar "x"),
    (~?=)
      (collectExpr mempty $ exprFunc0 [stmtReturn $ ExprVar "x"])
      (Frees ["x"], ExprFunc [] [stmtReturn $ ExprVar "x"] $ Just (Frees ["x"], [])),
    (~?=)
      (collectStmts mempty [StmtDecl "x" $ ExprInt 0, StmtDecl "y" $ ExprVar "z"])
      ( Env ["y", "x"],
        (Frees ["z"], [StmtDecl "x" $ ExprInt 0, StmtDecl "y" $ ExprVar "z"])
      ),
    (~?=)
      ( collectStmts
          mempty
          [StmtDecl "x" $ ExprInt 0, StmtDecl "f" $ exprFunc0 [stmtReturn $ ExprVar "x"]]
      )
      ( Env ["f", "x"],
        ( Frees ["x"],
          [ StmtDecl "x" $ ExprInt 0,
            StmtDecl "f" $ ExprFunc [] [stmtReturn $ ExprVar "x"] $ Just (Frees ["x"], [])
          ]
        )
      ),
    (~?=)
      (collectExpr mempty $ exprFunc0 [stmtReturn $ exprFunc0 [stmtReturn $ ExprVar "x"]])
      ( Frees ["x"],
        ExprFunc
          []
          [stmtReturn $ ExprFunc [] [stmtReturn $ ExprVar "x"] $ Just (Frees ["x"], [])]
          $ Just (Frees ["x"], [])
      ),
    (~?=)
      ( collectExpr
          mempty
          $ exprFunc0 [StmtDecl "x" $ ExprInt 0, stmtReturn $ exprFunc0 [stmtReturn $ ExprVar "x"]]
      )
      ( mempty,
        ExprFunc
          []
          [ StmtDecl "x" $ ExprInt 0,
            stmtReturn $ ExprFunc [] [stmtReturn $ ExprVar "x"] $ Just (Frees ["x"], [])
          ]
          $ Just (mempty, ["x"])
      ),
    (~?=)
      (collectStmts mempty [StmtVoid $ ExprVar "x", StmtVoid $ ExprVar "y"])
      (mempty, (Frees ["x", "y"], [StmtVoid $ ExprVar "x", StmtVoid $ ExprVar "y"])),
    (~?=)
      (collectStmts mempty [StmtVoid $ ExprVar "y", StmtVoid $ ExprVar "x"])
      (mempty, (Frees ["y", "x"], [StmtVoid $ ExprVar "y", StmtVoid $ ExprVar "x"])),
    (~?=)
      ( collectStmts
          mempty
          [StmtVoid $ ExprVar "x", StmtVoid $ ExprVar "y", StmtVoid $ ExprVar "x"]
      )
      ( mempty,
        ( Frees ["x", "y"],
          [StmtVoid $ ExprVar "x", StmtVoid $ ExprVar "y", StmtVoid $ ExprVar "x"]
        )
      ),
    (~?=)
      ( collectExpr
          mempty
          $ exprFunc
            ["a", "b"]
            [ StmtDecl "x" $ ExprVar "a",
              stmtReturn $
                exprFunc0
                  [ StmtSet (ExprVar "x") $ ExprCall (ExprVar "+") [ExprVar "x", ExprVar "b"],
                    stmtReturn $ ExprVar "x"
                  ]
            ]
      )
      ( Frees ["+"],
        ExprFunc
          ["a", "b"]
          [ StmtDecl "x" $ ExprVar "a",
            stmtReturn
              $ ExprFunc
                []
                [ StmtSet (ExprVar "x") $ ExprCall (ExprVar "+") [ExprVar "x", ExprVar "b"],
                  stmtReturn $ ExprVar "x"
                ]
              $ Just (Frees ["+", "x", "b"], [])
          ]
          $ Just (Frees ["+"], ["x", "b"])
      ),
    (~?=)
      ( collectExpr
          mempty
          $ exprFunc0 [stmtReturn $ ExprCall (ExprVar "+") [ExprVar "x", ExprVar "y"]]
      )
      ( Frees ["+", "x", "y"],
        ExprFunc
          []
          [stmtReturn $ ExprCall (ExprVar "+") [ExprVar "x", ExprVar "y"]]
          $ Just (Frees ["+", "x", "y"], [])
      ),
    (~?=)
      ( collectExpr
          mempty
          $ exprFunc0
            [ StmtDecl "x" $ ExprInt 0,
              StmtVoid $
                exprFunc0
                  [ StmtDecl "y" $ ExprInt 1,
                    stmtReturn $
                      exprFunc0 [stmtReturn $ ExprCall (ExprVar "+") [ExprVar "x", ExprVar "y"]]
                  ]
            ]
      )
      ( Frees ["+"],
        ExprFunc
          []
          [ StmtDecl "x" $ ExprInt 0,
            StmtVoid
              $ ExprFunc
                []
                [ StmtDecl "y" $ ExprInt 1,
                  stmtReturn $
                    ExprFunc [] [stmtReturn $ ExprCall (ExprVar "+") [ExprVar "x", ExprVar "y"]] $
                      Just (Frees ["+", "x", "y"], [])
                ]
              $ Just (Frees ["+", "x"], ["y"])
          ]
          $ Just (Frees ["+"], ["x"])
      ),
    (~?=)
      ( collectExpr
          mempty
          $ exprFunc0
            [ StmtDecl "x" $ ExprInt 0,
              stmtReturn $
                exprFunc
                  ["y"]
                  [ stmtReturn $
                      exprFunc0 [stmtReturn $ ExprCall (ExprVar "+") [ExprVar "x", ExprVar "y"]]
                  ]
            ]
      )
      ( Frees ["+"],
        ExprFunc
          []
          [ StmtDecl "x" $ ExprInt 0,
            stmtReturn
              $ ExprFunc
                ["y"]
                [ stmtReturn $
                    ExprFunc [] [stmtReturn $ ExprCall (ExprVar "+") [ExprVar "x", ExprVar "y"]] $
                      Just (Frees ["+", "x", "y"], [])
                ]
              $ Just (Frees ["+", "x"], ["y"])
          ]
          $ Just (Frees ["+"], ["x"])
      ),
    (~?=)
      ( collectExpr mempty $
          exprFunc0 [stmtReturn $ ExprCall (ExprVar "+") [ExprVar "x", ExprVar "y"]]
      )
      ( Frees ["+", "x", "y"],
        ExprFunc
          []
          [stmtReturn $ ExprCall (ExprVar "+") [ExprVar "x", ExprVar "y"]]
          $ Just (Frees ["+", "x", "y"], [])
      ),
    (~?=)
      ( collectExpr (Env ["y"]) $
          exprFunc0 [stmtReturn $ ExprCall (ExprVar "+") [ExprVar "x", ExprVar "y"]]
      )
      ( Frees ["+", "x", "y"],
        ExprFunc
          []
          [stmtReturn $ ExprCall (ExprVar "+") [ExprVar "x", ExprVar "y"]]
          $ Just (Frees ["+", "x", "y"], [])
      ),
    (~?=) (convertExpr (Frees ["x", "y"]) [] $ ExprVar "x") (exprAccess2 (ExprVar "env") 0 0),
    (~?=) (convertExpr (Frees ["x", "y"]) [] $ ExprVar "y") (exprAccess2 (ExprVar "env") 1 0),
    (~?=)
      (convertStmt mempty ["x"] $ StmtDecl "x" $ ExprInt 0)
      (StmtDecl "x" $ ExprArray [ExprInt 0]),
    (~?=)
      (convertStmt mempty ["x"] $ StmtVoid $ ExprVar "x")
      (StmtVoid $ ExprAccess (ExprVar "x") 0),
    (~?=)
      ( convertExpr (Frees ["x", "y"]) [] $
          ExprFunc [] [stmtReturn $ ExprCall (ExprVar "+") [ExprVar "x", ExprVar "y"]] $
            Just (Frees ["x", "y"], [])
      )
      ( ExprArray
          [ ExprFunc
              ["env"]
              [ stmtReturn $
                  ExprCall
                    (ExprVar "+")
                    [exprAccess2 (ExprVar "env") 0 0, exprAccess2 (ExprVar "env") 1 0]
              ]
              $ Just (Frees ["x", "y"], []),
            ExprArray [ExprAccess (ExprVar "env") 0, ExprAccess (ExprVar "env") 1]
          ]
      ),
    (~?=)
      ( convertExpr (Frees ["x"]) ["y"] $
          ExprFunc [] [stmtReturn $ ExprCall (ExprVar "+") [ExprVar "x", ExprVar "y"]] $
            Just (Frees ["x", "y"], [])
      )
      ( ExprArray
          [ ExprFunc
              ["env"]
              [ stmtReturn $
                  ExprCall
                    (ExprVar "+")
                    [exprAccess2 (ExprVar "env") 0 0, exprAccess2 (ExprVar "env") 1 0]
              ]
              $ Just (Frees ["x", "y"], []),
            ExprArray [ExprAccess (ExprVar "env") 0, ExprVar "y"]
          ]
      ),
    (~?=)
      ( convertExpr
          mempty
          []
          ( ExprFunc
              []
              [ StmtDecl "x" $ ExprInt 0,
                stmtReturn
                  $ ExprFunc
                    []
                    [ StmtDecl "y" $ ExprInt 1,
                      stmtReturn
                        $ ExprFunc
                          []
                          [stmtReturn $ ExprCall (ExprVar "+") [ExprVar "x", ExprVar "y"]]
                        $ Just (Frees ["x", "y"], [])
                    ]
                  $ Just (Frees ["x"], ["y"])
              ]
              $ Just (mempty, ["x"])
          )
      )
      ( ExprArray
          [ ExprFunc
              ["env"]
              [ StmtDecl "x" $ ExprArray [ExprInt 0],
                stmtReturn $
                  ExprArray
                    [ ExprFunc
                        ["env"]
                        [ StmtDecl "y" $ ExprArray [ExprInt 1],
                          stmtReturn $
                            ExprArray
                              [ ExprFunc
                                  ["env"]
                                  [ stmtReturn $
                                      ExprCall
                                        (ExprVar "+")
                                        [ exprAccess2 (ExprVar "env") 0 0,
                                          exprAccess2 (ExprVar "env") 1 0
                                        ]
                                  ]
                                  $ Just (Frees ["x", "y"], []),
                                ExprArray [ExprAccess (ExprVar "env") 0, ExprVar "y"]
                              ]
                        ]
                        $ Just (Frees ["x"], ["y"]),
                      ExprArray [ExprVar "x"]
                    ]
              ]
              $ Just (mempty, ["x"]),
            ExprArray []
          ]
      ),
    (~?=)
      ( convertExpr mempty [] $
          ExprFunc [] [StmtDecl "x" $ ExprInt 0, StmtVoid $ ExprVar "x"] $
            Just (Frees [], ["x"])
      )
      ( ExprArray
          [ ExprFunc
              ["env"]
              [StmtDecl "x" $ ExprArray [ExprInt 0], StmtVoid $ ExprAccess (ExprVar "x") 0]
              $ Just (Frees [], ["x"]),
            ExprArray []
          ]
      ),
    (~?=)
      ( convertExpr
          mempty
          []
          ( ExprFunc
              []
              [ StmtDecl "x" $ ExprInt 0,
                stmtReturn
                  $ ExprFunc
                    ["y"]
                    [ stmtReturn
                        $ ExprFunc
                          []
                          [stmtReturn $ ExprCall (ExprVar "+") [ExprVar "x", ExprVar "y"]]
                        $ Just (Frees ["x", "y"], [])
                    ]
                  $ Just (Frees ["x"], ["y"])
              ]
              $ Just (mempty, ["x"])
          )
      )
      ( ExprArray
          [ ExprFunc
              ["env"]
              [ StmtDecl "x" $ ExprArray [ExprInt 0],
                stmtReturn $
                  ExprArray
                    [ ExprFunc
                        ["env", "y"]
                        [ StmtDecl "y" $ ExprArray [ExprVar "y"],
                          stmtReturn $
                            ExprArray
                              [ ExprFunc
                                  ["env"]
                                  [ stmtReturn $
                                      ExprCall
                                        (ExprVar "+")
                                        [ exprAccess2 (ExprVar "env") 0 0,
                                          exprAccess2 (ExprVar "env") 1 0
                                        ]
                                  ]
                                  $ Just (Frees ["x", "y"], []),
                                ExprArray [ExprAccess (ExprVar "env") 0, ExprVar "y"]
                              ]
                        ]
                        $ Just (Frees ["x"], ["y"]),
                      ExprArray [ExprVar "x"]
                    ]
              ]
              $ Just (mempty, ["x"]),
            ExprArray []
          ]
      )
  ]

main :: IO ()
main = runTestTTAndExit $ TestList tests
