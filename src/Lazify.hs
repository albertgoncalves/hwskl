import Control.Applicative ((<|>))
import Control.Monad ((<=<))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy
  ( StateT,
    evalStateT,
    execStateT,
    get,
    gets,
    modify,
    put,
  )
import Data.Bifunctor (bimap, first)
import Data.List (intercalate)
import qualified Data.Map as M
import Test.HUnit (Test (..), runTestTT, (~?=))

{- % -}

data Expr
  = ExprInt Int
  | ExprVar String
  | ExprCall Expr [Expr]
  | ExprLazy Expr
  | ExprForce (Maybe Int) Expr
  deriving (Eq)

instance Show Expr where
  show (ExprInt int) = show int
  show (ExprVar var) = var
  show (ExprCall (ExprVar "+") [left, right]) = "(" ++ show left ++ " + " ++ show right ++ ")"
  show (ExprCall func args) = show func ++ "(" ++ intercalate ", " (map show args) ++ ")"
  show (ExprLazy expr) = "Lazy(lambda: " ++ show expr ++ ")"
  show (ExprForce (Just k) arg) = "__?_" ++ show k ++ "__(" ++ show arg ++ ")"
  show (ExprForce Nothing arg) = "__force__(" ++ show arg ++ ")"

{- % -}

data Stmt
  = StmtDecl String Expr
  | StmtFunc String [String] [Stmt]
  | StmtVoid Expr
  | StmtReturn (Maybe Expr)
  deriving (Eq)

instance Show Stmt where
  show = ('\n' :) . showStmt 0

indent :: Int -> String
indent = concat . (`replicate` "    ")

showStmt :: Int -> Stmt -> String
showStmt k (StmtDecl var expr) = indent k ++ var ++ " = " ++ show expr ++ "\n"
showStmt k (StmtFunc var args stmts) =
  concat
    [ indent k,
      "def ",
      var,
      "(",
      intercalate ", " args,
      "):\n",
      concatMap (showStmt $ k + 1) stmts,
      "\n"
    ]
showStmt k (StmtVoid expr) = indent k ++ show expr ++ "\n"
showStmt k (StmtReturn (Just expr)) = indent k ++ "return " ++ show expr ++ "\n"
showStmt k (StmtReturn Nothing) = indent k ++ "return\n"

mapStmt :: (Monad m) => (Expr -> m Expr) -> Stmt -> m Stmt
mapStmt f (StmtDecl var expr) = StmtDecl var <$> f expr
mapStmt f (StmtFunc var args stmts) = StmtFunc var args <$> mapM (mapStmt f) stmts
mapStmt f (StmtVoid expr) = StmtVoid <$> f expr
mapStmt f (StmtReturn (Just expr)) = StmtReturn . Just <$> f expr
mapStmt _ stmt@(StmtReturn Nothing) = return stmt

{- % -}

data Type
  = TypeNone
  | TypeInt
  | TypeLazy Type
  | TypeFunc [Type] Type
  | TypeK Int
  deriving (Eq, Ord, Show)

data TypeChecker = TypeChecker
  { typeCheckerK :: Int,
    typeCheckerFuncs :: M.Map [String] Type,
    typeCheckerLocals :: M.Map [String] Type,
    typeCheckerForces :: M.Map Int Type,
    typeCheckerFuncStack :: [String],
    typeCheckerReturnTypes :: [Type],
    typeCheckerPairs :: [(Type, Type)]
  }
  deriving (Eq, Show)

data Error
  = ErrorExprToLazy Expr
  | ErrorExprToForce Expr
  | ErrorExprToType Expr
  | ErrorRewriteExpr Expr
  | ErrorStmtToType Stmt
  | ErrorUnify1 Type Type
  | ErrorRewriteExprForce Type Type Expr
  | ErrorUnifyForces Type
  | ErrorLazify
  deriving (Eq, Show)

{- % -}

intrinsics :: M.Map String Type
intrinsics =
  M.fromList [("print", TypeFunc [TypeInt] TypeNone), ("+", TypeFunc [TypeInt, TypeInt] TypeInt)]

newTypeChecker :: TypeChecker
newTypeChecker = TypeChecker 0 (M.mapKeys (: []) intrinsics) mempty mempty [] [] []

{- % -}

nextK :: (Monad m) => StateT Int m Int
nextK = do
  k <- get
  put $ k + 1
  return k

{- % -}

exprToLazy :: Expr -> StateT Int (Either Error) Expr
exprToLazy expr@(ExprInt {}) = return expr
exprToLazy expr@(ExprVar {}) = return expr
exprToLazy (ExprCall func@(ExprVar var) args)
  | var `M.member` intrinsics = ExprCall func <$> mapM (exprToForce <=< exprToLazy) args
exprToLazy (ExprCall func args) =
  (ExprLazy .) . ExprCall <$> (exprToForce <=< exprToLazy) func <*> mapM exprToLazy args
exprToLazy expr@(ExprLazy {}) = lift $ Left $ ErrorExprToLazy expr
exprToLazy expr@(ExprForce {}) = lift $ Left $ ErrorExprToLazy expr

exprToForce :: Expr -> StateT Int (Either Error) Expr
exprToForce expr@(ExprInt {}) = return expr
exprToForce expr@(ExprVar {}) = (`ExprForce` expr) . Just <$> nextK
exprToForce expr@(ExprCall {}) = (`ExprForce` expr) . Just <$> nextK
exprToForce expr@(ExprLazy {}) = (`ExprForce` expr) . Just <$> nextK
exprToForce expr@(ExprForce {}) = lift $ Left $ ErrorExprToForce expr

testExprToLazy :: [Test]
testExprToLazy =
  map
    (uncurry (~?=) . first ((`evalStateT` 0) . exprToLazy))
    [ ( ExprCall (ExprVar "f") $ map ExprVar ["x", "y"],
        Right $ ExprLazy $ ExprCall (ExprForce (Just 0) $ ExprVar "f") $ map ExprVar ["x", "y"]
      ),
      ( ExprCall (ExprVar "f") [ExprVar "x", ExprCall (ExprVar "f") $ map ExprVar ["y", "z"]],
        Right $
          ExprLazy $
            ExprCall
              (ExprForce (Just 0) $ ExprVar "f")
              [ ExprVar "x",
                ExprLazy $ ExprCall (ExprForce (Just 1) $ ExprVar "f") $ map ExprVar ["y", "z"]
              ]
      ),
      ( ExprCall (ExprVar "print") [ExprInt 1],
        Right $ ExprCall (ExprVar "print") [ExprInt 1]
      ),
      ( ExprCall (ExprVar "print") [ExprVar "x"],
        Right $ ExprCall (ExprVar "print") [ExprForce (Just 0) $ ExprVar "x"]
      ),
      ( ExprCall (ExprVar "print") [ExprCall (ExprVar "f") []],
        Right $
          ExprCall
            (ExprVar "print")
            [ExprForce (Just 1) $ ExprLazy $ ExprCall (ExprForce (Just 0) $ ExprVar "f") []]
      )
    ]

{- % -}

stmtToLazy :: Stmt -> StateT Int (Either Error) Stmt
stmtToLazy = mapStmt exprToLazy

testStmtToLazy :: [Test]
testStmtToLazy =
  map
    (uncurry (~?=) . first ((`evalStateT` 0) . stmtToLazy))
    [ (StmtDecl "x" $ ExprInt 0, Right $ StmtDecl "x" $ ExprInt 0),
      ( StmtDecl "z" $ ExprCall (ExprVar "f") $ map ExprVar ["x", "y"],
        Right $
          StmtDecl "z" $
            ExprLazy $
              ExprCall (ExprForce (Just 0) $ ExprVar "f") $
                map ExprVar ["x", "y"]
      ),
      ( StmtFunc
          "main"
          []
          [ StmtFunc
              "add_0"
              ["x", "y"]
              [StmtReturn $ Just $ ExprCall (ExprVar "+") $ map ExprVar ["x", "y"]],
            StmtVoid $ ExprCall (ExprVar "print") [ExprCall (ExprVar "add_0") $ map ExprInt [4, 5]]
          ],
        Right $
          StmtFunc
            "main"
            []
            [ StmtFunc
                "add_0"
                ["x", "y"]
                [ StmtReturn $
                    Just $
                      ExprCall
                        (ExprVar "+")
                        [ExprForce (Just 0) $ ExprVar "x", ExprForce (Just 1) $ ExprVar "y"]
                ],
              StmtVoid $
                ExprCall
                  (ExprVar "print")
                  [ ExprForce (Just 3) $
                      ExprLazy $
                        ExprCall (ExprForce (Just 2) $ ExprVar "add_0") $
                          map ExprInt [4, 5]
                  ]
            ]
      ),
      ( StmtFunc
          "main"
          []
          [ StmtFunc
              "add_0"
              ["x", "y"]
              [StmtReturn $ Just $ ExprCall (ExprVar "+") $ map ExprVar ["x", "y"]],
            StmtFunc
              "lazy_add_0"
              ["x"]
              [StmtReturn $ Just $ ExprCall (ExprVar "add_0") [ExprVar "x", ExprInt 1]],
            StmtVoid $ ExprCall (ExprVar "print") [ExprCall (ExprVar "lazy_add_0") [ExprInt 3]],
            StmtVoid $ ExprCall (ExprVar "print") [ExprCall (ExprVar "add_0") $ map ExprInt [4, 5]]
          ],
        Right $
          StmtFunc
            "main"
            []
            [ StmtFunc
                "add_0"
                ["x", "y"]
                [ StmtReturn $
                    Just $
                      ExprCall
                        (ExprVar "+")
                        [ ExprForce (Just 0) $ ExprVar "x",
                          ExprForce (Just 1) $ ExprVar "y"
                        ]
                ],
              StmtFunc
                "lazy_add_0"
                ["x"]
                [ StmtReturn $
                    Just $
                      ExprLazy $
                        ExprCall (ExprForce (Just 2) $ ExprVar "add_0") [ExprVar "x", ExprInt 1]
                ],
              StmtVoid $
                ExprCall
                  (ExprVar "print")
                  [ ExprForce
                      (Just 4)
                      $ ExprLazy
                      $ ExprCall (ExprForce (Just 3) $ ExprVar "lazy_add_0") [ExprInt 3]
                  ],
              StmtVoid $
                ExprCall
                  (ExprVar "print")
                  [ ExprForce
                      (Just 6)
                      $ ExprLazy
                      $ ExprCall (ExprForce (Just 5) $ ExprVar "add_0")
                      $ map ExprInt [4, 5]
                  ]
            ]
      ),
      ( StmtFunc
          "main"
          []
          [ StmtFunc
              "add_0"
              ["x", "y"]
              [StmtReturn $ Just $ ExprCall (ExprVar "+") $ map ExprVar ["x", "y"]],
            StmtFunc
              "lazy_add_0"
              ["x"]
              [StmtReturn $ Just $ ExprCall (ExprVar "add_0") [ExprVar "x", ExprInt 1]],
            StmtVoid $ ExprCall (ExprVar "print") [ExprCall (ExprVar "lazy_add_0") [ExprInt 3]]
          ],
        Right $
          StmtFunc
            "main"
            []
            [ StmtFunc
                "add_0"
                ["x", "y"]
                [ StmtReturn $
                    Just $
                      ExprCall
                        (ExprVar "+")
                        [ExprForce (Just 0) $ ExprVar "x", ExprForce (Just 1) $ ExprVar "y"]
                ],
              StmtFunc
                "lazy_add_0"
                ["x"]
                [ StmtReturn $
                    Just $
                      ExprLazy $
                        ExprCall (ExprForce (Just 2) $ ExprVar "add_0") [ExprVar "x", ExprInt 1]
                ],
              StmtVoid $
                ExprCall
                  (ExprVar "print")
                  [ ExprForce (Just 4) $
                      ExprLazy $
                        ExprCall (ExprForce (Just 3) $ ExprVar "lazy_add_0") [ExprInt 3]
                  ]
            ]
      )
    ]

{- % -}

nextTypeK :: (Monad m) => StateT TypeChecker m Type
nextTypeK = do
  typeChecker <- get
  let k = typeCheckerK typeChecker
  let kType = TypeK k
  put $ typeChecker {typeCheckerK = k + 1}
  return kType

pushLocal :: (Monad m) => String -> Type -> StateT TypeChecker m ()
pushLocal var varType = do
  modify $ \typeChecker ->
    typeChecker
      { typeCheckerLocals =
          M.insert (var : typeCheckerFuncStack typeChecker) varType $ typeCheckerLocals typeChecker
      }

pushFunc :: (Monad m) => String -> Type -> StateT TypeChecker m ()
pushFunc func funcType = do
  modify $ \typeChecker ->
    typeChecker
      { typeCheckerFuncs =
          M.insert (func : typeCheckerFuncStack typeChecker) funcType $ typeCheckerFuncs typeChecker
      }

argToType :: (Monad m) => String -> StateT TypeChecker m Type
argToType arg = do
  argType <- nextTypeK
  pushLocal arg argType
  return argType

{- % -}

exprToType :: Expr -> StateT TypeChecker (Either Error) Type
exprToType (ExprInt {}) = return TypeInt
exprToType expr@(ExprVar var) = do
  funcs <- gets typeCheckerFuncs
  locals <- gets typeCheckerLocals
  funcStack <- gets typeCheckerFuncStack
  maybe (lift $ Left $ ErrorExprToType expr) return $
    M.lookup (var : funcStack) locals <|> exprToTypeLoop funcs var funcStack
exprToType (ExprCall func args) = do
  funcType <- exprToType func
  argTypes <- mapM exprToType args
  returnType <- nextTypeK
  modify $ \typeChecker ->
    typeChecker
      { typeCheckerPairs = (funcType, TypeFunc argTypes returnType) : typeCheckerPairs typeChecker
      }
  return returnType
exprToType (ExprLazy expr) = TypeLazy <$> exprToType expr
exprToType (ExprForce (Just k) arg) = do
  argType <- exprToType arg
  returnType <- nextTypeK
  modify $
    \typeChecker ->
      typeChecker
        { typeCheckerForces =
            M.insert k (TypeFunc [argType] returnType) $ typeCheckerForces typeChecker
        }
  return returnType
exprToType expr@(ExprForce Nothing _) = lift $ Left $ ErrorExprToType expr

exprToTypeLoop :: M.Map [String] Type -> String -> [String] -> Maybe Type
exprToTypeLoop funcs var [] = M.lookup [var] funcs
exprToTypeLoop funcs var funcStack@(_ : rest) =
  M.lookup (var : funcStack) funcs <|> exprToTypeLoop funcs var rest

{- % -}

stmtToType :: Stmt -> StateT TypeChecker (Either Error) ()
stmtToType stmt@(StmtDecl var expr) = do
  exprType <- exprToType expr
  funcs <- gets typeCheckerFuncs
  locals <- gets typeCheckerLocals
  funcStack <- gets typeCheckerFuncStack
  let lookup0 = M.lookup $ var : funcStack
  _ <- case lookup0 funcs <|> lookup0 locals of
    Just _ -> lift $ Left $ ErrorStmtToType stmt
    Nothing -> return ()
  pushLocal var exprType
stmtToType stmt@(StmtFunc var args stmts) = do
  parentFuncs <- gets typeCheckerFuncs
  parentLocals <- gets typeCheckerLocals
  parentFuncStack <- gets typeCheckerFuncStack
  parentReturnTypes <- gets typeCheckerReturnTypes

  modify $ \typeChecker ->
    typeChecker {typeCheckerFuncStack = var : parentFuncStack, typeCheckerReturnTypes = []}

  argTypes <- mapM argToType args
  returnType <- nextTypeK

  mapM_ stmtToType stmts

  modify $ \typeChecker ->
    typeChecker
      { typeCheckerFuncStack = parentFuncStack,
        typeCheckerReturnTypes = parentReturnTypes,
        typeCheckerPairs =
          map (returnType,) (typeCheckerReturnTypes typeChecker) ++ typeCheckerPairs typeChecker
      }

  let lookup1 = M.lookup $ var : parentFuncStack
  _ <- case lookup1 parentFuncs <|> lookup1 parentLocals of
    Just _ -> lift $ Left $ ErrorStmtToType stmt
    Nothing -> return ()

  pushFunc var (TypeFunc argTypes returnType)
stmtToType (StmtVoid expr) = do
  exprType <- exprToType expr
  modify $ \typeChecker ->
    typeChecker {typeCheckerPairs = (exprType, TypeNone) : typeCheckerPairs typeChecker}
stmtToType (StmtReturn (Just expr)) = do
  exprType <- exprToType expr
  modify $ \typeChecker ->
    typeChecker {typeCheckerReturnTypes = exprType : typeCheckerReturnTypes typeChecker}
stmtToType (StmtReturn Nothing) = modify $ \typeChecker ->
  typeChecker {typeCheckerReturnTypes = TypeNone : typeCheckerReturnTypes typeChecker}

testStmtToType :: [Test]
testStmtToType =
  map
    (uncurry (~?=) . first ((`execStateT` newTypeChecker) . stmtToType))
    [ (StmtFunc "f" [] [StmtReturn $ Just $ ExprVar "x"], Left $ ErrorExprToType $ ExprVar "x"),
      ( StmtFunc
          "main"
          []
          [StmtDecl "x" $ ExprInt 1, StmtFunc "f" [] [StmtReturn $ Just $ ExprVar "x"]],
        Left $ ErrorExprToType $ ExprVar "x"
      ),
      ( StmtFunc "f" ["x"] [StmtReturn $ Just $ ExprVar "x"],
        Right $
          newTypeChecker
            { typeCheckerK = 2,
              typeCheckerFuncs =
                M.union
                  (M.singleton ["f"] $ TypeFunc [TypeK 0] $ TypeK 1)
                  $ typeCheckerFuncs newTypeChecker,
              typeCheckerLocals =
                M.union (M.singleton ["x", "f"] $ TypeK 0) $ typeCheckerLocals newTypeChecker,
              typeCheckerPairs = [(TypeK 1, TypeK 0)]
            }
      ),
      ( StmtFunc "f" ["x"] [StmtReturn $ Just $ ExprForce (Just 0) $ ExprVar "x"],
        Right $
          newTypeChecker
            { typeCheckerK = 3,
              typeCheckerFuncs =
                M.union
                  (M.singleton ["f"] $ TypeFunc [TypeK 0] $ TypeK 1)
                  $ typeCheckerFuncs newTypeChecker,
              typeCheckerLocals =
                M.union (M.singleton ["x", "f"] $ TypeK 0) $ typeCheckerLocals newTypeChecker,
              typeCheckerForces = M.singleton 0 $ TypeFunc [TypeK 0] $ TypeK 2,
              typeCheckerPairs = [(TypeK 1, TypeK 2)]
            }
      ),
      ( StmtFunc
          "f"
          ["x", "y"]
          [StmtReturn $ Just $ ExprCall (ExprVar "+") $ map ExprVar ["x", "y"]],
        Right $
          newTypeChecker
            { typeCheckerK = 4,
              typeCheckerFuncs =
                M.union
                  (M.singleton ["f"] $ TypeFunc (map TypeK [0, 1]) $ TypeK 2)
                  $ typeCheckerFuncs newTypeChecker,
              typeCheckerLocals = M.fromList [(["x", "f"], TypeK 0), (["y", "f"], TypeK 1)],
              typeCheckerPairs =
                [ (TypeK 2, TypeK 3),
                  (TypeFunc [TypeInt, TypeInt] TypeInt, TypeFunc [TypeK 0, TypeK 1] $ TypeK 3)
                ]
            }
      ),
      ( StmtFunc
          "f"
          ["x", "y"]
          [StmtReturn $ Just $ ExprLazy $ ExprCall (ExprVar "+") $ map ExprVar ["x", "y"]],
        Right $
          newTypeChecker
            { typeCheckerK = 4,
              typeCheckerFuncs =
                M.union
                  (M.singleton ["f"] $ TypeFunc (map TypeK [0, 1]) $ TypeK 2)
                  $ typeCheckerFuncs newTypeChecker,
              typeCheckerLocals = M.fromList [(["x", "f"], TypeK 0), (["y", "f"], TypeK 1)],
              typeCheckerPairs =
                [ (TypeK 2, TypeLazy $ TypeK 3),
                  (TypeFunc [TypeInt, TypeInt] TypeInt, TypeFunc [TypeK 0, TypeK 1] $ TypeK 3)
                ]
            }
      ),
      ( StmtFunc
          "main"
          []
          [ StmtFunc
              "f"
              ["x", "y"]
              [ StmtReturn $
                  Just $
                    ExprCall
                      (ExprVar "+")
                      [ExprForce (Just 0) $ ExprVar "x", ExprForce (Just 1) $ ExprVar "y"]
              ],
            StmtVoid $ ExprCall (ExprVar "print") [ExprCall (ExprVar "f") $ map ExprInt [0, 1]]
          ],
        Right $
          newTypeChecker
            { typeCheckerK = 9,
              typeCheckerFuncs =
                M.union
                  ( M.fromList
                      [ (["f", "main"], TypeFunc [TypeK 1, TypeK 2] (TypeK 3)),
                        (["main"], TypeFunc [] (TypeK 0))
                      ]
                  )
                  $ typeCheckerFuncs newTypeChecker,
              typeCheckerLocals =
                M.fromList
                  [ (["x", "f", "main"], TypeK 1),
                    (["y", "f", "main"], TypeK 2)
                  ],
              typeCheckerForces =
                M.fromList
                  [ (0, TypeFunc [TypeK 1] (TypeK 4)),
                    (1, TypeFunc [TypeK 2] (TypeK 5))
                  ],
              typeCheckerPairs =
                [ (TypeK 8, TypeNone),
                  (TypeFunc [TypeInt] TypeNone, TypeFunc [TypeK 7] (TypeK 8)),
                  (TypeFunc [TypeK 1, TypeK 2] (TypeK 3), TypeFunc [TypeInt, TypeInt] (TypeK 7)),
                  (TypeK 3, TypeK 6),
                  (TypeFunc [TypeInt, TypeInt] TypeInt, TypeFunc [TypeK 4, TypeK 5] (TypeK 6))
                ]
            }
      ),
      ( StmtFunc
          "main"
          []
          [ StmtFunc "f" ["x"] [StmtReturn $ Just $ ExprVar "x"],
            StmtDecl "y" $ ExprLazy $ ExprCall (ExprForce (Just 0) $ ExprVar "f") [ExprInt 1],
            StmtVoid $ ExprCall (ExprVar "print") [ExprForce (Just 1) $ ExprVar "y"]
          ],
        Right $
          newTypeChecker
            { typeCheckerK = 7,
              typeCheckerFuncs =
                M.union
                  ( M.fromList
                      [ (["f", "main"], TypeFunc [TypeK 1] (TypeK 2)),
                        (["main"], TypeFunc [] (TypeK 0))
                      ]
                  )
                  $ typeCheckerFuncs newTypeChecker,
              typeCheckerLocals =
                M.fromList [(["x", "f", "main"], TypeK 1), (["y", "main"], TypeLazy (TypeK 4))],
              typeCheckerForces =
                M.fromList
                  [ (0, TypeFunc [TypeFunc [TypeK 1] (TypeK 2)] (TypeK 3)),
                    (1, TypeFunc [TypeLazy (TypeK 4)] (TypeK 5))
                  ],
              typeCheckerPairs =
                [ (TypeK 6, TypeNone),
                  (TypeFunc [TypeInt] TypeNone, TypeFunc [TypeK 5] (TypeK 6)),
                  (TypeK 3, TypeFunc [TypeInt] (TypeK 4)),
                  (TypeK 2, TypeK 1)
                ]
            }
      ),
      ( StmtFunc
          "main"
          []
          [ StmtFunc
              "add_0"
              ["x", "y"]
              [ StmtReturn $
                  Just $
                    ExprCall
                      (ExprVar "+")
                      [ExprForce (Just 0) $ ExprVar "x", ExprForce (Just 1) $ ExprVar "y"]
              ],
            StmtVoid $
              ExprCall
                (ExprVar "print")
                [ ExprForce (Just 3) $
                    ExprLazy $
                      ExprCall (ExprForce (Just 2) $ ExprVar "add_0") $
                        map ExprInt [4, 5]
                ]
          ],
        Right $
          newTypeChecker
            { typeCheckerK = 11,
              typeCheckerFuncs =
                M.union
                  ( M.fromList
                      [ (["add_0", "main"], TypeFunc [TypeK 1, TypeK 2] (TypeK 3)),
                        (["main"], TypeFunc [] (TypeK 0))
                      ]
                  )
                  $ typeCheckerFuncs newTypeChecker,
              typeCheckerLocals =
                M.fromList
                  [ (["x", "add_0", "main"], TypeK 1),
                    (["y", "add_0", "main"], TypeK 2)
                  ],
              typeCheckerForces =
                M.fromList
                  [ (0, TypeFunc [TypeK 1] (TypeK 4)),
                    (1, TypeFunc [TypeK 2] (TypeK 5)),
                    (2, TypeFunc [TypeFunc [TypeK 1, TypeK 2] (TypeK 3)] (TypeK 7)),
                    (3, TypeFunc [TypeLazy (TypeK 8)] (TypeK 9))
                  ],
              typeCheckerPairs =
                [ (TypeK 10, TypeNone),
                  (TypeFunc [TypeInt] TypeNone, TypeFunc [TypeK 9] (TypeK 10)),
                  (TypeK 7, TypeFunc [TypeInt, TypeInt] (TypeK 8)),
                  (TypeK 3, TypeK 6),
                  (TypeFunc [TypeInt, TypeInt] TypeInt, TypeFunc [TypeK 4, TypeK 5] (TypeK 6))
                ]
            }
      ),
      ( StmtFunc
          "main"
          []
          [ StmtFunc
              "add_0"
              ["x", "y"]
              [ StmtReturn $
                  Just $
                    ExprCall
                      (ExprVar "+")
                      [ ExprForce (Just 0) $ ExprVar "x",
                        ExprForce (Just 1) $ ExprVar "y"
                      ]
              ],
            StmtFunc
              "lazy_add_0"
              ["x"]
              [ StmtReturn $
                  Just $
                    ExprLazy $
                      ExprCall (ExprForce (Just 2) $ ExprVar "add_0") [ExprVar "x", ExprInt 1]
              ],
            StmtVoid $
              ExprCall
                (ExprVar "print")
                [ ExprForce
                    (Just 4)
                    $ ExprLazy
                    $ ExprCall (ExprForce (Just 3) $ ExprVar "lazy_add_0") [ExprInt 3]
                ],
            StmtVoid $
              ExprCall
                (ExprVar "print")
                [ ExprForce
                    (Just 6)
                    $ ExprLazy
                    $ ExprCall (ExprForce (Just 5) $ ExprVar "add_0")
                    $ map ExprInt [4, 5]
                ]
          ],
        Right $
          newTypeChecker
            { typeCheckerK = 19,
              typeCheckerFuncs =
                M.union
                  ( M.fromList
                      [ (["add_0", "main"], TypeFunc [TypeK 1, TypeK 2] (TypeK 3)),
                        (["lazy_add_0", "main"], TypeFunc [TypeK 7] (TypeK 8)),
                        (["main"], TypeFunc [] (TypeK 0))
                      ]
                  )
                  $ typeCheckerFuncs newTypeChecker,
              typeCheckerLocals =
                M.fromList
                  [ (["x", "add_0", "main"], TypeK 1),
                    (["x", "lazy_add_0", "main"], TypeK 7),
                    (["y", "add_0", "main"], TypeK 2)
                  ],
              typeCheckerForces =
                M.fromList
                  [ (0, TypeFunc [TypeK 1] (TypeK 4)),
                    (1, TypeFunc [TypeK 2] (TypeK 5)),
                    (2, TypeFunc [TypeFunc [TypeK 1, TypeK 2] (TypeK 3)] (TypeK 9)),
                    (3, TypeFunc [TypeFunc [TypeK 7] (TypeK 8)] (TypeK 11)),
                    (4, TypeFunc [TypeLazy (TypeK 12)] (TypeK 13)),
                    (5, TypeFunc [TypeFunc [TypeK 1, TypeK 2] (TypeK 3)] (TypeK 15)),
                    (6, TypeFunc [TypeLazy (TypeK 16)] (TypeK 17))
                  ],
              typeCheckerPairs =
                [ (TypeK 18, TypeNone),
                  (TypeFunc [TypeInt] TypeNone, TypeFunc [TypeK 17] (TypeK 18)),
                  (TypeK 15, TypeFunc [TypeInt, TypeInt] (TypeK 16)),
                  (TypeK 14, TypeNone),
                  (TypeFunc [TypeInt] TypeNone, TypeFunc [TypeK 13] (TypeK 14)),
                  (TypeK 11, TypeFunc [TypeInt] (TypeK 12)),
                  (TypeK 8, TypeLazy (TypeK 10)),
                  (TypeK 9, TypeFunc [TypeK 7, TypeInt] (TypeK 10)),
                  (TypeK 3, TypeK 6),
                  (TypeFunc [TypeInt, TypeInt] TypeInt, TypeFunc [TypeK 4, TypeK 5] (TypeK 6))
                ]
            }
      ),
      ( StmtFunc
          "main"
          []
          [ StmtFunc
              "add_0"
              ["x", "y"]
              [ StmtReturn $
                  Just $
                    ExprCall
                      (ExprVar "+")
                      [ExprForce (Just 0) $ ExprVar "x", ExprForce (Just 1) $ ExprVar "y"]
              ],
            StmtFunc
              "lazy_add_0"
              ["x"]
              [ StmtReturn $
                  Just $
                    ExprLazy $
                      ExprCall (ExprForce (Just 2) $ ExprVar "add_0") [ExprVar "x", ExprInt 1]
              ]
          ],
        Right $
          newTypeChecker
            { typeCheckerK = 11,
              typeCheckerFuncs =
                M.union
                  ( M.fromList
                      [ (["add_0", "main"], TypeFunc [TypeK 1, TypeK 2] (TypeK 3)),
                        (["lazy_add_0", "main"], TypeFunc [TypeK 7] (TypeK 8)),
                        (["main"], TypeFunc [] (TypeK 0))
                      ]
                  )
                  $ typeCheckerFuncs newTypeChecker,
              typeCheckerLocals =
                M.fromList
                  [ (["x", "add_0", "main"], TypeK 1),
                    (["x", "lazy_add_0", "main"], TypeK 7),
                    (["y", "add_0", "main"], TypeK 2)
                  ],
              typeCheckerForces =
                M.fromList
                  [ (0, TypeFunc [TypeK 1] (TypeK 4)),
                    (1, TypeFunc [TypeK 2] (TypeK 5)),
                    (2, TypeFunc [TypeFunc [TypeK 1, TypeK 2] (TypeK 3)] (TypeK 9))
                  ],
              typeCheckerPairs =
                [ (TypeK 8, TypeLazy (TypeK 10)),
                  (TypeK 9, TypeFunc [TypeK 7, TypeInt] (TypeK 10)),
                  (TypeK 3, TypeK 6),
                  (TypeFunc [TypeInt, TypeInt] TypeInt, TypeFunc [TypeK 4, TypeK 5] (TypeK 6))
                ]
            }
      ),
      ( StmtFunc
          "main"
          []
          [ StmtFunc
              "add_0"
              ["x", "y"]
              [ StmtReturn $
                  Just $
                    ExprCall
                      (ExprVar "+")
                      [ExprForce (Just 0) $ ExprVar "x", ExprForce (Just 1) $ ExprVar "y"]
              ],
            StmtFunc
              "lazy_add_0"
              ["x"]
              [ StmtReturn $
                  Just $
                    ExprLazy $
                      ExprCall (ExprForce (Just 2) $ ExprVar "add_0") [ExprVar "x", ExprInt 1]
              ],
            StmtVoid $
              ExprCall
                (ExprVar "print")
                [ ExprForce (Just 4) $
                    ExprLazy $
                      ExprCall (ExprForce (Just 3) $ ExprVar "lazy_add_0") [ExprInt 3]
                ]
          ],
        Right $
          newTypeChecker
            { typeCheckerK = 15,
              typeCheckerFuncs =
                M.union
                  ( M.fromList
                      [ (["add_0", "main"], TypeFunc [TypeK 1, TypeK 2] (TypeK 3)),
                        (["lazy_add_0", "main"], TypeFunc [TypeK 7] (TypeK 8)),
                        (["main"], TypeFunc [] (TypeK 0))
                      ]
                  )
                  $ typeCheckerFuncs newTypeChecker,
              typeCheckerLocals =
                M.fromList
                  [ (["x", "add_0", "main"], TypeK 1),
                    (["x", "lazy_add_0", "main"], TypeK 7),
                    (["y", "add_0", "main"], TypeK 2)
                  ],
              typeCheckerForces =
                M.fromList
                  [ (0, TypeFunc [TypeK 1] (TypeK 4)),
                    (1, TypeFunc [TypeK 2] (TypeK 5)),
                    (2, TypeFunc [TypeFunc [TypeK 1, TypeK 2] (TypeK 3)] (TypeK 9)),
                    (3, TypeFunc [TypeFunc [TypeK 7] (TypeK 8)] (TypeK 11)),
                    (4, TypeFunc [TypeLazy (TypeK 12)] (TypeK 13))
                  ],
              typeCheckerPairs =
                [ (TypeK 14, TypeNone),
                  (TypeFunc [TypeInt] TypeNone, TypeFunc [TypeK 13] (TypeK 14)),
                  (TypeK 11, TypeFunc [TypeInt] (TypeK 12)),
                  (TypeK 8, TypeLazy (TypeK 10)),
                  (TypeK 9, TypeFunc [TypeK 7, TypeInt] (TypeK 10)),
                  (TypeK 3, TypeK 6),
                  (TypeFunc [TypeInt, TypeInt] TypeInt, TypeFunc [TypeK 4, TypeK 5] (TypeK 6))
                ]
            }
      )
    ]

{- % -}

unify :: [(Type, Type)] -> StateT (M.Map Int Type) (Either Error) ()
unify [] = return ()
unify pairs = mapM (uncurry unify1) pairs >>= unify . concat

unify1 :: Type -> Type -> StateT (M.Map Int Type) (Either Error) [(Type, Type)]
unify1 left right
  | left == right = return []
unify1 left@(TypeFunc leftArgs leftReturn) right@(TypeFunc rightArgs rightReturn)
  | length leftArgs /= length rightArgs = lift $ Left $ ErrorUnify1 left right
  | otherwise = return $ zip (leftReturn : leftArgs) (rightReturn : rightArgs)
unify1 (TypeLazy left) (TypeLazy right) = unify1 left right
unify1 (TypeK k) right = do
  insertK k right
  return []
unify1 left (TypeK k) = do
  insertK k left
  return []
unify1 left right = lift $ Left $ ErrorUnify1 left right

insertK :: Int -> Type -> StateT (M.Map Int Type) (Either Error) ()
insertK k newType = do
  types <- get
  case M.lookup k types of
    Just oldType -> unify1 oldType newType >>= unify
    Nothing -> put $ M.insert k newType types

testUnify :: [Test]
testUnify =
  map
    (uncurry (~?=) . first ((`execStateT` mempty) . unify))
    [ ([], Right mempty),
      ([(TypeNone, TypeNone)], Right mempty),
      ([(TypeK 0, TypeNone)], Right $ M.singleton 0 TypeNone),
      ([(TypeK 0, TypeK 1)], Right $ M.singleton 0 $ TypeK 1),
      ([(TypeFunc [TypeK 0] TypeInt, TypeFunc [TypeInt] $ TypeK 0)], Right $ M.singleton 0 TypeInt),
      ( [(TypeFunc [TypeK 0] $ TypeFunc [] TypeNone, TypeFunc [TypeInt] $ TypeK 1)],
        Right $ M.fromList [(0, TypeInt), (1, TypeFunc [] TypeNone)]
      ),
      ( [(TypeFunc [] TypeNone, TypeFunc [TypeInt] TypeNone)],
        Left $ ErrorUnify1 (TypeFunc [] TypeNone) (TypeFunc [TypeInt] TypeNone)
      ),
      ([(TypeNone, TypeInt)], Left $ ErrorUnify1 TypeNone TypeInt),
      ( [(TypeK 1, TypeK 0), (TypeK 2, TypeK 0), (TypeK 0, TypeInt)],
        Right $ M.fromList [(0, TypeInt), (1, TypeK 0), (2, TypeK 0)]
      ),
      ( [ (TypeFunc [] $ TypeK 0, TypeFunc [] $ TypeK 1),
          (TypeFunc [] $ TypeK 0, TypeFunc [] $ TypeK 2),
          (TypeFunc [] $ TypeK 0, TypeFunc [] TypeInt)
        ],
        Right $ M.fromList [(0, TypeK 1), (1, TypeK 2), (2, TypeInt)]
      )
    ]

{- % -}

deref :: (Monad m) => Type -> StateT (M.Map Int Type) m Type
deref parentType@(TypeK parentK) = do
  types <- get
  case M.lookup parentK types of
    Just childType0 -> do
      modify $ M.delete parentK
      childType1 <- deref childType0
      modify $ M.insert parentK childType1
      return childType1
    Nothing -> return parentType
deref (TypeFunc argTypes returnType) = TypeFunc <$> mapM deref argTypes <*> deref returnType
deref (TypeLazy childType) = TypeLazy <$> deref childType
deref TypeInt = return TypeInt
deref TypeNone = return TypeNone

testDeref :: [Test]
testDeref =
  map
    ( uncurry (~?=)
        . bimap
          ( uncurry evalStateT
              . first (deref :: Type -> StateT (M.Map Int Type) (Either Error) Type)
          )
          Right
    )
    [ ((TypeK 0, mempty), TypeK 0),
      ((TypeK 0, M.singleton 0 TypeInt), TypeInt),
      ((TypeK 0, M.fromList [(0, TypeK 1), (1, TypeK 0)]), TypeK 0),
      ((TypeK 0, M.singleton 0 $ TypeFunc [TypeK 0] $ TypeK 0), TypeFunc [TypeK 0] $ TypeK 0),
      ( (TypeK 0, M.fromList [(0, TypeFunc [TypeK 1] $ TypeK 1), (1, TypeK 0)]),
        TypeFunc [TypeK 0] $ TypeK 0
      )
    ]

{- % -}

rewriteExpr :: M.Map Int Type -> Expr -> StateT (M.Map Int Type) (Either Error) Expr
rewriteExpr forces parentExpr@(ExprForce (Just k) childExpr0) =
  case M.lookup k forces of
    Just (TypeFunc [argType0] returnType0) -> do
      argType1 <- deref argType0
      returnType1 <- deref returnType0
      childExpr1 <- rewriteExpr forces childExpr0
      rewriteExprForce returnType1 argType1 childExpr1
    _ -> lift $ Left $ ErrorRewriteExpr parentExpr
rewriteExpr _ expr@(ExprForce Nothing _) = lift $ Left $ ErrorRewriteExpr expr
rewriteExpr forces (ExprCall func args) =
  ExprCall <$> rewriteExpr forces func <*> mapM (rewriteExpr forces) args
rewriteExpr _ expr@(ExprInt {}) = return expr
rewriteExpr _ expr@(ExprVar {}) = return expr
rewriteExpr forces (ExprLazy expr) = ExprLazy <$> rewriteExpr forces expr

rewriteExprForce :: Type -> Type -> Expr -> StateT (M.Map Int Type) (Either Error) Expr
rewriteExprForce expectedType@(TypeLazy {}) givenType expr =
  lift $ Left $ ErrorRewriteExprForce expectedType givenType expr
rewriteExprForce expectedType@(TypeK {}) givenType expr =
  lift $ Left $ ErrorRewriteExprForce expectedType givenType expr
rewriteExprForce expectedType (TypeLazy givenType) expr =
  rewriteExprForce expectedType givenType $ ExprForce Nothing expr
rewriteExprForce expectedType givenType expr = do
  unify [(expectedType, givenType)]
  return expr

rewriteStmt :: M.Map Int Type -> Stmt -> StateT (M.Map Int Type) (Either Error) Stmt
rewriteStmt = mapStmt . rewriteExpr

testRewriteStmt :: [Test]
testRewriteStmt =
  map
    (uncurry (~?=) . first ((`evalStateT` mempty) . uncurry rewriteStmt))
    [ ( (mempty, StmtVoid $ ExprForce (Just 0) $ ExprVar "x"),
        Left $ ErrorRewriteExpr $ ExprForce (Just 0) $ ExprVar "x"
      ),
      ( (M.singleton 0 TypeNone, StmtVoid $ ExprForce (Just 0) $ ExprVar "x"),
        Left $ ErrorRewriteExpr $ ExprForce (Just 0) $ ExprVar "x"
      ),
      ( (M.singleton 0 $ TypeFunc [TypeInt] TypeInt, StmtVoid $ ExprForce (Just 0) $ ExprVar "x"),
        Right $ StmtVoid $ ExprVar "x"
      ),
      ( (M.singleton 0 $ TypeFunc [TypeInt] TypeNone, StmtVoid $ ExprForce (Just 0) $ ExprVar "x"),
        Left $ ErrorUnify1 TypeNone TypeInt
      ),
      ( ( M.singleton 0 $ TypeFunc [TypeInt] $ TypeFunc [] TypeInt,
          StmtVoid $ ExprForce (Just 0) $ ExprVar "x"
        ),
        Left $ ErrorUnify1 (TypeFunc [] TypeInt) TypeInt
      ),
      ( ( M.singleton 0 $ TypeFunc [TypeLazy $ TypeLazy TypeInt] TypeInt,
          StmtVoid $ ExprForce (Just 0) $ ExprVar "x"
        ),
        Right $ StmtVoid $ ExprForce Nothing $ ExprForce Nothing $ ExprVar "x"
      ),
      ( ( M.singleton 0 $ TypeFunc [TypeLazy $ TypeLazy $ TypeK 0] $ TypeK 0,
          StmtVoid $ ExprForce (Just 0) $ ExprVar "x"
        ),
        Left $ ErrorRewriteExprForce (TypeK 0) (TypeLazy $ TypeLazy $ TypeK 0) $ ExprVar "x"
      ),
      ( ( M.singleton 0 $ TypeFunc [TypeLazy TypeNone] TypeNone,
          StmtVoid $ ExprForce (Just 0) $ ExprVar "x"
        ),
        Right $ StmtVoid $ ExprForce Nothing $ ExprVar "x"
      ),
      ( ( M.fromList
            [ (0, TypeFunc [TypeLazy TypeInt] TypeInt),
              (1, TypeFunc [TypeInt] TypeInt)
            ],
          StmtFunc
            "f"
            ["x", "y"]
            [ StmtReturn $
                Just $
                  ExprCall
                    (ExprVar "+")
                    [ExprForce (Just 0) $ ExprVar "x", ExprForce (Just 1) $ ExprVar "y"]
            ]
        ),
        Right $
          StmtFunc
            "f"
            ["x", "y"]
            [ StmtReturn $
                Just $
                  ExprCall (ExprVar "+") [ExprForce Nothing $ ExprVar "x", ExprVar "y"]
            ]
      ),
      ( ( M.fromList
            [ (0, TypeFunc [TypeK 0] $ TypeK 1),
              (1, TypeFunc [TypeK 2] $ TypeK 3)
            ],
          StmtFunc
            "f"
            ["x", "y"]
            [ StmtReturn $
                Just $
                  ExprCall
                    (ExprVar "+")
                    [ExprForce (Just 0) $ ExprVar "x", ExprForce (Just 1) $ ExprVar "y"]
            ]
        ),
        Left $ ErrorRewriteExprForce (TypeK 1) (TypeK 0) $ ExprVar "x"
      ),
      ( ( M.singleton 0 $ TypeFunc [TypeLazy $ TypeLazy TypeInt] TypeInt,
          StmtVoid $ ExprCall (ExprVar "print") [ExprForce (Just 0) $ ExprVar "x"]
        ),
        Right $
          StmtVoid $
            ExprCall (ExprVar "print") [ExprForce Nothing $ ExprForce Nothing $ ExprVar "x"]
      ),
      ( ( M.singleton 0 $ TypeFunc [TypeLazy $ TypeLazy TypeInt] $ TypeK 0,
          StmtVoid $ ExprCall (ExprVar "print") [ExprForce (Just 0) $ ExprVar "x"]
        ),
        Left $ ErrorRewriteExprForce (TypeK 0) (TypeLazy $ TypeLazy TypeInt) $ ExprVar "x"
      )
    ]

{- % -}

lazify :: Stmt -> Either Error Stmt
lazify stmt = do
  lazyStmt <- evalStateT (stmtToLazy stmt) 0
  typeChecker <- execStateT (stmtToType lazyStmt) newTypeChecker
  pair <- case M.lookup ["main"] $ typeCheckerFuncs typeChecker of
    Just mainType -> return (mainType, TypeFunc [] TypeNone)
    Nothing -> Left ErrorLazify
  types0 <- execStateT (unify (pair : typeCheckerPairs typeChecker)) mempty
  types1 <- execStateT (mapM unifyForces $ M.elems $ typeCheckerForces typeChecker) types0
  evalStateT (rewriteStmt (typeCheckerForces typeChecker) lazyStmt) types1

unifyForces :: Type -> StateT (M.Map Int Type) (Either Error) ()
unifyForces (TypeFunc [argType0] returnType0) = do
  argType1 <- deref argType0
  returnType1 <- deref returnType0
  case (argType1, returnType1) of
    (TypeFunc {}, TypeFunc {}) -> unify [(argType1, returnType1)]
    _ -> return ()
unifyForces forceType = lift $ Left $ ErrorUnifyForces forceType

testLazify :: [Test]
testLazify =
  map
    (uncurry (~?=) . first lazify)
    [ (StmtDecl "y" $ ExprVar "x", Left $ ErrorExprToType $ ExprVar "x"),
      ( StmtFunc
          "main"
          []
          [ StmtFunc "f" ["x"] [StmtReturn $ Just $ ExprVar "x"],
            StmtDecl "y" $ ExprCall (ExprVar "f") [ExprInt 1],
            StmtVoid $ ExprCall (ExprVar "print") [ExprVar "y"]
          ],
        Right $
          StmtFunc
            "main"
            []
            [ StmtFunc "f" ["x"] [StmtReturn $ Just $ ExprVar "x"],
              StmtDecl "y" $ ExprLazy $ ExprCall (ExprVar "f") [ExprInt 1],
              StmtVoid $ ExprCall (ExprVar "print") [ExprForce Nothing $ ExprVar "y"]
            ]
      ),
      ( StmtFunc
          "main"
          []
          [ StmtFunc "f0" ["x"] [StmtReturn $ Just $ ExprVar "x"],
            StmtFunc "f1" ["x"] [StmtReturn $ Just $ ExprVar "x"],
            StmtDecl "y" $ ExprCall (ExprVar "f0") [ExprVar "f1"],
            StmtReturn Nothing
          ],
        Right $
          StmtFunc
            "main"
            []
            [ StmtFunc "f0" ["x"] [StmtReturn $ Just $ ExprVar "x"],
              StmtFunc "f1" ["x"] [StmtReturn $ Just $ ExprVar "x"],
              StmtDecl "y" $ ExprLazy $ ExprCall (ExprVar "f0") [ExprVar "f1"],
              StmtReturn Nothing
            ]
      ),
      (StmtFunc "main" [] [StmtReturn $ Just $ ExprInt 0], Left $ ErrorUnify1 TypeInt TypeNone),
      ( StmtFunc
          "main"
          []
          [ StmtFunc "f0" [] [StmtReturn $ Just $ ExprInt 1],
            StmtFunc "f1" [] [StmtReturn $ Just $ ExprVar "f0"],
            StmtDecl "x0" $ ExprCall (ExprVar "f1") [],
            StmtDecl "x1" $ ExprCall (ExprVar "x0") [],
            StmtVoid $ ExprCall (ExprVar "print") [ExprVar "x1"]
          ],
        Right $
          StmtFunc
            "main"
            []
            [ StmtFunc "f0" [] [StmtReturn $ Just $ ExprInt 1],
              StmtFunc "f1" [] [StmtReturn $ Just $ ExprVar "f0"],
              StmtDecl "x0" $ ExprLazy $ ExprCall (ExprVar "f1") [],
              StmtDecl "x1" $ ExprLazy $ ExprCall (ExprForce Nothing $ ExprVar "x0") [],
              StmtVoid $ ExprCall (ExprVar "print") [ExprForce Nothing $ ExprVar "x1"]
            ]
      ),
      ( StmtFunc
          "main"
          []
          [ StmtFunc
              "add_0"
              ["x", "y"]
              [StmtReturn $ Just $ ExprCall (ExprVar "+") $ map ExprVar ["x", "y"]],
            StmtFunc
              "lazy_add_0"
              ["x"]
              [StmtReturn $ Just $ ExprCall (ExprVar "add_0") [ExprVar "x", ExprInt 1]],
            StmtVoid $ ExprCall (ExprVar "print") [ExprCall (ExprVar "lazy_add_0") [ExprInt 3]]
          ],
        Right $
          StmtFunc
            "main"
            []
            [ StmtFunc
                "add_0"
                ["x", "y"]
                [StmtReturn $ Just $ ExprCall (ExprVar "+") $ map ExprVar ["x", "y"]],
              StmtFunc
                "lazy_add_0"
                ["x"]
                [ StmtReturn $
                    Just $
                      ExprLazy $
                        ExprCall (ExprVar "add_0") [ExprVar "x", ExprInt 1]
                ],
              StmtVoid $
                ExprCall
                  (ExprVar "print")
                  [ ExprForce Nothing $
                      ExprForce Nothing $
                        ExprLazy $
                          ExprCall (ExprVar "lazy_add_0") [ExprInt 3]
                  ]
            ]
      ),
      ( StmtFunc
          "main"
          []
          [ StmtFunc
              "add_0"
              ["x", "y"]
              [StmtReturn $ Just $ ExprCall (ExprVar "+") $ map ExprVar ["x", "y"]],
            StmtVoid $ ExprCall (ExprVar "print") [ExprCall (ExprVar "add_0") $ map ExprInt [4, 5]]
          ],
        Right $
          StmtFunc
            "main"
            []
            [ StmtFunc
                "add_0"
                ["x", "y"]
                [StmtReturn $ Just $ ExprCall (ExprVar "+") $ map ExprVar ["x", "y"]],
              StmtVoid $
                ExprCall
                  (ExprVar "print")
                  [ExprForce Nothing $ ExprLazy $ ExprCall (ExprVar "add_0") $ map ExprInt [4, 5]]
            ]
      ),
      ( StmtFunc
          "main"
          []
          [ StmtFunc
              "add_0"
              ["x", "y"]
              [StmtReturn $ Just $ ExprCall (ExprVar "+") $ map ExprVar ["x", "y"]],
            StmtFunc
              "lazy_add_0"
              ["x"]
              [StmtReturn $ Just $ ExprCall (ExprVar "add_0") [ExprVar "x", ExprInt 1]],
            StmtVoid $ ExprCall (ExprVar "print") [ExprCall (ExprVar "lazy_add_0") [ExprInt 3]],
            StmtVoid $ ExprCall (ExprVar "print") [ExprCall (ExprVar "add_0") $ map ExprInt [4, 5]]
          ],
        Right $
          StmtFunc
            "main"
            []
            [ StmtFunc
                "add_0"
                ["x", "y"]
                [StmtReturn $ Just $ ExprCall (ExprVar "+") $ map ExprVar ["x", "y"]],
              StmtFunc
                "lazy_add_0"
                ["x"]
                [ StmtReturn $
                    Just $
                      ExprLazy $
                        ExprCall (ExprVar "add_0") [ExprVar "x", ExprInt 1]
                ],
              StmtVoid $
                ExprCall
                  (ExprVar "print")
                  [ ExprForce Nothing $
                      ExprForce Nothing $
                        ExprLazy $
                          ExprCall (ExprVar "lazy_add_0") [ExprInt 3]
                  ],
              StmtVoid $
                ExprCall
                  (ExprVar "print")
                  [ExprForce Nothing $ ExprLazy $ ExprCall (ExprVar "add_0") $ map ExprInt [4, 5]]
            ]
      ),
      ( StmtFunc
          "main"
          []
          [ StmtFunc "f0" ["x"] [StmtReturn $ Just $ ExprVar "x"],
            StmtFunc "f1" [] [StmtReturn $ Just $ ExprVar "f0"],
            StmtFunc "f2" [] [StmtReturn $ Just $ ExprVar "f1"],
            StmtDecl "y" $ ExprCall (ExprCall (ExprCall (ExprVar "f2") []) []) [ExprInt 1],
            StmtVoid $ ExprCall (ExprVar "print") [ExprVar "y"]
          ],
        Right $
          StmtFunc
            "main"
            []
            [ StmtFunc "f0" ["x"] [StmtReturn $ Just $ ExprVar "x"],
              StmtFunc "f1" [] [StmtReturn $ Just $ ExprVar "f0"],
              StmtFunc "f2" [] [StmtReturn $ Just $ ExprVar "f1"],
              StmtDecl "y" $
                ExprLazy $
                  ExprCall
                    ( ExprForce Nothing $
                        ExprLazy $
                          ExprCall (ExprForce Nothing $ ExprLazy $ ExprCall (ExprVar "f2") []) []
                    )
                    [ExprInt 1],
              StmtVoid $ ExprCall (ExprVar "print") [ExprForce Nothing $ ExprVar "y"]
            ]
      )
    ]

main :: IO ()
main = do
  _ <-
    runTestTT $
      TestList $
        concat
          [ testExprToLazy,
            testStmtToLazy,
            testStmtToType,
            testUnify,
            testDeref,
            testRewriteStmt,
            testLazify
          ]
  putStr $ show stmt
  either print (putStr . show) $ lazify stmt
  where
    stmt =
      StmtFunc
        "main"
        []
        [ StmtFunc
            "add_0"
            ["x", "y"]
            [StmtReturn $ Just $ ExprCall (ExprVar "+") $ map ExprVar ["x", "y"]],
          StmtFunc
            "add_1"
            ["x", "y"]
            [StmtReturn $ Just $ ExprCall (ExprVar "+") $ map ExprVar ["x", "y"]],
          StmtFunc
            "lazy_add_0"
            ["x"]
            [StmtReturn $ Just $ ExprCall (ExprVar "add_0") [ExprVar "x", ExprInt 1]],
          StmtFunc
            "lazy_add_1"
            ["x"]
            [StmtReturn $ Just $ ExprCall (ExprVar "add_1") [ExprVar "x", ExprInt 1]],
          StmtDecl "x" $
            ExprCall (ExprVar "lazy_add_1") [ExprCall (ExprVar "lazy_add_0") [ExprInt (-2)]],
          StmtDecl "y" $ ExprVar "x",
          StmtVoid $ ExprCall (ExprVar "print") [ExprInt 1],
          StmtVoid $ ExprCall (ExprVar "print") [ExprVar "x"],
          StmtVoid $ ExprCall (ExprVar "print") [ExprVar "y"],
          StmtVoid $ ExprCall (ExprVar "print") [ExprCall (ExprVar "lazy_add_0") [ExprInt 3]],
          StmtVoid $ ExprCall (ExprVar "print") [ExprCall (ExprVar "lazy_add_1") [ExprVar "x"]],
          StmtVoid $ ExprCall (ExprVar "print") [ExprCall (ExprVar "add_0") $ map ExprInt [4, 5]]
        ]
