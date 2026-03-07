import Control.Applicative ((<|>))
import Control.Monad ((<=<))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy
  ( State,
    StateT,
    evalState,
    evalStateT,
    execStateT,
    get,
    gets,
    modify,
    put,
  )
import Data.Bifunctor (first)
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Test.HUnit (Test (..), runTestTT, (~?=))

{- % -}

data Expr
  = ExprInt Int
  | ExprVar String
  | ExprCall Expr [Expr]
  | ExprLazy Expr [Expr]
  | ExprForce (Maybe Int) Expr
  deriving (Eq)

instance Show Expr where
  show (ExprInt int) = show int
  show (ExprVar var) = var
  show (ExprCall (ExprVar "+") [left, right]) = "(" ++ show left ++ " + " ++ show right ++ ")"
  show (ExprCall func args) = show func ++ "(" ++ intercalate ", " (map show args) ++ ")"
  show (ExprLazy func args) = "Lazy(" ++ intercalate ", " (map show $ func : args) ++ ")"
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
    [indent k, "def ", var, "(", intercalate ", " args, "):\n", concatMap (showStmt $ k + 1) stmts]
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
    typeCheckerBindings :: M.Map [String] Type,
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
  | ErrorRewriteExprForce Expr
  | ErrorStmtToType Stmt
  | ErrorUnify1 Type Type
  deriving (Eq, Show)

{- % -}

intrinsics :: M.Map String Type
intrinsics =
  M.fromList [("print", TypeFunc [TypeInt] TypeNone), ("+", TypeFunc [TypeInt, TypeInt] TypeInt)]

newTypeChecker :: TypeChecker
newTypeChecker = TypeChecker 0 (M.mapKeys (: []) intrinsics) mempty [] [] []

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
exprToLazy (ExprCall func args) = ExprLazy <$> exprToLazy func <*> mapM exprToLazy args
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
    (uncurry (~?=) . first (`evalStateT` 0))
    [ ( exprToLazy $ ExprCall (ExprVar "f") $ map ExprVar ["x", "y"],
        Right $ ExprLazy (ExprVar "f") $ map ExprVar ["x", "y"]
      ),
      ( exprToLazy $
          ExprCall (ExprVar "f") [ExprVar "x", ExprCall (ExprVar "f") $ map ExprVar ["y", "z"]],
        Right $
          ExprLazy (ExprVar "f") $
            ExprVar "x" : [ExprLazy (ExprVar "f") $ map ExprVar ["y", "z"]]
      ),
      ( exprToLazy $ ExprCall (ExprVar "print") [ExprInt 1],
        Right $ ExprCall (ExprVar "print") [ExprInt 1]
      ),
      ( exprToLazy $ ExprCall (ExprVar "print") [ExprVar "x"],
        Right $ ExprCall (ExprVar "print") [ExprForce (Just 0) $ ExprVar "x"]
      ),
      ( exprToLazy $ ExprCall (ExprVar "print") [ExprCall (ExprVar "f") []],
        Right $ ExprCall (ExprVar "print") [ExprForce (Just 0) $ ExprLazy (ExprVar "f") []]
      )
    ]

{- % -}

stmtToLazy :: Stmt -> StateT Int (Either Error) Stmt
stmtToLazy = mapStmt exprToLazy

testStmtToLazy :: [Test]
testStmtToLazy =
  map
    (uncurry (~?=) . first (`evalStateT` 0))
    [ (stmtToLazy $ StmtDecl "x" $ ExprInt 0, Right $ StmtDecl "x" $ ExprInt 0),
      ( stmtToLazy $ StmtDecl "z" $ ExprCall (ExprVar "f") $ map ExprVar ["x", "y"],
        Right $ StmtDecl "z" $ ExprLazy (ExprVar "f") $ map ExprVar ["x", "y"]
      )
    ]

{- % -}

nextTypeK :: (Monad m) => StateT TypeChecker m Type
nextTypeK = do
  typeChecker <- get
  let k = typeCheckerK typeChecker
  let typeK = TypeK k
  put $ typeChecker {typeCheckerK = k + 1}
  return typeK

pushBinding :: (Monad m) => String -> Type -> StateT TypeChecker m ()
pushBinding var varType = do
  modify $ \typeChecker ->
    typeChecker
      { typeCheckerBindings =
          M.insert (var : typeCheckerFuncStack typeChecker) varType $ typeCheckerBindings typeChecker
      }

argToType :: (Monad m) => String -> StateT TypeChecker m Type
argToType arg = do
  argType <- nextTypeK
  pushBinding arg argType
  return argType

{- % -}

exprToType :: Expr -> StateT TypeChecker (Either Error) Type
exprToType (ExprInt _) = return TypeInt
exprToType expr@(ExprVar var) = do
  bindings <- gets typeCheckerBindings
  funcStack <- gets typeCheckerFuncStack
  maybe (lift $ Left $ ErrorExprToType expr) return $ exprToTypeLoop bindings var funcStack
exprToType (ExprCall func args) = do
  funcType <- exprToType func
  argTypes <- mapM exprToType args
  returnType <- nextTypeK
  modify $ \typeChecker ->
    typeChecker
      { typeCheckerPairs = (funcType, TypeFunc argTypes returnType) : typeCheckerPairs typeChecker
      }
  return returnType
exprToType (ExprLazy func args) = TypeLazy <$> exprToType (ExprCall func args)
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
exprToTypeLoop bindings var [] = M.lookup [var] bindings
exprToTypeLoop bindings var funcStack@(_ : rest) =
  M.lookup (var : funcStack) bindings <|> exprToTypeLoop bindings var rest

{- % -}

stmtToType :: Stmt -> StateT TypeChecker (Either Error) ()
stmtToType stmt@(StmtDecl var expr) = do
  exprType <- exprToType expr
  bindings <- gets typeCheckerBindings
  funcStack <- gets typeCheckerFuncStack
  _ <- case M.lookup (var : funcStack) bindings of
    Just _ -> lift $ Left $ ErrorStmtToType stmt
    Nothing -> return ()
  pushBinding var exprType
stmtToType (StmtFunc var args stmts) = do
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
  pushBinding var (TypeFunc argTypes returnType)
stmtToType (StmtVoid expr) = do
  exprType <- exprToType expr
  modify $ \typeChecker ->
    typeChecker {typeCheckerPairs = (exprType, TypeNone) : typeCheckerPairs typeChecker}
stmtToType (StmtReturn (Just expr)) = do
  exprType <- exprToType expr
  modify $ \typeChecker ->
    typeChecker {typeCheckerReturnTypes = exprType : typeCheckerReturnTypes typeChecker}
stmtToType (StmtReturn Nothing) = do
  modify $ \typeChecker ->
    typeChecker {typeCheckerReturnTypes = TypeNone : typeCheckerReturnTypes typeChecker}

testStmtToType :: [Test]
testStmtToType =
  map
    (uncurry (~?=))
    [ ( execStateT
          (stmtToType $ StmtFunc "add" ["x"] [StmtReturn $ Just $ ExprVar "x"])
          newTypeChecker,
        Right $
          newTypeChecker
            { typeCheckerK = 2,
              typeCheckerBindings =
                M.union
                  (M.fromList [(["add"], TypeFunc [TypeK 0] $ TypeK 1), (["x", "add"], TypeK 0)])
                  $ typeCheckerBindings newTypeChecker,
              typeCheckerPairs = [(TypeK 1, TypeK 0)]
            }
      ),
      ( execStateT
          (stmtToType $ StmtFunc "add" ["x"] [StmtReturn $ Just $ ExprForce (Just 0) $ ExprVar "x"])
          newTypeChecker,
        Right $
          newTypeChecker
            { typeCheckerK = 3,
              typeCheckerBindings =
                M.union
                  (M.fromList [(["add"], TypeFunc [TypeK 0] $ TypeK 1), (["x", "add"], TypeK 0)])
                  $ typeCheckerBindings newTypeChecker,
              typeCheckerForces = M.singleton 0 $ TypeFunc [TypeK 0] $ TypeK 2,
              typeCheckerPairs = [(TypeK 1, TypeK 2)]
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
    (uncurry (~?=) . first (`execStateT` mempty))
    [ (unify [], Right mempty),
      (unify [(TypeNone, TypeNone)], Right mempty),
      (unify [(TypeK 0, TypeNone)], Right $ M.singleton 0 TypeNone),
      (unify [(TypeK 0, TypeK 1)], Right $ M.singleton 0 $ TypeK 1),
      ( unify [(TypeFunc [TypeK 0] TypeInt, TypeFunc [TypeInt] $ TypeK 0)],
        Right $ M.singleton 0 TypeInt
      ),
      ( unify [(TypeFunc [TypeK 0] $ TypeFunc [] TypeNone, TypeFunc [TypeInt] $ TypeK 1)],
        Right $ M.fromList [(0, TypeInt), (1, TypeFunc [] TypeNone)]
      ),
      ( unify [(TypeFunc [] TypeNone, TypeFunc [TypeInt] TypeNone)],
        Left $ ErrorUnify1 (TypeFunc [] TypeNone) (TypeFunc [TypeInt] TypeNone)
      ),
      (unify [(TypeNone, TypeInt)], Left $ ErrorUnify1 TypeNone TypeInt)
    ]

{- % -}

derefK :: Int -> State (M.Map Int Type) (Maybe Type)
derefK parentK = do
  types <- get
  case M.lookup parentK types of
    Just parentType@(TypeK childK) -> do
      put $ M.delete parentK types
      childType <- fromMaybe parentType <$> derefK childK
      modify $ M.insert parentK childType
      return $ Just childType
    Just (TypeLazy childType) -> Just . TypeLazy <$> deref childType
    Just (TypeFunc argTypes returnType) ->
      Just <$> (TypeFunc <$> mapM deref argTypes <*> deref returnType)
    Just parentType -> return $ Just parentType
    Nothing -> return Nothing

deref :: Type -> State (M.Map Int Type) Type
deref parentType@(TypeK parentK) = do
  maybeType <- derefK parentK
  case maybeType of
    Just childType -> return childType
    Nothing -> return parentType
deref (TypeFunc argTypes returnType) = TypeFunc <$> mapM deref argTypes <*> deref returnType
deref (TypeLazy childType) = TypeLazy <$> deref childType
deref parentType = return parentType

{- % -}

rewriteExpr :: M.Map Int Type -> Expr -> Either Error Expr
rewriteExpr forces parentExpr@(ExprForce (Just k) childExpr) =
  case M.lookup k forces of
    Just (TypeFunc [argType] returnType) -> rewriteExprForce returnType argType childExpr
    _ -> Left $ ErrorRewriteExpr parentExpr
rewriteExpr _ expr@(ExprForce Nothing _) = Left $ ErrorRewriteExpr expr
rewriteExpr forces (ExprCall func args) =
  ExprCall <$> rewriteExpr forces func <*> mapM (rewriteExpr forces) args
rewriteExpr _ expr@(ExprInt {}) = Right expr
rewriteExpr _ expr@(ExprVar {}) = Right expr
rewriteExpr _ expr@(ExprLazy {}) = Right expr

rewriteExprForce :: Type -> Type -> Expr -> Either Error Expr
rewriteExprForce exprType (TypeLazy childType) expr =
  rewriteExprForce exprType childType $ ExprForce Nothing expr
rewriteExprForce _ (TypeK _) expr = Left $ ErrorRewriteExprForce expr
rewriteExprForce _ TypeNone expr = Left $ ErrorRewriteExprForce expr
rewriteExprForce expectedType givenType expr
  | expectedType /= givenType = Left $ ErrorRewriteExprForce expr
  | otherwise = Right expr

rewriteStmt :: M.Map Int Type -> Stmt -> Either Error Stmt
rewriteStmt = mapStmt . rewriteExpr

testRewriteStmt :: [Test]
testRewriteStmt =
  map
    (uncurry (~?=))
    [ ( rewriteStmt mempty $ StmtVoid $ ExprForce (Just 0) $ ExprVar "x",
        Left $ ErrorRewriteExpr $ ExprForce (Just 0) $ ExprVar "x"
      ),
      ( rewriteStmt (M.singleton 0 TypeNone) $ StmtVoid $ ExprForce (Just 0) $ ExprVar "x",
        Left $ ErrorRewriteExpr $ ExprForce (Just 0) $ ExprVar "x"
      ),
      ( rewriteStmt (M.singleton 0 $ TypeFunc [TypeInt] TypeInt) $
          StmtVoid $
            ExprForce (Just 0) $
              ExprVar "x",
        Right $ StmtVoid $ ExprVar "x"
      ),
      ( rewriteStmt (M.singleton 0 $ TypeFunc [TypeInt] $ TypeFunc [] TypeInt) $
          StmtVoid $
            ExprForce (Just 0) $
              ExprVar "x",
        Left $ ErrorRewriteExprForce $ ExprVar "x"
      ),
      ( rewriteStmt (M.singleton 0 $ TypeFunc [TypeLazy $ TypeLazy TypeInt] TypeInt) $
          StmtVoid $
            ExprForce (Just 0) $
              ExprVar "x",
        Right $ StmtVoid $ ExprForce Nothing $ ExprForce Nothing $ ExprVar "x"
      ),
      ( rewriteStmt (M.singleton 0 $ TypeFunc [TypeLazy $ TypeLazy $ TypeK 0] $ TypeK 0) $
          StmtVoid $
            ExprForce (Just 0) $
              ExprVar "x",
        Left $ ErrorRewriteExprForce $ ExprForce Nothing $ ExprForce Nothing $ ExprVar "x"
      ),
      ( rewriteStmt (M.singleton 0 $ TypeFunc [TypeLazy $ TypeLazy TypeNone] TypeNone) $
          StmtVoid $
            ExprForce (Just 0) $
              ExprVar "x",
        Left $ ErrorRewriteExprForce $ ExprForce Nothing $ ExprForce Nothing $ ExprVar "x"
      )
    ]

{- % -}

lazify :: Stmt -> Either Error Stmt
lazify stmt = do
  lazyStmt <- evalStateT (stmtToLazy stmt) 0
  typeChecker <- execStateT (stmtToType lazyStmt) newTypeChecker
  types <- execStateT (unify $ typeCheckerPairs typeChecker) mempty
  rewriteStmt
    (M.fromList $ evalState (mapM (mapM deref) $ M.toList $ typeCheckerForces typeChecker) types)
    lazyStmt

main :: IO ()
main = do
  _ <-
    runTestTT $
      TestList $
        concat
          [testExprToLazy, testStmtToLazy, testUnify, testStmtToType, testRewriteStmt]
  either print print $
    lazify $
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
          StmtDecl "x" $ ExprCall (ExprVar "lazy_add_0") [ExprInt (-2)],
          StmtDecl "y" $ ExprCall (ExprVar "lazy_add_1") [ExprVar "x"],
          StmtVoid $ ExprCall (ExprVar "print") [ExprInt 1],
          StmtVoid $ ExprCall (ExprVar "print") [ExprVar "x"],
          StmtVoid $ ExprCall (ExprVar "print") [ExprVar "y"],
          StmtVoid $ ExprCall (ExprVar "print") [ExprCall (ExprVar "lazy_add_0") [ExprInt 3]],
          StmtVoid $ ExprCall (ExprVar "print") [ExprCall (ExprVar "lazy_add_1") [ExprVar "x"]],
          StmtVoid $ ExprCall (ExprVar "print") [ExprCall (ExprVar "add_0") $ map ExprInt [4, 5]]
        ]
