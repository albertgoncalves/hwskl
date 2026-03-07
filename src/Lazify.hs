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
  | ExprForce Int Expr
  deriving (Eq)

instance Show Expr where
  show (ExprInt int) = show int
  show (ExprVar var) = var
  show (ExprCall (ExprVar "+") [left, right]) = "(" ++ show left ++ " + " ++ show right ++ ")"
  show (ExprCall func args) = show func ++ "(" ++ intercalate ", " (map show args) ++ ")"
  show (ExprLazy func args) = "Lazy(" ++ intercalate ", " (map show $ func : args) ++ ")"
  show (ExprForce k arg) = "__force_" ++ show k ++ "__(" ++ show arg ++ ")"

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

{- % -}

data TypeChecker = TypeChecker
  { getTypeCheckerK :: Int,
    getTypeCheckerBindings :: M.Map [String] Type,
    getTypeCheckerForces :: M.Map Int Type,
    getTypeCheckerFuncStack :: [String],
    getTypeCheckerReturnTypes :: [Type],
    getTypeCheckerPairs :: [(Type, Type)]
  }
  deriving (Show)

data Error
  = ErrorExpr Expr
  | ErrorStmt Stmt
  | ErrorUnify1 Type Type
  deriving (Eq, Show)

{- % -}

intrinsics :: M.Map String Type
intrinsics =
  M.fromList [("print", TypeFunc [TypeInt] TypeNone), ("+", TypeFunc [TypeInt, TypeInt] TypeInt)]

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
exprToLazy expr@(ExprLazy {}) = lift $ Left $ ErrorExpr expr
exprToLazy expr@(ExprForce {}) = lift $ Left $ ErrorExpr expr

exprToForce :: Expr -> StateT Int (Either Error) Expr
exprToForce expr@(ExprInt {}) = return expr
exprToForce expr@(ExprVar {}) = (`ExprForce` expr) <$> nextK
exprToForce expr@(ExprCall {}) = (`ExprForce` expr) <$> nextK
exprToForce expr@(ExprLazy {}) = (`ExprForce` expr) <$> nextK
exprToForce expr@(ExprForce {}) = lift $ Left $ ErrorExpr expr

testExprToLazy :: [Test]
testExprToLazy =
  [ evalStateT (exprToLazy $ ExprCall (ExprVar "f") $ map ExprVar ["x", "y"]) 0
      ~?= Right (ExprLazy (ExprVar "f") $ map ExprVar ["x", "y"]),
    evalStateT
      ( exprToLazy $
          ExprCall (ExprVar "f") [ExprVar "x", ExprCall (ExprVar "f") $ map ExprVar ["y", "z"]]
      )
      0
      ~?= Right
        (ExprLazy (ExprVar "f") $ ExprVar "x" : [ExprLazy (ExprVar "f") $ map ExprVar ["y", "z"]]),
    evalStateT (exprToLazy $ ExprCall (ExprVar "print") [ExprInt 1]) 0
      ~?= Right (ExprCall (ExprVar "print") [ExprInt 1]),
    evalStateT (exprToLazy $ ExprCall (ExprVar "print") [ExprVar "x"]) 0
      ~?= Right (ExprCall (ExprVar "print") [ExprForce 0 $ ExprVar "x"]),
    evalStateT (exprToLazy $ ExprCall (ExprVar "print") [ExprCall (ExprVar "f") []]) 0
      ~?= Right (ExprCall (ExprVar "print") [ExprForce 0 $ ExprLazy (ExprVar "f") []])
  ]

{- % -}

stmtToLazy :: Stmt -> StateT Int (Either Error) Stmt
stmtToLazy = mapStmt exprToLazy

testStmtToLazy :: [Test]
testStmtToLazy =
  [ evalStateT (stmtToLazy (StmtDecl "z" $ ExprCall (ExprVar "f") $ map ExprVar ["x", "y"])) 0
      ~?= Right (StmtDecl "z" $ ExprLazy (ExprVar "f") $ map ExprVar ["x", "y"])
  ]

{- % -}

nextTypeK :: (Monad m) => StateT TypeChecker m Type
nextTypeK = do
  typeChecker <- get
  let k = getTypeCheckerK typeChecker
  let typeK = TypeK k
  modify $ \typeChecker -> typeChecker {getTypeCheckerK = k + 1}
  return typeK

pushBinding :: (Monad m) => String -> Type -> StateT TypeChecker m ()
pushBinding var varType = do
  modify $ \typeChecker ->
    typeChecker
      { getTypeCheckerBindings =
          M.insert (var : getTypeCheckerFuncStack typeChecker) varType $
            getTypeCheckerBindings typeChecker
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
  bindings <- gets getTypeCheckerBindings
  funcStack <- gets getTypeCheckerFuncStack
  maybe (lift $ Left $ ErrorExpr expr) return $ loop bindings var funcStack
  where
    loop :: M.Map [String] Type -> String -> [String] -> Maybe Type
    loop bindings var [] = M.lookup [var] bindings
    loop bindings var funcStack@(_ : rest) =
      M.lookup (var : funcStack) bindings <|> loop bindings var rest
exprToType (ExprCall func args) = do
  funcType <- exprToType func
  argTypes <- mapM exprToType args
  returnType <- nextTypeK
  modify $ \typeChecker ->
    typeChecker
      { getTypeCheckerPairs =
          (funcType, TypeFunc argTypes returnType) : getTypeCheckerPairs typeChecker
      }
  return returnType
exprToType (ExprLazy func args) = TypeLazy <$> exprToType (ExprCall func args)
exprToType (ExprForce k arg) = do
  argType <- exprToType arg
  returnType <- nextTypeK
  modify $
    \typeChecker ->
      typeChecker
        { getTypeCheckerForces =
            M.insert k (TypeFunc [argType] returnType) $ getTypeCheckerForces typeChecker
        }
  return returnType

{- % -}

stmtToType :: Stmt -> StateT TypeChecker (Either Error) ()
stmtToType stmt@(StmtDecl var expr) = do
  exprType <- exprToType expr
  bindings <- gets getTypeCheckerBindings
  funcStack <- gets getTypeCheckerFuncStack
  _ <- case M.lookup (var : funcStack) bindings of
    Just _ -> lift $ Left $ ErrorStmt stmt
    Nothing -> return ()
  pushBinding var exprType
stmtToType (StmtFunc var args stmts) = do
  parentFuncStack <- gets getTypeCheckerFuncStack
  parentReturnTypes <- gets getTypeCheckerReturnTypes

  modify $ \typeChecker ->
    typeChecker
      { getTypeCheckerFuncStack = var : parentFuncStack,
        getTypeCheckerReturnTypes = []
      }

  argTypes <- mapM argToType args
  returnType <- nextTypeK

  mapM_ stmtToType stmts

  modify $ \typeChecker ->
    typeChecker
      { getTypeCheckerFuncStack = parentFuncStack,
        getTypeCheckerReturnTypes = parentReturnTypes,
        getTypeCheckerPairs =
          map (returnType,) (getTypeCheckerReturnTypes typeChecker)
            ++ getTypeCheckerPairs typeChecker
      }
  pushBinding var (TypeFunc argTypes returnType)
stmtToType (StmtVoid expr) = do
  exprType <- exprToType expr
  modify $ \typeChecker ->
    typeChecker {getTypeCheckerPairs = (exprType, TypeNone) : getTypeCheckerPairs typeChecker}
stmtToType (StmtReturn (Just expr)) = do
  exprType <- exprToType expr
  modify $ \typeChecker ->
    typeChecker {getTypeCheckerReturnTypes = exprType : getTypeCheckerReturnTypes typeChecker}
stmtToType (StmtReturn Nothing) = do
  modify $ \typeChecker ->
    typeChecker {getTypeCheckerReturnTypes = TypeNone : getTypeCheckerReturnTypes typeChecker}

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

derefK :: Int -> State (M.Map Int Type) (Maybe Type)
derefK parentK = do
  types <- get
  case M.lookup parentK types of
    Just parentType@(TypeK childK) -> do
      put $ M.delete parentK types
      someType <- fromMaybe parentType <$> derefK childK
      modify $ M.insert parentK someType
      return $ Just someType
    Just (TypeLazy (TypeK childK)) -> do
      maybeType <- derefK childK
      case maybeType of
        Just someType -> do
          let parentType = TypeLazy someType
          modify $ M.insert parentK parentType
          return $ Just parentType
        Nothing -> return Nothing
    Just parentType -> return $ Just parentType
    Nothing -> return Nothing

derefType :: Type -> State (M.Map Int Type) Type
derefType parentType@(TypeK parentK) = do
  maybeType <- derefK parentK
  case maybeType of
    Just childType -> return childType
    Nothing -> return parentType
derefType (TypeFunc argTypes returnType) =
  TypeFunc <$> mapM derefType argTypes <*> derefType returnType
derefType (TypeLazy childType) = TypeLazy <$> derefType childType
derefType parentType = return parentType

{- % -}

rewriteExpr :: M.Map Int Type -> Expr -> Either Error Expr
rewriteExpr forces parentExpr@(ExprForce k childExpr) =
  case M.lookup k forces of
    Just (TypeFunc [argType] _) -> rewriteExprForce argType childExpr
    _ -> Left $ ErrorExpr parentExpr
rewriteExpr forces (ExprCall func args) =
  ExprCall <$> rewriteExpr forces func <*> mapM (rewriteExpr forces) args
rewriteExpr _ expr = Right expr

rewriteExprForce :: Type -> Expr -> Either Error Expr
rewriteExprForce (TypeLazy childType) expr =
  rewriteExprForce childType $ ExprCall (ExprVar "__force__") [expr]
rewriteExprForce _ expr = Right expr

rewriteStmt :: M.Map Int Type -> Stmt -> Either Error Stmt
rewriteStmt = mapStmt . rewriteExpr

{- % -}

lazify :: Stmt -> Either Error Stmt
lazify stmt = do
  lazyStmt <- evalStateT (stmtToLazy stmt) 0
  typeChecker <-
    execStateT (stmtToType lazyStmt) $
      TypeChecker 0 (M.mapKeys (: []) intrinsics) mempty [] [] []
  types <- execStateT (unify $ getTypeCheckerPairs typeChecker) mempty
  rewriteStmt
    ( M.fromList $
        evalState (mapM (mapM derefType) $ M.toList $ getTypeCheckerForces typeChecker) types
    )
    lazyStmt

main :: IO ()
main = do
  _ <- runTestTT $ TestList $ testExprToLazy ++ testStmtToLazy
  either print print $
    lazify $
      StmtFunc
        "main"
        []
        [ StmtFunc
            "add"
            ["x", "y"]
            [StmtReturn $ Just $ ExprCall (ExprVar "+") $ map ExprVar ["x", "y"]],
          StmtFunc
            "f"
            ["x"]
            [StmtReturn $ Just $ ExprCall (ExprVar "add") [ExprVar "x", ExprInt 1]],
          StmtDecl "x" $ ExprCall (ExprVar "f") [ExprInt (-2)],
          StmtVoid $ ExprCall (ExprVar "print") [ExprInt 1],
          StmtVoid $ ExprCall (ExprVar "print") [ExprVar "x"],
          StmtVoid $ ExprCall (ExprVar "print") [ExprCall (ExprVar "f") [ExprInt 3]],
          StmtVoid $ ExprCall (ExprVar "print") [ExprCall (ExprVar "add") $ map ExprInt [4, 5]]
        ]
