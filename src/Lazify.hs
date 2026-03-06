import Control.Applicative ((<|>))
import Control.Monad ((<=<))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy
  ( State,
    StateT,
    evalStateT,
    execState,
    execStateT,
    get,
    gets,
    modify,
    put,
  )
import Data.List (intercalate, sort)
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

data TypeCheckerError
  = TypeCheckerErrorExpr Expr
  | TypeCheckerErrorStmt Stmt
  | TypeCheckerErrorUnify1 Type Type
  deriving (Show)

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

exprToLazy :: Expr -> StateT Int Maybe Expr
exprToLazy expr@(ExprInt {}) = return expr
exprToLazy expr@(ExprVar {}) = return expr
exprToLazy (ExprCall func@(ExprVar var) args)
  | var `M.member` intrinsics = ExprCall func <$> mapM (exprToForce <=< exprToLazy) args
exprToLazy (ExprCall func args) = ExprLazy <$> exprToLazy func <*> mapM exprToLazy args
exprToLazy (ExprLazy {}) = lift Nothing
exprToLazy (ExprForce {}) = lift Nothing

exprToForce :: Expr -> StateT Int Maybe Expr
exprToForce expr@(ExprInt {}) = return expr
exprToForce expr@(ExprVar {}) = (`ExprForce` expr) <$> nextK
exprToForce expr@(ExprCall {}) = (`ExprForce` expr) <$> nextK
exprToForce (ExprLazy func args) = return $ ExprCall func args
exprToForce (ExprForce {}) = lift Nothing

testExprToLazy :: [Test]
testExprToLazy =
  [ evalStateT (exprToLazy $ ExprCall (ExprVar "f") $ map ExprVar ["x", "y"]) 0
      ~?= Just (ExprLazy (ExprVar "f") $ map ExprVar ["x", "y"]),
    evalStateT
      ( exprToLazy $
          ExprCall (ExprVar "f") [ExprVar "x", ExprCall (ExprVar "f") $ map ExprVar ["y", "z"]]
      )
      0
      ~?= Just
        (ExprLazy (ExprVar "f") $ ExprVar "x" : [ExprLazy (ExprVar "f") $ map ExprVar ["y", "z"]]),
    evalStateT (exprToLazy $ ExprCall (ExprVar "print") [ExprInt 1]) 0
      ~?= Just (ExprCall (ExprVar "print") [ExprInt 1]),
    evalStateT (exprToLazy $ ExprCall (ExprVar "print") [ExprVar "x"]) 0
      ~?= Just (ExprCall (ExprVar "print") [ExprForce 0 $ ExprVar "x"]),
    evalStateT (exprToLazy $ ExprCall (ExprVar "print") [ExprCall (ExprVar "f") []]) 0
      ~?= Just (ExprCall (ExprVar "print") [ExprCall (ExprVar "f") []])
  ]

{- % -}

stmtToLazy :: Stmt -> StateT Int Maybe Stmt
stmtToLazy (StmtDecl var expr) = StmtDecl var <$> exprToLazy expr
stmtToLazy (StmtFunc var args stmts) = StmtFunc var args <$> mapM stmtToLazy stmts
stmtToLazy (StmtVoid expr) = StmtVoid <$> exprToLazy expr
stmtToLazy (StmtReturn (Just expr)) = StmtReturn . Just <$> exprToLazy expr
stmtToLazy (StmtReturn Nothing) = return $ StmtReturn Nothing

testStmtToLazy :: [Test]
testStmtToLazy =
  [ evalStateT (stmtToLazy (StmtDecl "z" $ ExprCall (ExprVar "f") $ map ExprVar ["x", "y"])) 0
      ~?= Just (StmtDecl "z" $ ExprLazy (ExprVar "f") $ map ExprVar ["x", "y"])
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

exprToType :: Expr -> StateT TypeChecker (Either TypeCheckerError) Type
exprToType (ExprInt _) = return TypeInt
exprToType expr@(ExprVar var) = do
  bindings <- gets getTypeCheckerBindings
  funcStack <- gets getTypeCheckerFuncStack
  maybe (lift $ Left $ TypeCheckerErrorExpr expr) return $ loop bindings var funcStack
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

stmtToType :: Stmt -> StateT TypeChecker (Either TypeCheckerError) ()
stmtToType stmt@(StmtDecl var expr) = do
  exprType <- exprToType expr
  bindings <- gets getTypeCheckerBindings
  funcStack <- gets getTypeCheckerFuncStack
  _ <- case M.lookup (var : funcStack) bindings of
    Just _ -> lift $ Left $ TypeCheckerErrorStmt stmt
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

unify :: [(Type, Type)] -> StateT [(Int, Type)] (Either TypeCheckerError) ()
unify [] = return ()
unify pairs = mapM (uncurry unify1) pairs >>= unify . concat

unify1 :: Type -> Type -> StateT [(Int, Type)] (Either TypeCheckerError) [(Type, Type)]
unify1 left@(TypeFunc leftArgs leftReturn) right@(TypeFunc rightArgs rightReturn)
  | length leftArgs /= length rightArgs = lift $ Left $ TypeCheckerErrorUnify1 left right
  | otherwise = return $ zip (leftReturn : leftArgs) (rightReturn : rightArgs)
unify1 left right
  | left == right = return []
unify1 left@(TypeK leftK) right@(TypeK rightK) = do
  modify ([(leftK, right), (rightK, left)] ++)
  return []
unify1 (TypeK k) right = do
  modify ((k, right) :)
  return []
unify1 left (TypeK k) = do
  modify ((k, left) :)
  return []
unify1 left right = lift $ Left $ TypeCheckerErrorUnify1 left right

deref1 :: Int -> State (M.Map Int Type) (Maybe Type)
deref1 parentK = do
  types <- get
  case M.lookup parentK types of
    Just parentType@(TypeK childK) -> do
      put $ M.delete parentK types
      someType <- fromMaybe parentType <$> deref1 childK
      modify $ M.insert parentK someType
      return $ Just someType
    Just parentType -> return $ Just parentType
    Nothing -> return Nothing

deref :: M.Map Int Type -> M.Map Int Type
deref types = execState (mapM deref1 $ M.keys types) types

{- % -}

main :: IO ()
main = do
  _ <- runTestTT $ TestList $ testExprToLazy ++ testStmtToLazy
  case evalStateT (stmtToLazy stmt) 0 of
    Just lazyStmt -> do
      print lazyStmt
      either
        print
        ( \typeChecker -> do
            mapM_ print $ M.toList $ getTypeCheckerBindings typeChecker
            putChar '\n'
            mapM_ print $ M.toList $ getTypeCheckerForces typeChecker
            putChar '\n'
            either print (mapM_ print . sort) $
              execStateT (unify $ getTypeCheckerPairs typeChecker) []
        )
        $ execStateT (stmtToType lazyStmt)
        $ TypeChecker 0 (M.mapKeys (: []) intrinsics) mempty [] [] []
    Nothing -> return ()
  where
    stmt =
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
