import Control.Monad (void, when)
import Control.Monad.State (State, execState, modify, state)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import qualified Data.Map as M
import Text.ParserCombinators.ReadP
  ( ReadP,
    char,
    eof,
    get,
    look,
    munch,
    munch1,
    pfail,
    readP_to_S,
    satisfy,
    string,
    (<++),
  )
import Text.Printf (printf)

data Expr
  = ExprInt Int
  | ExprStr String
  | ExprVar String
  | ExprFunc [String] [Stmt] Expr
  | ExprCall Expr [Expr]
  deriving (Eq, Ord)

data Stmt
  = StmtBind String Expr
  | StmtEffect Expr
  deriving (Eq, Ord)

data Type
  = TypeInt
  | TypeStr
  | TypeVar String
  | TypeFunc [Type] Type
  deriving (Eq, Ord)

data Compiler = Compiler
  { compilerK :: Int,
    compilerBindings :: M.Map String Type,
    compilerScopes :: [[String]],
    compilerCallVars :: M.Map String String
  }

showExpr :: Int -> Expr -> String
showExpr _ (ExprInt int) = show int
showExpr _ (ExprStr str) = show str
showExpr _ (ExprVar var) = var
showExpr n (ExprFunc args stmts expr) =
  printf "\\%s %s" (unwords args) $ showScope n stmts expr
showExpr n (ExprCall call args) =
  printf "%s(%s)" (showExpr n call) $ unwords $ map (showExpr n) args

indent :: Int -> String
indent n = replicate (n * 4) ' '

showScope :: Int -> [Stmt] -> Expr -> String
showScope n0 body returnExpr =
  printf
    "{\n%s%s}"
    ( unlines $
        map (indent n1 ++) $
          map (showStmt n1) body ++ [showExpr n1 returnExpr]
    )
    (indent n0)
  where
    n1 = n0 + 1

showStmt :: Int -> Stmt -> String
showStmt n (StmtBind var expr) = printf "%s = %s" var $ showExpr n expr
showStmt n (StmtEffect effectExpr) = showExpr n effectExpr

instance Show Stmt where
  show = showStmt 0

instance Show Expr where
  show = showExpr 0

instance Show Type where
  show TypeInt = "Int"
  show TypeStr = "Str"
  show (TypeVar var) = var
  show (TypeFunc args returnType) =
    printf "(\\%s -> %s)" (unwords $ map show args) $ show returnType

parse :: ReadP [Stmt]
parse = many1 stmt <* token eof
  where
    many :: ReadP a -> ReadP [a]
    many p = many1 p <++ return []

    many1 :: ReadP a -> ReadP [a]
    many1 p = do
      x <- p
      xs <- many p
      return $ x : xs

    choice :: [ReadP a] -> ReadP a
    choice [] = pfail
    choice [p] = p
    choice (p : ps) = p <++ choice ps

    dropThru :: String -> ReadP ()
    dropThru [] = return ()
    dropThru s@(c : cs) = do
      match <- (c ==) <$> get
      if match
        then void (string cs) <++ dropThru s
        else dropThru s

    space :: ReadP ()
    space = do
      rest <- look
      case rest of
        ('/' : '*' : _) -> do
          _ <- get
          _ <- get
          dropThru "*/"
          space
        ('#' : _) -> do
          _ <- get
          munch (/= '\n') *> ((char '\n' *> space) <++ eof)
        (c : _) | isSpace c -> do
          _ <- get
          space
        _ -> return ()

    token :: ReadP a -> ReadP a
    token = (space *>)

    str :: ReadP String
    str = token $ do
      _ <- char '"'
      s <-
        many $
          choice
            [ '\n' <$ string "\\n",
              '\\' <$ string "\\\\",
              '"' <$ string "\\\"",
              satisfy (`notElem` "\"\\")
            ]
      _ <- char '"'
      return s

    int :: ReadP Int
    int = token $ read <$> munch1 isDigit

    ident :: ReadP String
    ident =
      token $
        ((:) <$> satisfy isAlpha <*> munch isAlphaNum)
          <++ ((: []) <$> satisfy (`elem` "+-*/%"))

    call :: ReadP Expr
    call =
      ExprCall
        <$> (token (char '(') *> expr)
        <*> (many expr <* token (char ')'))

    func :: ReadP Expr
    func = do
      _ <- token $ char '\\'
      args <- many ident
      _ <- token $ char '{'
      stmts <- many stmt
      _ <- token $ char '}'
      case reverse stmts of
        (StmtEffect returnExpr : body) ->
          return $ ExprFunc args (reverse body) returnExpr
        _ -> undefined

    expr :: ReadP Expr
    expr =
      choice
        [ func,
          call,
          ExprStr <$> str,
          ExprInt <$> int,
          ExprVar <$> ident
        ]

    bind :: ReadP Stmt
    bind = StmtBind <$> ident <*> (token (char '=') *> expr)

    stmt :: ReadP Stmt
    stmt = bind <++ (StmtEffect <$> expr)

getBindings :: State Compiler (M.Map String Type)
getBindings = state $ \c -> (compilerBindings c, c)

getK :: State Compiler Int
getK = state $ \c -> (compilerK c, c)

setK :: Int -> State Compiler ()
setK k = modify $ \c -> c {compilerK = k}

nextK :: State Compiler Int
nextK = do
  k <- getK
  setK (succ k)
  return k

setBindings :: (M.Map String Type -> M.Map String Type) -> State Compiler ()
setBindings f = modify $ \c -> c {compilerBindings = f $ compilerBindings c}

newScope :: State Compiler ()
newScope = modify $ \c -> c {compilerScopes = [] : compilerScopes c}

pushScopeVar :: String -> State Compiler ()
pushScopeVar var = modify $ \c ->
  case compilerScopes c of
    (scope : scopes) -> c {compilerScopes = (var : scope) : scopes}
    _ -> undefined

popScope :: State Compiler [String]
popScope =
  state $
    \c ->
      (head $ compilerScopes c, c {compilerScopes = tail $ compilerScopes c})

getCallVars :: State Compiler (M.Map String String)
getCallVars = state $ \c -> (compilerCallVars c, c)

setCallVars ::
  (M.Map String String -> M.Map String String) -> State Compiler ()
setCallVars f = modify $ \c -> c {compilerCallVars = f $ compilerCallVars c}

intoCallVar :: Type -> State Compiler Type
intoCallVar (TypeVar var0) = do
  callVars <- getCallVars
  case M.lookup var0 callVars of
    Nothing -> do
      var1 <- varLabel <$> nextK
      setCallVars $ M.insert var0 var1
      return $ TypeVar var1
    Just var1 -> return $ TypeVar var1
intoCallVar (TypeFunc argTypes0 returnType0) = do
  argTypes1 <- intoCallVars argTypes0
  returnType1 <- intoCallVar returnType0
  return $ TypeFunc argTypes1 returnType1
intoCallVar otherType = return otherType

intoCallVars :: [Type] -> State Compiler [Type]
intoCallVars = mapM intoCallVar

varLabel :: Int -> String
varLabel = printf "_%d_"

deref :: String -> State Compiler (Maybe Type)
deref var0 = do
  bindings <- getBindings
  case M.lookup var0 bindings of
    Just type0@(TypeVar var1) -> do
      setBindings $ M.delete var0
      maybeType <- deref var1
      case maybeType of
        Just type1 -> do
          setBindings $ M.insert var0 type1
          return $ Just type1
        Nothing -> do
          setBindings $ M.insert var0 type0
          return $ Just type0
    maybeType -> return maybeType

intoType :: Expr -> State Compiler Type
intoType (ExprInt _) = return TypeInt
intoType (ExprStr _) = return TypeStr
intoType (ExprVar var) = do
  maybeType <- deref var
  case maybeType of
    Just varType -> return varType
    Nothing -> do
      varType <- TypeVar . varLabel <$> nextK
      setBindings $ M.insert var varType
      return varType
intoType (ExprFunc args stmts expr) = do
  bindings <- getBindings
  when (any (`M.member` bindings) args) undefined
  k <- getK
  setK (k + length args)
  setBindings
    (`M.union` M.fromList (zip args $ map (TypeVar . varLabel) [k ..]))
  newScope
  stmtChecks stmts
  returnType0 <- intoType expr
  argTypes <- shrinkVars args
  returnType1 <- shrinkType returnType0
  shrinkBindings
  vars <- popScope
  setBindings $ flip (foldr M.delete) $ args ++ vars
  return $ TypeFunc argTypes returnType1
intoType (ExprCall call args) = do
  callType <- intoType call
  case callType of
    TypeFunc argTypes0 returnType0 -> do
      setCallVars $ const M.empty
      argTypes1 <- intoCallVars argTypes0
      returnType1 <- intoCallVar returnType0
      exprCombines args argTypes1
      return returnType1
    TypeVar _ -> do
      argTypes <- mapM intoType args
      returnType <- TypeVar . varLabel <$> nextK
      typeCombine callType $ TypeFunc argTypes returnType
      return returnType
    _ -> undefined

stmtCheck :: Stmt -> State Compiler ()
stmtCheck (StmtBind var expr) = do
  bindings <- getBindings
  case M.lookup var bindings of
    Just exprType0 -> do
      exprType1 <- intoType expr
      typeCombine exprType0 exprType1
    Nothing -> do
      exprType1 <- intoType expr
      setBindings $ M.insert var exprType1
      pushScopeVar var
stmtCheck (StmtEffect expr) = void $ intoType expr

stmtChecks :: [Stmt] -> State Compiler ()
stmtChecks = mapM_ stmtCheck

typeCombine :: Type -> Type -> State Compiler ()
typeCombine type0 type1
  | type0 == type1 = return ()
typeCombine (TypeVar var) type0 = do
  maybeType <- deref var
  case maybeType of
    Just type1 -> do
      setBindings $ M.delete var
      typeCombine type0 type1
      setBindings $ M.insert var type1
    Nothing -> do
      setBindings $ M.insert var type0
typeCombine type0 (TypeVar var) = do
  maybeType <- deref var
  case maybeType of
    Just type1 -> do
      setBindings $ M.delete var
      typeCombine type0 type1
      setBindings $ M.insert var type1
    Nothing -> do
      setBindings $ M.insert var type0
typeCombine (TypeFunc args0 return0) (TypeFunc args1 return1) = do
  typeCombines args0 args1
  typeCombine return0 return1
typeCombine _ _ = undefined

typeCombines :: [Type] -> [Type] -> State Compiler ()
typeCombines [] [] = return ()
typeCombines [] _ = undefined
typeCombines _ [] = undefined
typeCombines (type0 : types0) (type1 : types1) = do
  typeCombine type0 type1
  typeCombines types0 types1

exprCombine :: Expr -> Type -> State Compiler ()
exprCombine expr = (intoType expr >>=) . typeCombine

exprCombines :: [Expr] -> [Type] -> State Compiler ()
exprCombines [] [] = return ()
exprCombines [] _ = undefined
exprCombines _ [] = undefined
exprCombines (expr0 : exprs0) (type1 : types1) = do
  exprCombine expr0 type1
  exprCombines exprs0 types1

shrinkVar :: String -> State Compiler Type
shrinkVar var = do
  maybeType <- deref var
  case maybeType of
    Just (TypeFunc argTypes0 returnType0) -> do
      setBindings $ M.delete var
      argTypes1 <- shrinkTypes argTypes0
      returnType1 <- shrinkType returnType0
      let funcType = TypeFunc argTypes1 returnType1
      setBindings $ M.insert var funcType
      return funcType
    Just varType -> return varType
    Nothing -> return $ TypeVar var

shrinkVars :: [String] -> State Compiler [Type]
shrinkVars = mapM shrinkVar

shrinkType :: Type -> State Compiler Type
shrinkType TypeInt = return TypeInt
shrinkType TypeStr = return TypeStr
shrinkType (TypeVar var) = shrinkVar var
shrinkType (TypeFunc argTypes0 returnType0) = do
  argTypes1 <- shrinkTypes argTypes0
  returnType1 <- shrinkType returnType0
  return $ TypeFunc argTypes1 returnType1

shrinkTypes :: [Type] -> State Compiler [Type]
shrinkTypes = mapM shrinkType

shrinkBindings :: State Compiler ()
shrinkBindings = do
  bindings <- getBindings
  _ <- shrinkVars $ M.keys bindings
  return ()

main :: IO ()
main =
  mapM_
    ( putStrLn
        . unlines
        . map show
        . M.toList
        . compilerBindings
        . ( \stmts ->
              execState (stmtChecks stmts >> shrinkBindings) $
                Compiler 0 M.empty [[]] M.empty
          )
        . fst
        . head
        . readP_to_S parse
    )
    [ "  fib = \\ {\
      \      a = 0\
      \      b = 1\
      \      \\ {\
      \          \\ {\
      \              c = b\
      \              b = (+ a c)\
      \              a = c\
      \              c\
      \          }\
      \      }\
      \  }\
      \  \
      \  f = (fib)\
      \  ((f))\
      \  ((f))\
      \  ((f))\
      \  ((f))\
      \  ((f))\
      \  (printf \"%ld\n\" ((f)))",
      "  a = 0\
      \  b = \"a\"\
      \  c = (\\a0 b0 { (+ a0 b0) } a b)\
      \  d = ((f1 c d) (+ (f0 a) (+ 2 b)) a)",
      "  f = \\x y { (+ x y) }\
      \  c = (f a b)\
      \  e = (f c d)",
      "  f = \\x { x }\n\
      \  x = 0\n\
      \  y = (f x)\n\
      \  z = (f \"123\")"
    ]
