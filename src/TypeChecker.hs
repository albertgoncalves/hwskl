import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.List (foldl', sortOn)
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
  show (TypeVar var) = var
  show TypeStr = "Str"
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
          _ <- munch (/= '\n') *> char '\n'
          space
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

exprCollect :: Int -> Expr -> (Int, [(Expr, Type)], Type)
exprCollect k (ExprInt _) = (k, [], TypeInt)
exprCollect k (ExprStr _) = (k, [], TypeStr)
exprCollect k (ExprVar var) = (k, [], TypeVar var)
exprCollect _ ExprFunc {} = undefined
exprCollect k0 (ExprCall call args) =
  case (call, callType) of
    (ExprVar var0, TypeVar var1)
      | var0 == var1 -> (k3, infers3, returnType)
    _ -> (k3, (call, callType) : infers3, returnType)
  where
    (k1, infers1, callType) = exprCollect k0 call
    (k2, infers2, argTypes) = exprCollects k1 args
    returnType = TypeVar $ printf "_%d_" k2
    k3 = succ k2
    infers3 = (call, TypeFunc argTypes returnType) : infers1 ++ infers2

exprCollects :: Int -> [Expr] -> (Int, [(Expr, Type)], [Type])
exprCollects = foldl' f . (,[],[])
  where
    f :: (Int, [(Expr, Type)], [Type]) -> Expr -> (Int, [(Expr, Type)], [Type])
    f (k0, infers0, types0) expr = (k1, infers0 ++ infers1, types0 ++ [type1])
      where
        (k1, infers1, type1) = exprCollect k0 expr

stmtCollect :: Int -> Stmt -> (Int, [(Expr, Type)])
stmtCollect k0 (StmtBind var expr) = (k1, (ExprVar var, bindType) : infers)
  where
    (k1, infers, bindType) = exprCollect k0 expr
stmtCollect k0 (StmtEffect expr) = (k1, infers)
  where
    (k1, infers, _) = exprCollect k0 expr

stmtCollects :: Int -> [Stmt] -> (Int, [(Expr, Type)])
stmtCollects = foldl' f . (,[])
  where
    f :: (Int, [(Expr, Type)]) -> Stmt -> (Int, [(Expr, Type)])
    f (k, infers) stmt = (infers ++) <$> stmtCollect k stmt

combineType :: M.Map String Type -> Type -> Type -> M.Map String Type
combineType bindings type0 type1
  | type0 == type1 = bindings
combineType bindings (TypeVar var) type0 =
  case M.lookup var bindings of
    Just type1 -> combineType bindings type0 type1
    Nothing -> M.insert var type0 bindings
combineType bindings type0 (TypeVar var) =
  case M.lookup var bindings of
    Just type1 -> combineType bindings type0 type1
    Nothing -> M.insert var type0 bindings
combineType bindings (TypeFunc args0 return0) (TypeFunc args1 return1) =
  combineType (combineTypes bindings args0 args1) return0 return1
combineType _ _ _ = undefined

combineTypes :: M.Map String Type -> [Type] -> [Type] -> M.Map String Type
combineTypes _ [] (_ : _) = undefined
combineTypes _ (_ : _) [] = undefined
combineTypes bindings [] [] = bindings
combineTypes bindings (type0 : types0) (type1 : types1) =
  combineTypes (combineType bindings type0 type1) types0 types1

intoType :: M.Map String Type -> Expr -> (M.Map String Type, Type)
intoType _ (ExprInt _) = undefined
intoType _ (ExprStr _) = undefined
intoType bindings (ExprVar var) =
  case M.lookup var bindings of
    Just varType -> (bindings, varType)
    Nothing -> (bindings, TypeVar var)
intoType _ (ExprFunc {}) = undefined
intoType bindings0 (ExprCall call args) =
  case callType of
    TypeFunc argTypes returnType ->
      let bindings2 = combineExprs bindings1 args argTypes
       in (bindings2, returnType)
    _ -> undefined
  where
    (bindings1, callType) = intoType bindings0 call

combineExpr :: M.Map String Type -> Expr -> Type -> M.Map String Type
combineExpr bindings0 expr = combineType bindings1 exprType
  where
    (bindings1, exprType) = intoType bindings0 expr

combineExprs :: M.Map String Type -> [Expr] -> [Type] -> M.Map String Type
combineExprs _ [] (_ : _) = undefined
combineExprs _ (_ : _) [] = undefined
combineExprs bindings [] [] = bindings
combineExprs bindings (expr0 : exprs0) (type0 : types0) =
  combineExprs (combineExpr bindings expr0 type0) exprs0 types0

reduce :: M.Map String Type -> Type -> Type
reduce _ TypeInt = TypeInt
reduce _ TypeStr = TypeStr
reduce bindings type0@(TypeVar var) =
  case M.lookup var bindings of
    Just type1 -> reduce (M.delete var bindings) type1
    Nothing -> type0
reduce bindings (TypeFunc argTypes returnType) =
  TypeFunc (map (reduce bindings) argTypes) $ reduce bindings returnType

main :: IO ()
main =
  ( putStrLn
      . unlines
      . map show
      . M.toList
      . ( \bindings ->
            M.fromList
              $ map
                ( \(var, type0) ->
                    (var, reduce (M.delete var bindings) type0)
                )
              $ M.toList bindings
        )
      . ( \pairs ->
            let (exprs, types) = unzip pairs
             in combineExprs M.empty exprs types
        )
      . sortOn fst
      . snd
      . stmtCollects 0
      . fst
      . head
      . readP_to_S parse
  )
    "  a = 0\
    \  b = \"a\"\
    \  c = (+ a b)\
    \  a = ((f1 c) (+ (f0 a) (+ 2 b)) a)"
