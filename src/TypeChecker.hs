import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.List (foldl')
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

deref :: M.Map String Type -> String -> (M.Map String Type, Maybe Type)
deref bindings0 var0 =
  case M.lookup var0 bindings0 of
    Just type0@(TypeVar var1) ->
      case deref (M.delete var0 bindings0) var1 of
        (bindings1, Just type1) -> (M.insert var0 type1 bindings1, Just type1)
        (bindings1, Nothing) -> (M.insert var0 type0 bindings1, Nothing)
    maybeType -> (bindings0, maybeType)

intoType :: Int -> M.Map String Type -> Expr -> (Int, M.Map String Type, Type)
intoType k bindings (ExprInt _) = (k, bindings, TypeInt)
intoType k bindings (ExprStr _) = (k, bindings, TypeStr)
intoType k bindings0 (ExprVar var0) =
  case deref bindings0 var0 of
    (bindings1, Just varType) -> (k, bindings1, varType)
    (bindings1, Nothing) -> (k, bindings1, TypeVar var0)
intoType _ bindings (ExprFunc args _ _)
  | any (`M.member` bindings) args = undefined
intoType k0 bindings0 (ExprFunc args stmts expr) =
  ( k3,
    M.union (M.intersection (shrinkBindings bindings5) bindings0) bindings0,
    TypeFunc argTypes returnType1
  )
  where
    k1 = k0 + length args
    bindings1 =
      M.union bindings0 $
        M.fromList $
          zip args $
            map (TypeVar . printf "_%d_") [k0 ..]
    (k2, bindings2) = stmtChecks k1 bindings1 stmts
    (k3, bindings3, returnType0) = intoType k2 bindings2 expr
    (bindings4, argTypes) = shrinkVars bindings3 args
    (bindings5, returnType1) = shrinkType bindings4 returnType0
intoType k0 bindings0 (ExprCall call args) =
  case callType of
    TypeFunc argTypes returnType ->
      let (k2, bindings2) = exprCombines k1 bindings1 args argTypes
       in (k2, bindings2, returnType)
    TypeVar _ ->
      let (k2, bindings2, argTypes) = intoTypes k1 bindings1 args
          returnType = TypeVar $ printf "_%d_" k2
          (k3, bindings3) =
            typeCombine (succ k2) bindings2 callType $
              TypeFunc argTypes returnType
       in (k3, bindings3, returnType)
    _ -> undefined
  where
    (k1, bindings1, callType) = intoType k0 bindings0 call

intoTypes ::
  Int -> M.Map String Type -> [Expr] -> (Int, M.Map String Type, [Type])
intoTypes k0 bindings0 =
  foldr
    ( \expr (k1, bindings1, types1) ->
        (: types1) <$> intoType k1 bindings1 expr
    )
    (k0, bindings0, [])

stmtCheck :: Int -> M.Map String Type -> Stmt -> (Int, M.Map String Type)
stmtCheck k0 bindings0 (StmtBind var expr) =
  case M.lookup var bindings0 of
    Just exprType0 -> typeCombine k1 bindings1 exprType0 exprType1
    Nothing -> (k1, M.insert var exprType1 bindings1)
  where
    (k1, bindings1, exprType1) = intoType k0 bindings0 expr
stmtCheck k0 bindings0 (StmtEffect expr) = (k1, bindings1)
  where
    (k1, bindings1, _) = intoType k0 bindings0 expr

stmtChecks :: Int -> M.Map String Type -> [Stmt] -> (Int, M.Map String Type)
stmtChecks k bindings = foldl' (uncurry stmtCheck) (k, bindings)

typeCombine ::
  Int -> M.Map String Type -> Type -> Type -> (Int, M.Map String Type)
typeCombine k bindings type0 type1
  | type0 == type1 = (k, bindings)
typeCombine k0 bindings0 (TypeVar var) type0 =
  case deref bindings0 var of
    (bindings1, Just type1) ->
      M.insert var type1
        <$> typeCombine k0 (M.delete var bindings1) type0 type1
    (bindings1, Nothing) -> (k0, M.insert var type0 bindings1)
typeCombine k0 bindings0 type0 (TypeVar var) =
  case deref bindings0 var of
    (bindings1, Just type1) ->
      M.insert var type1
        <$> typeCombine k0 (M.delete var bindings1) type0 type1
    (bindings1, Nothing) -> (k0, M.insert var type0 bindings1)
typeCombine k0 bindings0 (TypeFunc args0 return0) (TypeFunc args1 return1) =
  typeCombine k1 bindings1 return0 return1
  where
    (k1, bindings1) = typeCombines k0 bindings0 args0 args1
typeCombine _ _ _ _ = undefined

typeCombines ::
  Int -> M.Map String Type -> [Type] -> [Type] -> (Int, M.Map String Type)
typeCombines _ _ types0 types1
  | length types0 /= length types1 = undefined
typeCombines k bindings types0 types1 =
  foldl' (uncurry . uncurry typeCombine) (k, bindings) $ zip types0 types1

exprCombine ::
  Int -> M.Map String Type -> Expr -> Type -> (Int, M.Map String Type)
exprCombine k0 bindings0 expr = typeCombine k1 bindings1 exprType
  where
    (k1, bindings1, exprType) = intoType k0 bindings0 expr

exprCombines ::
  Int -> M.Map String Type -> [Expr] -> [Type] -> (Int, M.Map String Type)
exprCombines _ _ exprs types
  | length exprs /= length types = undefined
exprCombines k bindings exprs types =
  foldl' (uncurry . uncurry exprCombine) (k, bindings) $ zip exprs types

shrinkVar :: M.Map String Type -> String -> (M.Map String Type, Type)
shrinkVar bindings0 var0 =
  case deref bindings0 var0 of
    (bindings1, Just (TypeFunc argTypes0 returnType0)) ->
      let (bindings2, argTypes1) =
            shrinkTypes (M.delete var0 bindings1) argTypes0
          (bindings3, returnType2) = shrinkType bindings2 returnType0
          funcType = TypeFunc argTypes1 returnType2
       in (M.insert var0 funcType bindings3, funcType)
    (bindings1, Just varType) -> (bindings1, varType)
    (bindings1, Nothing) -> (bindings1, TypeVar var0)

shrinkVars :: M.Map String Type -> [String] -> (M.Map String Type, [Type])
shrinkVars =
  foldr
    (\var (bindings0, types0) -> (: types0) <$> shrinkVar bindings0 var)
    . (,[])

shrinkType :: M.Map String Type -> Type -> (M.Map String Type, Type)
shrinkType bindings TypeInt = (bindings, TypeInt)
shrinkType bindings TypeStr = (bindings, TypeStr)
shrinkType bindings (TypeVar var) = shrinkVar bindings var
shrinkType bindings0 (TypeFunc argTypes0 returnType0) =
  TypeFunc argTypes1 <$> shrinkType bindings1 returnType0
  where
    (bindings1, argTypes1) = shrinkTypes bindings0 argTypes0

shrinkTypes :: M.Map String Type -> [Type] -> (M.Map String Type, [Type])
shrinkTypes =
  foldr
    (\type0 (bindings0, types0) -> (: types0) <$> shrinkType bindings0 type0)
    . (,[])

shrinkBindings :: M.Map String Type -> M.Map String Type
shrinkBindings bindings =
  foldl' ((fst .) . shrinkVar) bindings $ M.keys bindings

main :: IO ()
main =
  mapM_
    ( putStrLn
        . unlines
        . map show
        . M.toList
        . shrinkBindings
        . snd
        . stmtChecks 0 M.empty
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
      \  d = ((f1 c d) (+ (f0 a) (+ 2 b)) a)"
    ]
