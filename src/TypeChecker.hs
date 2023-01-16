import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.List (foldl', sortOn)
import qualified Data.Map as M
import qualified Data.Set as S
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

main :: IO ()
main =
  ( putStrLn
      . unlines
      . map show
      . sortOn (length . snd)
      . M.toList
      . M.map S.toList
      . M.fromListWith S.union
      . map (S.singleton <$>)
      . snd
      . stmtCollects 0
      . fst
      . head
      . readP_to_S parse
  )
    "  a = d\
    \  b = 1\
    \  c = (+ a b)\
    \  d = ((f) (+ c 2) b)"
