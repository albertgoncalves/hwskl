import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.List (foldl', foldl1')
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
  | ExprVar String
  | ExprStr String
  | ExprFunc Func
  | ExprCall Expr [Expr]

data Stmt
  = StmtBind String Expr
  | StmtDecl String Expr
  | StmtSetPtr String Expr
  | StmtEffect Expr

data Func = Func [String] [Stmt] Expr

data Scope = Scope
  { scopeBindings :: S.Set String,
    scopeClosures :: S.Set String
  }

showExpr :: Int -> Expr -> String
showExpr _ (ExprInt int) = show int
showExpr _ (ExprStr str) = show str
showExpr _ (ExprVar var) = var
showExpr n (ExprFunc func) = showFunc n func
showExpr n (ExprCall call []) = printf "(%s)" $ showExpr n call
showExpr n (ExprCall call args) =
  printf "(%s %s)" (showExpr n call) $ unwords $ map (showExpr n) args

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
showStmt n (StmtEffect effectExpr) = showExpr n effectExpr
showStmt n (StmtBind var expr) = printf "%s = %s" var $ showExpr n expr
showStmt n (StmtDecl var expr) = printf "decl %s %s" var $ showExpr n expr
showStmt n (StmtSetPtr var expr) = printf "setptr %s %s" var $ showExpr n expr

showFunc :: Int -> Func -> String
showFunc n (Func args stmts expr) =
  printf "\\%s %s" (unwords args) $ showScope n stmts expr

instance Show Stmt where
  show = showStmt 0

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
          return $ ExprFunc $ Func args (reverse body) returnExpr
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

--  _f0_ a b {
--      decl c (deref b)
--      setptr b (+ (deref a) c)
--      setptr a c
--      c
--  }
--
--  fib {
--      decl a (alloc 0)
--      decl b (alloc 1)
--      (alloc _f0_ a b)
--  }
--
--  main {
--      decl f (fib)
--      (callclosure 2 f)
--      (callclosure 2 f)
--      (callclosure 2 f)
--      (callclosure 2 f)
--      (callclosure 2 f)
--      (printf "%ld\n" (callclosure 2 f))
--  }

intrinsics :: S.Set String
intrinsics = S.fromList ["printf", "+", "-", "*", "/", "%"]

exprCollect :: S.Set String -> Expr -> S.Set String
exprCollect _ (ExprInt _) = S.empty
exprCollect bindings (ExprVar var)
  | S.member var bindings = S.empty
  | otherwise = S.singleton var
exprCollect _ (ExprStr _) = S.empty
exprCollect bindings0 (ExprFunc (Func args stmts expr)) =
  S.difference (S.union closures1 closures2) bindings0
  where
    (Scope bindings1 closures1) =
      foldl'
        stmtCollect
        (Scope (S.union intrinsics $ S.fromList args) S.empty)
        stmts
    closures2 = exprCollect bindings1 expr
exprCollect bindings (ExprCall call args) =
  foldl1' S.union $ map (exprCollect bindings) $ call : args

stmtCollect :: Scope -> Stmt -> Scope
stmtCollect (Scope bindings closures0) (StmtBind var expr)
  | S.member var closures1 = Scope bindings closures1
  | otherwise = Scope (S.insert var bindings) closures1
  where
    closures1 = S.union closures0 $ exprCollect bindings expr
stmtCollect _ (StmtDecl _ _) = undefined
stmtCollect _ (StmtSetPtr _ _) = undefined
stmtCollect (Scope bindings closures) (StmtEffect expr) =
  Scope bindings (S.union closures $ exprCollect bindings expr)

exprClosure :: Scope -> Expr -> Expr
exprClosure _ expr@(ExprInt _) = expr
exprClosure (Scope _ closures) expr@(ExprVar var)
  | S.member var closures = ExprCall (ExprVar "deref") [expr]
  | otherwise = expr
exprClosure _ expr@(ExprStr _) = expr
exprClosure _ (ExprFunc (Func args stmts0 expr0)) =
  case S.toList $ S.difference closures2 bindings of
    [] -> ExprFunc $ Func args stmts1 expr1
    escaped ->
      ExprCall (ExprVar "alloc") $
        ExprFunc (Func (args ++ escaped) stmts1 expr1) : map ExprVar escaped
  where
    (Scope bindings closures1, stmts1) =
      transform (Scope (S.union intrinsics $ S.fromList args) S.empty) stmts0
    closures2 = S.union closures1 $ exprCollect bindings expr0
    expr1 = exprClosure (Scope bindings closures2) expr0
exprClosure scope (ExprCall call args) =
  ExprCall (exprClosure scope call) $ map (exprClosure scope) args

stmtClosure :: Scope -> Stmt -> Stmt
stmtClosure scope@(Scope bindings closures) (StmtBind var expr0)
  | S.member var bindings = StmtDecl var expr1
  | S.member var closures = StmtSetPtr var expr1
  | otherwise = undefined
  where
    expr1 = exprClosure scope expr0
stmtClosure _ (StmtDecl _ _) = undefined
stmtClosure _ (StmtSetPtr _ _) = undefined
stmtClosure scope (StmtEffect expr) = StmtEffect $ exprClosure scope expr

transform :: Scope -> [Stmt] -> (Scope, [Stmt])
transform scope [] = (scope, [])
transform scope0 (stmt : stmts) =
  (stmtClosure scope1 stmt :) <$> transform scope1 stmts
  where
    scope1 = stmtCollect scope0 stmt

main :: IO ()
main =
  ( putStrLn
      . unlines
      . map show
      . snd
      . transform (Scope intrinsics S.empty)
      . fst
      . head
      . readP_to_S parse
  )
    "fib = \\ {\
    \    a = 0\
    \    b = 1\
    \    \\ {\
    \        \\ {\
    \            c = b\
    \            b = (+ a c)\
    \            a = c\
    \            c\
    \        }\
    \    }\
    \}\
    \\
    \f = (fib)\
    \((f))\
    \((f))\
    \((f))\
    \((f))\
    \((f))\
    \(printf \"%ld\n\" ((f)))"
