import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
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
showFunc n (Func [] stmts expr) = printf "\\ %s" $ showScope n stmts expr
showFunc n (Func args stmts expr) =
  printf "\\ %s %s" (unwords args) $ showScope n stmts expr

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

main :: IO ()
main =
  ( putStrLn
      . unlines
      . map show
      . fst
      . head
      . readP_to_S parse
  )
    "fib = \\ {\
    \    a = 0\
    \    b = 1\
    \    \\ {\
    \        c = b\
    \        b = (+ a c)\
    \        a = c\
    \        c\
    \    }\
    \}\
    \\
    \f = (fib)\
    \(f)\
    \(f)\
    \(f)\
    \(f)\
    \(f)\
    \(printf \"%ld\n\" (f))"