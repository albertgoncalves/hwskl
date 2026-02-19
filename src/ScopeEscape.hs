{-# LANGUAGE LambdaCase #-}

import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.List (foldl1')
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

data Type
  = TypeAtom
  | TypeFunc Int Type
  | TypeClosure Int Int Type

showExpr :: Int -> Expr -> String
showExpr _ (ExprInt int) = show int
showExpr _ (ExprVar var) = var
showExpr _ (ExprStr str) = show str
showExpr n (ExprFunc func) = showFunc n func
showExpr n (ExprCall call args) =
  printf "(%s)" $ unwords $ showExpr n call : map (showExpr n) args

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
showStmt n (StmtDecl var expr) = printf "decl %s %s" var $ showExpr n expr
showStmt n (StmtSetPtr var expr) = printf "setptr %s %s" var $ showExpr n expr
showStmt n (StmtEffect effectExpr) = showExpr n effectExpr

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

exprEscaped :: Expr -> [String]
exprEscaped (ExprInt _) = []
exprEscaped (ExprVar _) = []
exprEscaped (ExprStr _) = []
exprEscaped (ExprFunc _) = []
exprEscaped (ExprCall (ExprVar "alloc-closure") (ExprFunc _ : args)) =
  map
    ( \case
        ExprVar var -> var
        _ -> undefined
    )
    args
exprEscaped (ExprCall _ _) = []

stmtEscaped :: Stmt -> [String]
stmtEscaped (StmtBind _ _) = undefined
stmtEscaped (StmtDecl _ expr) = exprEscaped expr
stmtEscaped (StmtSetPtr _ expr) = exprEscaped expr
stmtEscaped (StmtEffect expr) = exprEscaped expr

allocDecls :: S.Set String -> Stmt -> Stmt
allocDecls decls (StmtDecl var expr)
  | S.member var decls = StmtDecl var $ ExprCall (ExprVar "alloc") [expr]
allocDecls _ stmt = stmt

exprClosure :: Scope -> Expr -> Expr
exprClosure _ expr@(ExprInt _) = expr
exprClosure (Scope _ closures) expr@(ExprVar var)
  | S.member var closures = ExprCall (ExprVar "deref") [expr]
  | otherwise = expr
exprClosure _ expr@(ExprStr _) = expr
exprClosure _ (ExprFunc (Func args stmts0 expr0)) =
  case S.toList $ S.difference closures2 bindings of
    [] -> ExprFunc $ Func args stmts2 expr1
    fromAbove ->
      ExprCall (ExprVar "alloc-closure") $
        ExprFunc (Func (args ++ fromAbove) stmts2 expr1)
          : map ExprVar fromAbove
  where
    (Scope bindings closures1, stmts1) =
      transform (Scope (S.union intrinsics $ S.fromList args) S.empty) stmts0
    closures2 = S.union closures1 $ exprCollect bindings expr0
    expr1 = exprClosure (Scope bindings closures2) expr0
    stmts2 =
      map
        ( allocDecls $
            S.fromList $
              concatMap stmtEscaped stmts1 ++ exprEscaped expr1
        )
        stmts1
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

funcLabel :: Int -> String
funcLabel = printf "_fn_%d_"

exprLift :: Int -> Expr -> (Int, Expr, [(String, Func)])
exprLift k expr@(ExprInt _) = (k, expr, [])
exprLift k expr@(ExprVar _) = (k, expr, [])
exprLift k expr@(ExprStr _) = (k, expr, [])
exprLift k0 (ExprFunc (Func args stmts0 expr0)) =
  ( succ k2,
    ExprVar label,
    funcs1 ++ funcs2 ++ [(label, Func args stmts1 expr2)]
  )
  where
    (k1, stmts1, funcs1) = stmtLift k0 stmts0
    (k2, expr2, funcs2) = exprLift k1 expr0
    label = funcLabel k2
exprLift k0 (ExprCall call0 args0) = (k2, ExprCall call1 args2, funcs2)
  where
    (k1, call1, funcs1) = exprLift k0 call0
    (k2, args2, funcs2) =
      foldl'
        ( \(k3, exprs3, funcs3) expr3 ->
            let (k4, expr4, funcs4) = exprLift k3 expr3
             in (k4, exprs3 ++ [expr4], funcs3 ++ funcs4)
        )
        (k1, [], funcs1)
        args0

stmtLift :: Int -> [Stmt] -> (Int, [Stmt], [(String, Func)])
stmtLift k [] = (k, [], [])
stmtLift _ (StmtBind _ _ : _) = undefined
stmtLift k0 (StmtDecl var expr0 : stmts0) =
  (k2, StmtDecl var expr1 : stmts2, funcs1 ++ funcs2)
  where
    (k1, expr1, funcs1) = exprLift k0 expr0
    (k2, stmts2, funcs2) = stmtLift k1 stmts0
stmtLift k0 (StmtSetPtr var expr0 : stmts0) =
  (k2, StmtSetPtr var expr1 : stmts2, funcs1 ++ funcs2)
  where
    (k1, expr1, funcs1) = exprLift k0 expr0
    (k2, stmts2, funcs2) = stmtLift k1 stmts0
stmtLift k0 (StmtEffect expr0 : stmts0) =
  (k2, StmtEffect expr1 : stmts2, funcs1 ++ funcs2)
  where
    (k1, expr1, funcs1) = exprLift k0 expr0
    (k2, stmts2, funcs2) = stmtLift k1 stmts0

intoStmt :: String -> Func -> Stmt
intoStmt label func = StmtDecl label (ExprFunc func)

exprType :: M.Map String Type -> Expr -> Type
exprType _ (ExprInt _) = TypeAtom
exprType _ (ExprVar _) = TypeAtom
exprType _ (ExprStr _) = TypeAtom
exprType _ (ExprFunc _) = undefined
exprType types (ExprCall (ExprVar "alloc-closure") (ExprVar var : args)) =
  case M.lookup var types of
    Just (TypeFunc arity returnType) ->
      TypeClosure (length args) (arity - length args) returnType
    _ -> undefined
exprType _ (ExprCall _ _) = TypeAtom

stmtType :: M.Map String Type -> Stmt -> M.Map String Type
stmtType _ (StmtBind _ _) = undefined
stmtType types (StmtDecl label (ExprFunc (Func args _ expr))) =
  M.insert label (TypeFunc (length args) $ exprType types expr) types
stmtType types (StmtDecl label (ExprVar var)) =
  M.insert label ((M.!) types var) types
stmtType _ (StmtDecl _ _) = undefined
stmtType _ (StmtSetPtr _ _) = undefined
stmtType _ (StmtEffect _) = undefined

exprSwap :: M.Map String Type -> Expr -> (Expr, Maybe Type)
exprSwap _ expr@(ExprInt _) = (expr, Nothing)
exprSwap types expr@(ExprVar var) = (expr, M.lookup var types)
exprSwap _ expr@(ExprStr _) = (expr, Nothing)
exprSwap types0 (ExprFunc (Func args stmts0 expr)) =
  (ExprFunc $ Func args stmts2 $ fst $ exprSwap types2 expr, Nothing)
  where
    (types2, stmts2) =
      foldl'
        ( \(types1, stmts1) stmt1 ->
            (stmts1 ++) . (: []) <$> stmtSwap types1 stmt1
        )
        (types0, [])
        stmts0
exprSwap types (ExprCall call0@(ExprVar _) args0) =
  case exprSwap types call0 of
    (call1, Just (TypeClosure arity _ returnType)) ->
      ( ExprCall (ExprVar "call-closure") $ ExprInt arity : call1 : args1,
        Just returnType
      )
    (call1, Just (TypeFunc _ returnType)) ->
      (ExprCall call1 args1, Just returnType)
    (call1, _) -> (ExprCall call1 args1, Nothing)
  where
    args1 = map (fst . exprSwap types) args0
exprSwap types expr@(ExprCall call0 args0) =
  case exprSwap types call0 of
    (call1, Just (TypeClosure arity _ returnType)) ->
      ( ExprCall (ExprVar "call-closure") $ ExprInt arity : call1 : args1,
        Just returnType
      )
    (call1, Just (TypeFunc _ returnType)) ->
      (ExprCall call1 args1, Just returnType)
    (_, _) -> (expr, Nothing)
  where
    args1 = map (fst . exprSwap types) args0

stmtSwap :: M.Map String Type -> Stmt -> (M.Map String Type, Stmt)
stmtSwap _ (StmtBind _ _) = undefined
stmtSwap types (StmtDecl var expr0) =
  case exprSwap types expr0 of
    (expr1, Just returnType) ->
      (M.insert var returnType types, StmtDecl var expr1)
    (expr1, Nothing) -> (types, StmtDecl var expr1)
stmtSwap types (StmtSetPtr var expr0) =
  case exprSwap types expr0 of
    (expr1, Just returnType) ->
      (M.insert var returnType types, StmtSetPtr var expr1)
    (expr1, Nothing) -> (types, StmtSetPtr var expr1)
stmtSwap types (StmtEffect expr) =
  (types, StmtEffect $ fst $ exprSwap types expr)

main :: IO ()
main =
  ( putStrLn
      . unlines
      . map (show . snd)
      . (\stmts -> map (stmtSwap $ foldl' stmtType M.empty stmts) stmts)
      . (\(_, stmts, funcs) -> map (uncurry intoStmt) funcs ++ stmts)
      . stmtLift 0
      . snd
      . transform (Scope intrinsics S.empty)
      . (\stmts -> [StmtBind "_main_" $ ExprFunc $ Func [] stmts $ ExprInt 0])
      . fst
      . head
      . readP_to_S parse
  )
    "  fib = \\ {\
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
    \  (printf \"%ld\n\" ((f)))"
