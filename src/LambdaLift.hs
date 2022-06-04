import Data.List (foldl', intercalate)
import Text.Printf (printf)

data Ast
  = AstInt Int
  | AstString String
  | AstVar String
  | AstFn [String] [Ast]
  | AstCall [Ast] [Ast]
  | AstAssign String Ast
  | AstUpdate String Ast
  | AstPair Ast Ast

instance Show Ast where
  show (AstInt int) = show int
  show (AstString str) = show str
  show (AstVar var) = var
  show (AstFn args body) =
    printf "(\\%s -> %s)" (unwords args) (intercalate "; " (map show body))
  show (AstAssign var expr) = printf "%s := %s" var (show expr)
  show (AstUpdate var expr) = printf "%s = %s" var (show expr)
  show (AstCall func []) = printf "((%s) ())" (unwords $ map show func)
  show (AstCall func args) =
    printf "((%s) %s)" (unwords $ map show func) (unwords $ map show args)
  show (AstPair expr0 expr1) = printf "(%s, %s)" (show expr0) (show expr1)

scopeLabel :: Int -> String
scopeLabel = printf "_s%d_"

topScope :: [Ast] -> [Ast]
topScope =
  (AstAssign scope (AstCall [AstVar "@newScope"] []) :)
    . map (injectScope scope (succ k))
  where
    k :: Int
    k = 0
    scope = scopeLabel k

innerScope :: String -> Int -> [Ast] -> [Ast]
innerScope parentScope k =
  (AstAssign scope (AstCall [AstVar "@newScopeFrom"] [AstVar parentScope]) :)
    . map (injectScope scope (succ k))
  where
    scope = scopeLabel k

injectScope :: String -> Int -> Ast -> Ast
injectScope _ _ (AstPair _ _) = undefined
injectScope _ _ int@(AstInt _) = int
injectScope _ _ str@(AstString _) = str
injectScope scope _ (AstVar var) =
  AstCall [AstVar "@lookupScope"] [AstVar scope, AstString var]
injectScope scope k (AstAssign var expr) =
  AstCall
    [AstVar "@insertScope"]
    [AstVar scope, AstString var, injectScope scope k expr]
injectScope scope k (AstUpdate var expr) =
  AstCall
    [AstVar "@updateScope"]
    [AstVar scope, AstString var, injectScope scope k expr]
injectScope scope k (AstCall func args) =
  AstCall (map (injectScope scope k) func) (map (injectScope scope k) args)
injectScope scope k (AstFn args body) =
  AstPair
    (AstVar scope)
    (AstFn (scope : args) (innerScope scope k body))

funcLabel :: Int -> String
funcLabel = printf "_f%d_"

extractFuncs :: Int -> [Ast] -> (Int, [(String, Ast)], [Ast])
extractFuncs k0 = foldl' f (k0, [], [])
  where
    f (k1, funcs1, exprs) expr1 = (k2, funcs1 ++ funcs2, exprs ++ [expr2])
      where
        (k2, funcs2, expr2) = extractFunc k1 expr1

extractFunc :: Int -> Ast -> (Int, [(String, Ast)], Ast)
extractFunc k0 (AstFn args body0) =
  (k1, (label, AstFn args body1) : funcs, AstVar label)
  where
    label = funcLabel k0
    (k1, funcs, body1) = extractFuncs (succ k0) body0
extractFunc k0 (AstCall func0 args0) =
  (k2, funcs1 ++ funcs2, AstCall func1 args1)
  where
    (k1, funcs1, func1) = extractFuncs k0 func0
    (k2, funcs2, args1) = extractFuncs k1 args0
extractFunc k0 (AstAssign var expr0) = (k1, funcs, AstAssign var expr1)
  where
    (k1, funcs, expr1) = extractFunc k0 expr0
extractFunc k0 (AstPair l0 r0) = (k2, funcs1 ++ funcs2, AstPair l1 r1)
  where
    (k1, funcs1, l1) = extractFunc k0 l0
    (k2, funcs2, r1) = extractFunc k1 r0
extractFunc k expr = (k, [], expr)

ast :: [Ast]
ast =
  [ AstAssign "f" (AstFn ["x"] [AstFn [] [AstVar "x"]]),
    AstAssign
      "g"
      ( AstFn
          []
          [ AstAssign "x" (AstInt 0),
            AstUpdate "x" (AstInt $ -1),
            AstVar "x"
          ]
      ),
    AstCall [AstCall [AstVar "f"] [AstCall [AstVar "g"] []]] []
  ]

main :: IO ()
main =
  mapM_
    (putStrLn . ("\n" ++) . intercalate ";\n")
    [ map show ast,
      map show $ map (uncurry AstAssign) funcs ++ exprs
    ]
  where
    (_, funcs, exprs) = extractFuncs 0 $ topScope ast
