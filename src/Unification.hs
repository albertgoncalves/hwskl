import Data.Maybe (fromMaybe)

-- NOTE: See `https://www.cs.cornell.edu/courses/cs3110/2011sp/Lectures/lec26-type-inference/type-inference.htm`.

{-
data Expr
  = Var String
  | App Expr Expr
  | Abs String Expr
  | Let String Expr Expr
-}

data Term
  = Var String
  | App String [Term]

instance Show Term where
  show (Var s) = s
  show (App "Func" [t0, t1]) = '(' : show t0 ++ " -> " ++ show t1 ++ ")"
  show (App "List" [t]) = '[' : show t ++ "]"
  show (App "Int" []) = "Int"
  show (App "Bool" []) = "Bool"
  show _ = undefined

occurs :: String -> Term -> Bool
occurs s0 (Var s1) = s0 == s1
occurs s (App _ ts) = any (occurs s) ts

subst :: Term -> String -> Term -> Term
subst t0 s0 t1@(Var s1)
  | s0 == s1 = t0
  | otherwise = t1
subst t0 s0 (App s1 ts1) = App s1 $ map (subst t0 s0) ts1

-- NOTE: Any given `String` must not occur in any `Term` earlier in the list.
type Subs = [(String, Term)]

apply :: Term -> Subs -> Term
apply = foldr (uncurry $ flip subst)

unify1 :: Term -> Term -> Maybe Subs
unify1 (Var s0) t1@(Var s1)
  | s0 == s1 = Just []
  | otherwise = Just [(s0, t1)]
unify1 (App s0 ts0) (App s1 ts1)
  | (s0 == s1) && (length ts0 == length ts1) = unify $ zip ts0 ts1
  | otherwise = Nothing
unify1 (Var s) t@(App _ _)
  | occurs s t = Nothing
  | otherwise = Just [(s, t)]
unify1 t@(App _ _) (Var s)
  | occurs s t = Nothing
  | otherwise = Just [(s, t)]

unify :: [(Term, Term)] -> Maybe Subs
unify [] = Just []
unify ((t0, t1) : ps) = do
  ss1 <- unify ps
  (++ ss1) <$> unify1 (apply t0 ss1) (apply t1 ss1)

main :: IO ()
main =
  mapM_
    ((>> putChar '\n') . mapM_ print . fromMaybe undefined . unify)
    [ -- x = map singleton
      [ (Var "x", Var "0"),
        (Var "map", App "Func" [Var "singleton", Var "0"]),
        (Var "singleton", App "Func" [Var "c", App "List" [Var "c"]]),
        ( Var "map",
          App
            "Func"
            [ App "Func" [Var "a", Var "b"],
              App "Func" [App "List" [Var "a"], App "List" [Var "b"]]
            ]
        )
      ],
      -- x = id (odd (id 3))
      [ (Var "x", Var "0"),
        (Var "odd", App "Func" [App "Int" [], App "Bool" []]),
        (Var "id1", App "Func" [Var "2", Var "2"]),
        (Var "id3", App "Func" [Var "4", Var "4"]),
        (Var "id3", App "Func" [App "Int" [], Var "5"]),
        (Var "odd", App "Func" [Var "5", Var "6"]),
        (Var "id1", App "Func" [Var "6", Var "0"])
      ]
    ]
