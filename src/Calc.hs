class Expr a where
  lit :: Int -> a
  add :: a -> a -> a
  mul :: a -> a -> a

data ExprT
  = Lit Int
  | Add ExprT ExprT
  | Mul ExprT ExprT
  deriving (Show, Eq)

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

instance Expr Int where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (0 <)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Int
  deriving (Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax l) (MinMax r) = MinMax $ max l r
  mul (MinMax l) (MinMax r) = MinMax $ min l r

newtype Mod7 = Mod7 Int
  deriving (Show)

instance Expr Mod7 where
  lit x = Mod7 $ mod x 7
  add (Mod7 l) (Mod7 r) = Mod7 $ mod (l + r) 7
  mul (Mod7 l) (Mod7 r) = Mod7 $ mod (l * r) 7

eval :: ExprT -> Int
eval (Lit x) = x
eval (Add l r) = eval l + eval r
eval (Mul l r) = eval l * eval r

{- NOTE: Let's just pretend this is implemented elsewhere. -}
parseExpr :: (Expr a) => String -> Maybe a
parseExpr = undefined

evalStr :: String -> Maybe Int
evalStr = (eval <$>) . parseExpr

main :: IO ()
main = do
  print $ eval (mul (add (lit 2) (lit 3)) (lit 4) :: ExprT)
  print (add (lit 5) (lit 3) :: Int)
  print (lit 0 :: Bool)
  print (lit 1 :: Bool)
  print (add (lit 5) (lit 3) :: MinMax)
  print (mul (lit 5) (lit 3) :: MinMax)
  print (add (lit 5) (lit 3) :: Mod7)
