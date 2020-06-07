{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map.Strict as M
import Data.Text (Text)

class Expr a where
  lit :: Int -> a
  add :: a -> a -> a
  mul :: a -> a -> a

class HasVars a where
  var :: Text -> a

data VarExprT
  = Lit Int
  | Add VarExprT VarExprT
  | Mul VarExprT VarExprT
  | Var Text
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = Lit
  add = Add
  mul = Mul

instance HasVars VarExprT where
  var = Var

instance (Monad m) => Expr (M.Map Text Int -> m Int) where
  lit = const . return
  add l r m = do
    l' <- l m
    r' <- r m
    return $ l' + r'
  mul l r m = do
    l' <- l m
    r' <- r m
    return $ l' * r'

instance HasVars (M.Map Text Int -> Maybe Int) where
  var = M.lookup

withVars :: [(Text, Int)] -> (M.Map Text Int -> Maybe Int) -> Maybe Int
withVars xs = ($ M.fromList xs)

main :: IO ()
main = do
  print (add (lit 3) (var "x") :: VarExprT)
  print $ withVars [("x", 6)] $ add (lit 3) (var "x")
  print $ withVars [("x", 6)] $ add (lit 3) (var "y")
  print
    $ withVars [("x", 6), ("y", 3)]
    $ mul (var "x") (add (var "y") (var "x"))
