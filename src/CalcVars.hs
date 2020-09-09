{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Map.Strict (Map, fromList, lookup)
import Data.Text (Text)
import Prelude hiding (lookup)

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

instance (Monad m) => Expr (Map Text Int -> m Int) where
  lit = const . return
  add l r m = do
    l' <- l m
    r' <- r m
    return $ l' + r'
  mul l r m = do
    l' <- l m
    r' <- r m
    return $ l' * r'

instance HasVars (Map Text Int -> Maybe Int) where
  var = lookup

withVars :: [(Text, Int)] -> (Map Text Int -> Maybe Int) -> Maybe Int
withVars xs = ($ fromList xs)

main :: IO ()
main = do
  print (add (lit 3) (var "x") :: VarExprT)
  print $ withVars [("x", 6)] $ add (lit 3) (var "x")
  print $ withVars [("x", 6)] $ mul (lit 3) (var "x")
  print $ withVars [("x", 6)] $ add (lit 3) (var "y")
  print $
    withVars [("x", 6), ("y", 3)] $
      mul (var "x") (add (var "y") (var "x"))
