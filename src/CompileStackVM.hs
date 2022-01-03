{-# LANGUAGE Strict #-}

import Data.Array (Array, listArray, (!))
import Data.List (elemIndex, foldl')

data Inst
  = InstHalt
  | InstPush
  | InstCopy
  | InstStore
  | InstDrop
  | InstSwap
  | InstJump
  | InstJifz
  | InstSub
  | InstLitInt Int
  deriving (Show)

newtype Node = Node {getInt :: Int}

instance Show Node where
  show (Node x) = show x

data BinOp
  = BinOpSub

data AstExpr
  = AstExprInt Int
  | AstExprVar String
  | AstExprBinOp BinOp AstExpr AstExpr

data AstStmt
  = AstStmtAssign String AstExpr

binOpToInst :: BinOp -> Inst
binOpToInst BinOpSub = InstSub

compileExpr :: AstExpr -> [String] -> Int -> ([String], Int, [Inst])
compileExpr (AstExprInt x) vs n = (vs, n + 1, [InstPush, InstLitInt x])
compileExpr (AstExprVar v) vs n =
  case elemIndex v vs of
    Just i -> (vs, n + 1, [InstCopy, InstLitInt $ (n - 1) - i])
    _ -> undefined
compileExpr (AstExprBinOp b l r) vs0 n0 =
  (vs2, n2 - 1, is1 ++ is2 ++ [binOpToInst b])
  where
    (vs1, n1, is1) = compileExpr l vs0 n0
    (vs2, n2, is2) = compileExpr r vs1 n1

compileStmt :: AstStmt -> [String] -> Int -> ([String], Int, [Inst])
compileStmt (AstStmtAssign v x) vs0 n0 =
  case elemIndex v vs1 of
    Just i ->
      let n2 = n1 - 1
          is2 = is1 ++ [InstStore, InstLitInt $ (n2 - 1) - i]
       in (vs1, n2, is2)
    Nothing -> (vs1 ++ [v], n1, is1)
  where
    (vs1, n1, is1) = compileExpr x vs0 n0

compile :: [AstStmt] -> [Inst]
compile =
  (\(_, _, xs) -> xs ++ [InstHalt])
    . foldl'
      (\(vs0, n0, is0) x -> (is0 ++) <$> compileStmt x vs0 n0)
      ([], 0, [])

store :: Int -> a -> [a] -> [a]
store _ _ [] = undefined
store 0 x xs = x : tail xs
store n _ xs
  | (n < 0) || (length xs < n) = undefined
store n x1 (x0 : xs) = x0 : store (n - 1) x1 xs

swap :: [a] -> [a]
swap (b : a : xs) = a : b : xs
swap _ = undefined

pop2 :: [a] -> (a, a, [a])
pop2 (b : a : xs) = (a, b, xs)
pop2 _ = undefined

lit :: Inst -> Int
lit (InstLitInt x) = x
lit _ = undefined

eval :: Array Int Inst -> Int -> [Node] -> [Node]
eval insts i xs =
  case insts ! i of
    InstHalt -> xs
    InstPush -> eval insts (i + 2) (Node (lit $ insts ! (i + 1)) : xs)
    InstCopy -> eval insts (i + 2) ((xs !! lit (insts ! (i + 1))) : xs)
    InstStore ->
      eval insts (i + 2) (store (lit $ insts ! (i + 1)) (head xs) (tail xs))
    InstDrop -> eval insts (i + 2) (drop (lit $ insts ! (i + 1)) xs)
    InstSwap -> eval insts (i + 1) (swap xs)
    InstJump -> eval insts (getInt $ head xs) (tail xs)
    InstJifz ->
      let (a, b, xs') = pop2 xs
       in if getInt a == 0
            then eval insts (getInt b) xs'
            else eval insts (i + 1) xs'
    InstSub ->
      let (a, b, xs') = pop2 xs
       in eval insts (i + 1) (Node (getInt a - getInt b) : xs')
    _ -> undefined

run :: [Inst] -> [Node]
run [] = undefined
run xs = eval (listArray (0, length xs - 1) xs) 0 []

main :: IO ()
main =
  print $
    reverse $
      run $
        compile
          [ AstStmtAssign "w" (AstExprInt (-10)),
            AstStmtAssign "y" (AstExprInt 9),
            AstStmtAssign "x" (AstExprInt 3),
            AstStmtAssign "w" (AstExprInt 1),
            AstStmtAssign
              "y"
              (AstExprBinOp BinOpSub (AstExprVar "w") (AstExprInt 8)),
            AstStmtAssign "z" (AstExprInt 4),
            AstStmtAssign
              "x"
              (AstExprBinOp BinOpSub (AstExprVar "x") (AstExprVar "z")),
            AstStmtAssign
              "u"
              (AstExprBinOp BinOpSub (AstExprVar "x") (AstExprVar "y"))
          ]
