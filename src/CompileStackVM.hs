{-# LANGUAGE Strict #-}

import Data.Array (Array, listArray, (!))
import Data.List (elemIndex, foldl')
import Data.Maybe (fromJust)

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

data Ast
  = AstInt Int
  | AstVar String
  | AstBinOp BinOp Ast Ast
  | AstAssign String Ast

getOffset :: Eq a => Int -> a -> [a] -> Int
getOffset n x = ((n - 1) -) . fromJust . elemIndex x

compile :: Ast -> [String] -> Int -> ([String], Int, [Inst])
compile (AstInt x) vs n = (vs, n + 1, [InstPush, InstLitInt x])
compile (AstVar v) vs n =
  (vs, n + 1, [InstCopy, InstLitInt $ getOffset n v vs])
compile (AstAssign v x) vs0 n0 =
  if v `elem` vs1
    then
      let n2 = n1 - 1
          is2 = is1 ++ [InstStore, InstLitInt $ getOffset n2 v vs1]
       in (vs1, n2, is2)
    else (vs1 ++ [v], n1, is1)
  where
    (vs1, n1, is1) = compile x vs0 n0
compile (AstBinOp b l r) vs0 n0 =
  ( vs2,
    n2 - 1,
    is1 ++ is2
      ++ [ case b of
             BinOpSub -> InstSub
         ]
  )
  where
    (vs1, n1, is1) = compile l vs0 n0
    (vs2, n2, is2) = compile r vs1 n1

compileAll :: [Ast] -> [Inst]
compileAll =
  (\(_, _, x) -> x)
    . fmap (++ [InstHalt])
    . foldl' (\(vs0, n0, is0) x -> (is0 ++) <$> compile x vs0 n0) ([], 0, [])

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
        compileAll
          [ AstAssign "w" (AstInt (-10)),
            AstAssign "y" (AstInt 9),
            AstAssign "x" (AstInt 3),
            AstAssign "w" (AstInt 1),
            AstAssign "y" (AstBinOp BinOpSub (AstVar "w") (AstInt 8)),
            AstAssign "z" (AstInt 4),
            AstAssign "x" (AstBinOp BinOpSub (AstVar "x") (AstVar "z")),
            AstBinOp BinOpSub (AstVar "x") (AstVar "y")
          ]
