{-# LANGUAGE Strict #-}

import Data.Array (Array, listArray, (!))
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Text.Printf (printf)

data Inst
  = InstHalt
  | InstPush
  | InstCopy
  | InstStore
  | InstDrop
  | InstRsrv
  | InstSwap
  | InstJump
  | InstJifz
  | InstSub
  | InstLitInt Int
  | PreInstLabelSet String
  | PreInstLabelPush String
  deriving (Show)

newtype Node = Node {getInt :: Int}

instance Show Node where
  show (Node x) = show x

data BinOp
  = BinOpSub
  deriving (Show)

data AstExpr
  = AstExprInt Int
  | AstExprVar String
  | AstExprBinOp BinOp AstExpr AstExpr
  | AstExprCall String [AstExpr]
  deriving (Show)

data AstStmt
  = AstStmtAssign String AstExpr
  deriving (Show)

data AstFunc = AstFunc
  { getAstFuncName :: String,
    getAstFuncArgs :: [String],
    getAstFuncLocals :: [String],
    getAstFuncAst :: [AstStmt],
    getAstFuncRet :: AstExpr
  }
  deriving (Show)

data Compiler = Compiler
  { getLabelCount :: Int,
    getInsts :: [Inst]
  }

data CompilerStack = CompilerStack
  { getCompiler :: Compiler,
    getStackOffset :: Int
  }

instance Semigroup Compiler where
  (Compiler _ xs0) <> (Compiler n1 xs1) = Compiler n1 $ xs0 ++ xs1

instance Semigroup CompilerStack where
  (CompilerStack c0 _) <> (CompilerStack c1 n1) = CompilerStack (c0 <> c1) n1

store :: Int -> a -> [a] -> [a]
store _ _ [] = undefined
store 0 x xs = x : tail xs
store n _ xs
  | (n < 0) || (length xs < n) = undefined
store n x1 (x0 : xs) = x0 : store (n - 1) x1 xs

reserve :: Int -> a -> [a] -> [a]
reserve n a = (replicate n a ++)

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
    InstPush -> eval insts (i + 2) $ Node (lit $ insts ! (i + 1)) : xs
    InstCopy -> eval insts (i + 2) $ (xs !! lit (insts ! (i + 1))) : xs
    InstStore ->
      eval insts (i + 2) $ store (lit $ insts ! (i + 1)) (head xs) $ tail xs
    InstDrop -> eval insts (i + 2) $ drop (lit $ insts ! (i + 1)) xs
    InstRsrv ->
      eval insts (i + 2) $ reserve (lit $ insts ! (i + 1)) (Node 0) xs
    InstSwap -> eval insts (i + 1) $ swap xs
    InstJump -> eval insts (getInt $ head xs) $ tail xs
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

append :: (a -> Int -> Compiler) -> Int -> [a] -> Compiler
append f n0 = foldl' (\c@(Compiler n1 _) x -> c <> f x n1) (Compiler n0 [])

binOpToInst :: BinOp -> Inst
binOpToInst BinOpSub = InstSub

lookupVar :: M.Map String Int -> String -> Int -> [Inst]
lookupVar vars name n =
  case M.lookup name vars of
    Just x -> [InstCopy, InstLitInt $ n + x]
    Nothing -> [PreInstLabelPush name]

compileExpr :: M.Map String Int -> AstExpr -> Int -> Int -> CompilerStack
compileExpr _ (AstExprInt x) n l =
  CompilerStack (Compiler l [InstPush, InstLitInt x]) $ succ n
compileExpr vars (AstExprVar name) n l =
  CompilerStack (Compiler l $ lookupVar vars name n) $ succ n
compileExpr vars (AstExprBinOp op l r) n0 l0 =
  CompilerStack
    (Compiler l2 $ insts1 ++ insts2 ++ [binOpToInst op])
    $ succ n0
  where
    (CompilerStack (Compiler l1 insts1) n1) = compileExpr vars l n0 l0
    (CompilerStack (Compiler l2 insts2) _) = compileExpr vars r n1 l1
compileExpr vars (AstExprCall name exprs) n0 l0 =
  CompilerStack
    ( Compiler (succ l3) $
        PreInstLabelPush label :
        insts3 ++ lookupVar vars name n3 ++ [InstJump, PreInstLabelSet label]
    )
    $ succ n0
  where
    label :: String
    label = printf "_%d" l3
    (CompilerStack (Compiler l3 insts3) n3) =
      foldl'
        ( \c1@(CompilerStack (Compiler l1 _) n1) x ->
            c1 <> compileExpr vars x n1 l1
        )
        -- NOTE: When compiling a `call`, we need to account for the return
        -- address of the call on the stack, so we need to use `n0 + 1` here.
        (CompilerStack (Compiler l0 []) $ succ n0)
        exprs

compileStmt :: M.Map String Int -> AstStmt -> Int -> Compiler
compileStmt vars (AstStmtAssign name expr) labelCount =
  Compiler l1 $ insts1 ++ [InstStore, InstLitInt $ vars M.! name]
  where
    (CompilerStack (Compiler l1 insts1) _) = compileExpr vars expr 0 labelCount

getVars :: [String] -> M.Map String Int
getVars xs = M.fromList $ zip (reverse xs) [0 ..]

compileFunc :: AstFunc -> Int -> Compiler
compileFunc (AstFunc name [] [] stmts expr) labelCount =
  Compiler l1 $ PreInstLabelSet name : insts0 ++ insts1 ++ [InstSwap, InstJump]
  where
    (Compiler l0 insts0) = append (compileStmt M.empty) labelCount stmts
    (Compiler l1 insts1) = getCompiler $ compileExpr M.empty expr 0 l0
compileFunc (AstFunc name args locals stmts expr) labelCount =
  Compiler l1 $
    PreInstLabelSet name :
    ( let n = length locals
       in if n == 0
            then []
            else [InstRsrv, InstLitInt n]
    )
      ++ insts0
      ++ insts1
      ++ ( let n = length args + length locals - 1
            in if n == 0
                 then [InstStore, InstLitInt n, InstSwap, InstJump]
                 else
                   [ InstStore,
                     InstLitInt n,
                     InstDrop,
                     InstLitInt n,
                     InstSwap,
                     InstJump
                   ]
         )
  where
    vars = getVars $ args ++ locals
    (Compiler l0 insts0) = append (compileStmt vars) labelCount stmts
    (Compiler l1 insts1) = getCompiler $ compileExpr vars expr 0 l0

compile :: [AstFunc] -> [Inst]
compile =
  ( [ PreInstLabelPush "_0",
      PreInstLabelPush "main",
      InstJump,
      PreInstLabelSet "_0",
      InstHalt
    ]
      ++
  )
    . getInsts
    . append compileFunc 1

weightInst :: Inst -> Int
weightInst (PreInstLabelPush _) = 2
weightInst (PreInstLabelSet _) = 0
weightInst _ = 1

getLabels :: [Inst] -> M.Map String Int
getLabels xs = M.fromList $ f $ zip xs $ scanl (+) 0 $ map weightInst xs
  where
    f [] = []
    f ((PreInstLabelSet x, n) : xs') = (x, n) : f xs'
    f (_ : xs') = f xs'

resolve :: [Inst] -> M.Map String Int -> [Inst]
resolve [] _ = []
resolve (PreInstLabelSet _ : xs) m = resolve xs m
resolve (PreInstLabelPush x : xs) m =
  [InstPush, InstLitInt (m M.! x)] ++ resolve xs m
resolve (x : xs) m = x : resolve xs m

assemble :: [Inst] -> [Inst]
assemble xs = resolve xs $ getLabels xs

main :: IO ()
main =
  print $
    run $
      assemble $
        compile
          [ AstFunc
              "f2"
              []
              ["x"]
              [ AstStmtAssign
                  "x"
                  (AstExprBinOp BinOpSub (AstExprInt 3) (AstExprInt 10))
              ]
              (AstExprVar "x"),
            AstFunc
              "f1"
              ["x", "y"]
              ["z", "u"]
              [ AstStmtAssign
                  "x"
                  (AstExprBinOp BinOpSub (AstExprVar "x") (AstExprVar "y")),
                AstStmtAssign
                  "u"
                  ( AstExprBinOp
                      BinOpSub
                      (AstExprBinOp BinOpSub (AstExprVar "f2") (AstExprInt 2))
                      (AstExprInt $ -2)
                  ),
                AstStmtAssign
                  "z"
                  ( AstExprBinOp
                      BinOpSub
                      (AstExprVar "x")
                      (AstExprCall "u" [])
                  )
              ]
              (AstExprBinOp BinOpSub (AstExprVar "z") (AstExprInt 1)),
            AstFunc
              "f0"
              ["x"]
              ["y"]
              [AstStmtAssign "y" (AstExprVar "f1")]
              (AstExprCall "y" [AstExprVar "x", AstExprInt 3]),
            AstFunc
              "main"
              []
              []
              []
              (AstExprCall "f0" [AstExprInt $ -1])
          ]
