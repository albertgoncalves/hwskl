{-# LANGUAGE Strict #-}

import Data.Array (Array, listArray, (!))
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Debug.Trace (trace)
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

data AstExpr
  = AstExprInt Int
  | AstExprVar String
  | AstExprBinOp BinOp AstExpr AstExpr
  | AstExprCall String [AstExpr]

data AstStmt
  = AstStmtAssign String AstExpr
  | AstStmtIf AstExpr [AstStmt]
  | AstStmtReturn AstExpr

data AstFunc = AstFunc
  { getAstFuncName :: String,
    getAstFuncArgs :: [String],
    getAstFuncLocals :: [String],
    getAstFuncAst :: [AstStmt],
    getAstFuncRet :: AstExpr
  }

newtype Labels = Labels
  { getLabelReturn :: String
  }

data Context = Context
  { getContextStackOffset :: Int,
    getContextVars :: M.Map String Int,
    getContextLabels :: Labels,
    getContextCompiler :: Compiler
  }

data Compiler = Compiler
  { getCompilerLabelCount :: Int,
    getCompilerInsts :: [Inst]
  }

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
  case trace (show $ reverse xs) $ insts ! i of
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
       in if getInt b == 0
            then eval insts (getInt a) xs'
            else eval insts (i + 1) xs'
    InstSub ->
      let (a, b, xs') = pop2 xs
       in eval insts (i + 1) (Node (getInt a - getInt b) : xs')
    _ -> undefined

run :: [Inst] -> [Node]
run [] = undefined
run xs = eval (listArray (0, length xs - 1) xs) 0 []

binOpToInst :: BinOp -> Inst
binOpToInst BinOpSub = InstSub

incrStackOffset :: Context -> Context
incrStackOffset (Context stackOffset vars labels compiler) =
  Context (succ stackOffset) vars labels compiler

decrStackOffset :: Context -> Context
decrStackOffset (Context stackOffset vars labels compiler) =
  Context (pred stackOffset) vars labels compiler

setStackOffset :: Context -> Int -> Context
setStackOffset (Context _ vars labels compiler) stackOffset =
  Context stackOffset vars labels compiler

incrLabelCount :: Context -> Context
incrLabelCount (Context stackOffset vars labels (Compiler labelCount insts)) =
  Context stackOffset vars labels $ Compiler (succ labelCount) insts

appendContextInsts :: Context -> [Inst] -> Context
appendContextInsts (Context stackOffset vars labels compiler) insts =
  Context stackOffset vars labels $ appendCompilerInsts compiler insts

getVarOffset :: Context -> String -> Int
getVarOffset context name =
  getContextStackOffset context + getContextVars context M.! name

makeLabel :: Context -> String
makeLabel = printf "_%d" . getCompilerLabelCount . getContextCompiler

pushVar :: Context -> String -> [Inst]
pushVar context name =
  if M.member name $ getContextVars context
    then [InstCopy, InstLitInt $ getVarOffset context name]
    else [PreInstLabelPush name]

compileExpr :: Context -> AstExpr -> Context
compileExpr context (AstExprInt x) =
  incrStackOffset $ appendContextInsts context [InstPush, InstLitInt x]
compileExpr context (AstExprVar name) =
  incrStackOffset $ appendContextInsts context $ pushVar context name
compileExpr context (AstExprBinOp op l r) =
  decrStackOffset $
    appendContextInsts (compileExpr (compileExpr context l) r) [binOpToInst op]
compileExpr context0 (AstExprCall name exprs) =
  setStackOffset
    ( appendContextInsts
        context1
        $ pushVar context1 name ++ [InstJump, PreInstLabelSet label]
    )
    (succ $ getContextStackOffset context0)
  where
    label = makeLabel context0
    context1 =
      foldl'
        compileExpr
        ( incrLabelCount $
            incrStackOffset $
              appendContextInsts
                context0
                [PreInstLabelPush label]
        )
        exprs

compileStmt :: Context -> AstStmt -> Context
compileStmt context (AstStmtAssign name expr) =
  decrStackOffset $
    appendContextInsts
      (compileExpr context expr)
      [InstStore, InstLitInt $ getVarOffset context name]
compileStmt context (AstStmtIf condition body) =
  appendContextInsts
    ( foldl'
        compileStmt
        ( appendContextInsts
            ( compileExpr
                ( appendContextInsts
                    (incrLabelCount $ incrStackOffset context)
                    [PreInstLabelPush label]
                )
                condition
            )
            [InstJifz]
            `setStackOffset` getContextStackOffset context
        )
        body
    )
    [PreInstLabelSet label]
  where
    label = makeLabel context
compileStmt context (AstStmtReturn expr) =
  decrStackOffset $
    appendContextInsts
      (compileExpr context expr)
      [ PreInstLabelPush $ getLabelReturn $ getContextLabels context,
        InstJump
      ]

appendCompilerInsts :: Compiler -> [Inst] -> Compiler
appendCompilerInsts (Compiler labelCount insts0) insts1 =
  Compiler labelCount $ insts0 ++ insts1

makeReturnLabel :: String -> String
makeReturnLabel = printf "_%s_return"

getVars :: [String] -> M.Map String Int
getVars xs = M.fromList $ zip (reverse xs) [0 ..]

compileFunc :: Compiler -> AstFunc -> Compiler
compileFunc compiler0 (AstFunc name [] [] body returnExpr) =
  appendCompilerInsts
    (getContextCompiler context2)
    [PreInstLabelSet returnLabel, InstSwap, InstJump]
  where
    returnLabel = makeReturnLabel name
    context0 = Context 0 M.empty $ Labels returnLabel
    context1 =
      foldl'
        compileStmt
        (context0 $ appendCompilerInsts compiler0 [PreInstLabelSet name])
        body
    context2 = compileExpr (context0 $ getContextCompiler context1) returnExpr
compileFunc compiler0 (AstFunc name args locals body returnExpr) =
  appendCompilerInsts (getContextCompiler context2) $
    PreInstLabelSet returnLabel :
    ( let n = length args + length locals - 1
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
    returnLabel = makeReturnLabel name
    context0 = Context 0 (getVars $ args ++ locals) $ Labels returnLabel
    context1 =
      foldl'
        compileStmt
        ( context0 $
            appendCompilerInsts
              compiler0
              $ PreInstLabelSet name :
              ( let n = length locals
                 in if n == 0
                      then []
                      else [InstRsrv, InstLitInt n]
              )
        )
        body
    context2 = compileExpr (context0 $ getContextCompiler context1) returnExpr

compile :: [AstFunc] -> [Inst]
compile =
  getCompilerInsts
    . foldl'
      compileFunc
      ( Compiler
          1
          [ PreInstLabelPush "_0",
            PreInstLabelPush "main",
            InstJump,
            PreInstLabelSet "_0",
            InstHalt
          ]
      )

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
  (print . head . run . assemble . compile)
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
          AstStmtIf
            (AstExprVar "x")
            [AstStmtAssign "x" (AstExprVar "y")],
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
            ),
          AstStmtReturn $ AstExprBinOp BinOpSub (AstExprVar "z") (AstExprInt 2)
        ]
        (AstExprBinOp BinOpSub (AstExprVar "z") (AstExprInt 1)),
      AstFunc
        "f0"
        ["x"]
        ["y"]
        [ AstStmtIf (AstExprVar "x") [AstStmtReturn $ AstExprInt 0],
          AstStmtAssign "y" (AstExprVar "f1")
        ]
        (AstExprCall "y" [AstExprVar "x", AstExprInt 3]),
      AstFunc
        "main"
        []
        []
        []
        (AstExprCall "f0" [AstExprInt 0])
    ]
