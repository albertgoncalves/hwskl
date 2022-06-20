import Data.Char (ord)
import Data.List (elemIndex, intercalate)
import Text.Printf (printf)

data Intrin
  = IntrinPrintf

data Label
  = LabelFunc String
  | LabelIntrin Intrin

data Expr
  = ExprI64 Int
  | ExprVar String
  | ExprStr String
  | ExprAssign String Expr
  | ExprUpdate String Expr
  | ExprCall Label [Expr]
  | ExprDrop Expr
  | ExprIf Expr [Expr] [Expr]
  | ExprEq Expr Expr
  | ExprAdd Expr Expr
  | ExprSub Expr Expr
  | ExprRet Expr

data Reg
  = RegRdi
  | RegRsi
  | RegRdx
  | RegR10
  | RegR8
  | RegR9
  | RegRax
  | RegEax
  | RegRbp
  | RegRsp
  | RegRcx
  | RegR11
  deriving (Eq)

instance Show Reg where
  show RegRdi = "rdi"
  show RegRsi = "rsi"
  show RegRdx = "rdx"
  show RegR10 = "r10"
  show RegR8 = "r8"
  show RegR9 = "r9"
  show RegRax = "rax"
  show RegEax = "eax"
  show RegRbp = "rbp"
  show RegRsp = "rsp"
  show RegRcx = "rcx"
  show RegR11 = "r11"

data Func = Func String [String] [Expr]

data Op
  = OpReg Reg
  | OpDeref Reg Int
  | OpImm Int
  | OpLabel String
  deriving (Eq)

instance Show Op where
  show (OpReg reg) = show reg
  show (OpDeref reg offset)
    | offset == 0 = printf "qword [%s]" (show reg)
    | 0 < offset = printf "qword [%s + %d]" (show reg) offset
    | otherwise = printf "qword [%s - %d]" (show reg) $ abs offset
  show (OpImm value) = show value
  show (OpLabel label) = label

data Inst
  = InstLabel String
  | InstCall Op
  | InstRet
  | InstPush Op
  | InstPop Op
  | InstMov Op Op
  | InstXor Op Op
  | InstSysCall
  | InstJmp Op
  | InstCmp Op Op
  | InstJnz Op
  | InstAdd Op Op
  | InstSub Op Op

instance Show Inst where
  show (InstLabel label) = printf "%s:" label
  show (InstCall op) = printf "\tcall %s" (show op)
  show (InstJmp op) = printf "\tjmp %s" (show op)
  show InstRet = "\tret"
  show (InstPush op) = printf "\tpush %s" (show op)
  show (InstPop op) = printf "\tpop %s" (show op)
  show (InstMov op0 op1) = printf "\tmov %s, %s" (show op0) (show op1)
  show (InstXor op0 op1) = printf "\txor %s, %s" (show op0) (show op1)
  show InstSysCall = "\tsyscall"
  show (InstCmp op0 op1) = printf "\tcmp %s, %s" (show op0) (show op1)
  show (InstJnz op) = printf "\tjnz %s" (show op)
  show (InstAdd op0 op1) = printf "\tadd %s, %s" (show op0) (show op1)
  show (InstSub op0 op1) = printf "\tsub %s, %s" (show op0) (show op1)

data ContextExpr = ContextExpr ContextFunc [String] Bool

data ContextFunc = ContextFunc [String] Int

argRegs :: [Reg]
argRegs = [RegRdi, RegRsi, RegRdx, RegR10, RegR8, RegR9]

labelString :: Int -> String
labelString = printf "_s%d_"

compileExpr :: ContextExpr -> Expr -> (ContextExpr, [Inst])
compileExpr context0 (ExprDrop expr0) =
  (context1, expr1 ++ [InstAdd (OpReg RegRsp) (OpImm 8)])
  where
    (context1, expr1) = compileExpr context0 expr0
compileExpr context0@(ContextExpr _ _ needStack) (ExprRet expr0) =
  ( context1,
    expr1
      ++ [InstPop (OpReg RegRax)]
      ++ ( if needStack
             then
               [ InstMov (OpReg RegRsp) (OpReg RegRbp),
                 InstPop (OpReg RegRbp)
               ]
             else []
         )
      ++ [InstRet]
  )
  where
    (context1, expr1) = compileExpr context0 expr0
compileExpr context0 (ExprAdd l0 r1) =
  ( context2,
    l1
      ++ [InstPop (OpReg RegRcx)]
      ++ r2
      ++ [ InstPop (OpReg RegR11),
           InstAdd (OpReg RegRcx) (OpReg RegR11),
           InstPush (OpReg RegRcx)
         ]
  )
  where
    (context1, l1) = compileExpr context0 l0
    (context2, r2) = compileExpr context1 r1
compileExpr context0@(ContextExpr _ _ needStack) (ExprSub l0 r1) =
  ( context2,
    l1
      ++ [InstPop (OpReg RegRcx)]
      ++ r2
      ++ ( if needStack
             then
               [ InstPop (OpReg RegR11),
                 InstSub (OpReg RegRcx) (OpReg RegR11)
               ]
             else []
         )
      ++ [InstPush (OpReg RegRcx)]
  )
  where
    (context1, l1) = compileExpr context0 l0
    (context2, r2) = compileExpr context1 r1
compileExpr _ (ExprEq _ _) = undefined
compileExpr
  (ContextExpr (ContextFunc strings k) locals needStack)
  (ExprIf (ExprEq l0 r1) ifThen2 ifElse3)
    | thenReturns && elseReturns =
      ( context4,
        l1
          ++ [InstPop (OpReg RegRcx)]
          ++ r2
          ++ [ InstPop (OpReg RegR11),
               InstCmp (OpReg RegRcx) (OpReg RegR11),
               InstJnz (OpLabel labelElse)
             ]
          ++ ifThen3
          ++ [InstLabel labelElse]
          ++ ifElse4
      )
    | thenReturns || elseReturns = undefined
    | otherwise =
      ( context4,
        l1
          ++ [InstPop (OpReg RegRcx)]
          ++ r2
          ++ [ InstPop (OpReg RegR11),
               InstCmp (OpReg RegRcx) (OpReg RegR11),
               InstJnz (OpLabel labelElse)
             ]
          ++ ifThen3
          ++ [InstJmp (OpLabel labelEnd), InstLabel labelElse]
          ++ ifElse4
          ++ [InstLabel labelEnd]
      )
    where
      thenReturns = isRet ifThen2
      elseReturns = isRet ifElse3
      (context1, l1) =
        compileExpr
          (ContextExpr (ContextFunc strings (succ k)) locals needStack)
          l0
      (context2, r2) = compileExpr context1 r1
      (context3, ifThen3) = compileExprs context2 ifThen2
      (context4, ifElse4) = compileExprs context3 ifElse3
      labelElse :: String
      labelElse = printf "_else%d_" k
      labelEnd :: String
      labelEnd = printf "_end%d_" k
compileExpr _ ExprIf {} = undefined
compileExpr context (ExprI64 i) = (context, [InstPush (OpImm i)])
compileExpr
  context@(ContextExpr (ContextFunc strings k) locals needStack)
  (ExprStr s) =
    case elemIndex s strings of
      Nothing ->
        ( ContextExpr (ContextFunc (s : strings) k) locals needStack,
          [InstPush (OpLabel $ labelString $ length strings)]
        )
      Just n ->
        (context, [InstPush (OpLabel $ labelString $ length strings - n)])
compileExpr context@(ContextExpr _ locals _) (ExprVar v) =
  case elemIndex v locals of
    Nothing -> undefined
    Just n ->
      (context, [InstPush (OpDeref RegRbp $ negate $ (length locals - n) * 8)])
compileExpr contextExpr (ExprAssign var expr) =
  (ContextExpr contextFunc (var : locals) needStack, asm)
  where
    (ContextExpr contextFunc locals needStack, asm) =
      compileExpr contextExpr expr
compileExpr _ (ExprUpdate _ _) = undefined
compileExpr context0 (ExprCall (LabelFunc label) exprs) =
  (context1, asm ++ [InstCall (OpLabel label), InstPush (OpReg RegRax)])
  where
    (context1, asm) = compileCallArgs context0 argRegs exprs
compileExpr context0 (ExprCall (LabelIntrin intrin) exprs) =
  (context1, asm ++ compileIntrin intrin)
  where
    (context1, asm) = compileCallArgs context0 argRegs exprs

isRet :: [Expr] -> Bool
isRet [] = False
isRet [ExprRet _] = True
isRet (ExprRet _ : _) = undefined
isRet (_ : exprs) = isRet exprs

compileExprs :: ContextExpr -> [Expr] -> (ContextExpr, [Inst])
compileExprs context [] = (context, [])
compileExprs context0 (expr : exprs) = (context2, asm1 ++ asm2)
  where
    (context1, asm1) = compileExpr context0 expr
    (context2, asm2) = compileExprs context1 exprs

compileIntrin :: Intrin -> [Inst]
compileIntrin IntrinPrintf =
  [ InstXor (OpReg RegEax) (OpReg RegEax),
    InstCall (OpLabel "printf"),
    InstPush (OpReg RegRax)
  ]

compileCallArgs :: ContextExpr -> [Reg] -> [Expr] -> (ContextExpr, [Inst])
compileCallArgs _ [] _ = undefined
compileCallArgs context _ [] = (context, [])
compileCallArgs context0 (reg : regs) (expr : exprs) =
  (context2, asm1 ++ asm2 ++ [InstPop (OpReg reg)])
  where
    (context1, asm1) = compileExpr context0 expr
    (context2, asm2) = compileCallArgs context1 regs exprs

compileFuncArgs :: [Reg] -> [String] -> [Inst]
compileFuncArgs [] _ = undefined
compileFuncArgs _ [] = []
compileFuncArgs (reg : regs) (_ : args) =
  InstPush (OpReg reg) : compileFuncArgs regs args

returnLast :: [Expr] -> [Expr]
returnLast [] = []
returnLast [ExprIf condition ifThen ifElse] =
  [ExprIf condition (returnLast ifThen) (returnLast ifElse)]
returnLast [expr] = [ExprRet expr]
returnLast (expr : exprs) = expr : returnLast exprs

compileFunc ::
  ContextFunc -> String -> [String] -> [Expr] -> (ContextFunc, [Inst])
compileFunc context0 label args exprs =
  ( context1,
    optimize $
      [InstLabel label]
        ++ ( if needStack
               then
                 [ InstPush (OpReg RegRbp),
                   InstMov (OpReg RegRbp) (OpReg RegRsp)
                 ]
               else []
           )
        ++ compileFuncArgs argRegs args
        ++ asm
  )
  where
    (ContextExpr context1 _ _, asm) =
      compileExprs
        (ContextExpr context0 (reverse args) needStack)
        $ returnLast exprs
    needStack = not (null args) || any isAssignOrUpdate exprs

isAssignOrUpdate :: Expr -> Bool
isAssignOrUpdate (ExprAssign _ _) = True
isAssignOrUpdate (ExprUpdate _ _) = True
isAssignOrUpdate _ = False

compileFuncs :: ContextFunc -> [Func] -> (ContextFunc, [Inst])
compileFuncs context [] = (context, [])
compileFuncs context0 ((Func label args exprs) : funcs) =
  (context2, insts1 ++ insts2)
  where
    (context1, insts1) = compileFunc context0 label args exprs
    (context2, insts2) = compileFuncs context1 funcs

optPushPop :: [Inst] -> [Inst]
optPushPop [] = []
optPushPop (InstPush opPush : InstPop opPop : insts)
  | opPush == opPop = optPushPop insts
  | otherwise = InstMov opPop opPush : optPushPop insts
optPushPop (InstPush _ : InstAdd (OpReg RegRsp) (OpImm 8) : insts) =
  optPushPop insts
optPushPop (inst : insts) = inst : optPushPop insts

optTailCall :: [Inst] -> [Inst]
optTailCall [] = []
optTailCall
  ( InstCall op
      : mov@(InstMov (OpReg RegRsp) (OpReg RegRbp))
      : pop@(InstPop (OpReg RegRbp))
      : InstRet
      : insts
    ) = mov : pop : InstJmp op : optTailCall insts
optTailCall (InstCall op : InstRet : insts) = InstJmp op : optTailCall insts
optTailCall (inst : insts) = inst : optTailCall insts

optimize :: [Inst] -> [Inst]
optimize = optTailCall . optPushPop

compileStrings :: [String] -> [String]
compileStrings =
  zipWith
    (printf "\t%s db %s,0" . labelString)
    [0 ..]
    . map (intercalate "," . map (show . ord))
    . reverse

program0 :: [Func]
program0 =
  [ Func
      "_entry_"
      []
      [ ExprAssign "x" (ExprCall (LabelFunc "f1") [ExprI64 $ -234]),
        ExprAssign "_" (ExprCall (LabelFunc "f3") []),
        ExprAssign "y" (ExprCall (LabelFunc "f4") []),
        ExprDrop
          ( ExprCall
              (LabelIntrin IntrinPrintf)
              [ExprStr "%d %d\n", ExprVar "x", ExprVar "y"]
          ),
        ExprAssign "s" (ExprStr "Hi there!"),
        ExprDrop
          (ExprCall (LabelIntrin IntrinPrintf) [ExprStr "%s\n", ExprVar "s"]),
        ExprI64 0
      ],
    Func "f1" ["x"] [ExprVar "x"],
    Func "f2" [] [ExprI64 456],
    Func
      "f3"
      []
      [ ExprDrop
          (ExprCall (LabelIntrin IntrinPrintf) [ExprStr "Hello, world!\n"]),
        ExprI64 0
      ],
    Func
      "f4"
      []
      [ ExprAssign "y" (ExprCall (LabelFunc "f6") []),
        ExprCall (LabelFunc "f5") [ExprVar "y", ExprI64 5]
      ],
    Func "f5" ["w", "z"] [ExprVar "w"],
    Func "f6" [] [ExprCall (LabelFunc "f2") []]
  ]

program1 :: [Func]
program1 =
  [ Func
      "_entry_"
      []
      [ ExprCall
          (LabelIntrin IntrinPrintf)
          [ ExprStr "%ld\n",
            ExprCall (LabelFunc "fib") [ExprI64 50, ExprI64 0, ExprI64 1]
          ]
      ],
    Func
      "fib"
      ["n", "a", "b"]
      [ ExprIf
          (ExprEq (ExprVar "n") (ExprI64 0))
          [ExprVar "a"]
          [ ExprCall
              (LabelFunc "fib")
              [ ExprSub (ExprVar "n") (ExprI64 1),
                ExprVar "b",
                ExprAdd (ExprVar "a") (ExprVar "b")
              ]
          ]
      ]
  ]

main :: IO ()
main =
  mapM_
    (mapM_ putStrLn)
    [ [ "format ELF64",
        "public _start",
        "extrn printf",
        "",
        "section '.data' writeable"
      ],
      compileStrings strings,
      [ "",
        "section '.text' executable",
        "_start:",
        "\tcall _entry_",
        "\tmov rdi, rax",
        "\tmov eax, 60",
        "\tsyscall"
      ],
      map show asm
    ]
  where
    (ContextFunc strings _, asm) = compileFuncs (ContextFunc [] 0) program1
