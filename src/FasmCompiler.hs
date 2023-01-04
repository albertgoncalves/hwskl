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
  | InstTest Op Op
  | InstJe Op
  | InstAdd Op Op
  | InstSub Op Op
  | InstLeave
  | InstDrop

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
  show (InstTest op0 op1) = printf "\ttest %s, %s" (show op0) (show op1)
  show (InstJe op) = printf "\tje %s" (show op)
  show (InstAdd op0 op1) = printf "\tadd %s, %s" (show op0) (show op1)
  show (InstSub op0 op1) = printf "\tsub %s, %s" (show op0) (show op1)
  show InstDrop = "\tadd rsp, 8"
  show InstLeave = "\tleave"

data ContextExpr = ContextExpr ContextFunc [String] Bool

data ContextFunc = ContextFunc [String] Int

argRegs :: [Reg]
argRegs = [RegRdi, RegRsi, RegRdx, RegRcx, RegR8, RegR9]

labelString :: Int -> String
labelString = printf "_s%d_"

isRet :: [Expr] -> Bool
isRet [] = False
isRet [ExprRet _] = True
isRet (ExprRet _ : _) = undefined
isRet (_ : exprs) = isRet exprs

compileExpr :: ContextExpr -> Expr -> (ContextExpr, [Inst])
compileExpr context0 (ExprDrop expr0) =
  (context1, expr1 ++ [InstDrop])
  where
    (context1, expr1) = compileExpr context0 expr0
compileExpr context0@(ContextExpr _ _ needStack) (ExprRet expr0) =
  ( context1,
    expr1 ++ [InstPop (OpReg RegRax)] ++ [InstLeave | needStack] ++ [InstRet]
  )
  where
    (context1, expr1) = compileExpr context0 expr0
compileExpr context0 (ExprAdd l0 (ExprI64 r)) =
  ( context1,
    l1
      ++ [ InstPop (OpReg RegR10),
           InstAdd (OpReg RegR10) (OpImm r),
           InstPush (OpReg RegR10)
         ]
  )
  where
    (context1, l1) = compileExpr context0 l0
compileExpr context0 (ExprAdd l0 r1) =
  ( context2,
    l1
      ++ [InstPop (OpReg RegR10)]
      ++ r2
      ++ [ InstPop (OpReg RegR11),
           InstAdd (OpReg RegR10) (OpReg RegR11),
           InstPush (OpReg RegR10)
         ]
  )
  where
    (context1, l1) = compileExpr context0 l0
    (context2, r2) = compileExpr context1 r1
compileExpr context0 (ExprSub l0 (ExprI64 r)) =
  ( context1,
    l1
      ++ [ InstPop (OpReg RegR10),
           InstSub (OpReg RegR10) (OpImm r),
           InstPush (OpReg RegR10)
         ]
  )
  where
    (context1, l1) = compileExpr context0 l0
compileExpr context0 (ExprSub l0 r1) =
  ( context2,
    l1
      ++ [InstPop (OpReg RegR10)]
      ++ r2
      ++ [ InstPop (OpReg RegR11),
           InstSub (OpReg RegR10) (OpReg RegR11),
           InstPush (OpReg RegR10)
         ]
  )
  where
    (context1, l1) = compileExpr context0 l0
    (context2, r2) = compileExpr context1 r1
compileExpr _ (ExprEq _ _) = undefined
compileExpr
  (ContextExpr (ContextFunc strings0 k0) locals0 needStack0)
  (ExprIf ifCondition0 ifThen1 ifElse2)
    | thenReturns && elseReturns =
        ( context3,
          ifCondition1 ++ ifElse3 ++ [InstLabel labelThen] ++ ifThen2
        )
    | thenReturns || elseReturns = undefined
    | otherwise =
        ( context3,
          ifCondition1
            ++ ifElse3
            ++ [InstJmp (OpLabel labelEnd), InstLabel labelThen]
            ++ ifThen2
            ++ [InstLabel labelEnd]
        )
    where
      thenReturns = isRet ifThen1
      elseReturns = isRet ifElse2
      (context1, ifCondition1) =
        compileIfCondition
          (ContextExpr (ContextFunc strings0 (succ k0)) locals0 needStack0)
          labelThen
          ifCondition0
      (context2, ifThen2) = compileExprs context1 ifThen1
      (context3, ifElse3) = compileExprs context2 ifElse2
      labelThen :: String
      labelThen = printf "_then%d_" k0
      labelEnd :: String
      labelEnd = printf "_end%d_" k0
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

compileIfCondition :: ContextExpr -> String -> Expr -> (ContextExpr, [Inst])
compileIfCondition context0 label (ExprEq l0 (ExprI64 0)) =
  ( context1,
    l1
      ++ [ InstPop (OpReg RegR10),
           InstTest (OpReg RegR10) (OpReg RegR10),
           InstJe (OpLabel label)
         ]
  )
  where
    (context1, l1) = compileExpr context0 l0
compileIfCondition context0 label (ExprEq (ExprI64 0) r0) =
  ( context1,
    r1
      ++ [ InstPop (OpReg RegR10),
           InstTest (OpReg RegR10) (OpReg RegR10),
           InstJe (OpLabel label)
         ]
  )
  where
    (context1, r1) = compileExpr context0 r0
compileIfCondition context0 label (ExprEq l0 r1) =
  ( context2,
    l1
      ++ [InstPop (OpReg RegR10)]
      ++ r2
      ++ [ InstPop (OpReg RegR11),
           InstCmp (OpReg RegR10) (OpReg RegR11),
           InstJe (OpLabel label)
         ]
  )
  where
    (context1, l1) = compileExpr context0 l0
    (context2, r2) = compileExpr context1 r1
compileIfCondition _ _ _ = undefined

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
returnLast [] = undefined
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
optPushPop (InstPush _ : InstDrop : insts) = optPushPop insts
optPushPop (inst : insts) = inst : optPushPop insts

optTailCall :: [Inst] -> [Inst]
optTailCall [] = []
optTailCall (InstCall op : InstLeave : InstRet : insts) =
  InstLeave : InstJmp op : optTailCall insts
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
        ExprDrop $
          ExprCall
            (LabelIntrin IntrinPrintf)
            [ExprStr "%d %d\n", ExprVar "x", ExprVar "y"],
        ExprAssign "s" (ExprStr "Hi there!"),
        ExprDrop $
          ExprCall (LabelIntrin IntrinPrintf) [ExprStr "%s\n", ExprVar "s"],
        ExprI64 0
      ],
    Func "f1" ["x"] [ExprVar "x"],
    Func "f2" [] [ExprI64 456],
    Func
      "f3"
      []
      [ ExprDrop $
          ExprCall (LabelIntrin IntrinPrintf) [ExprStr "Hello, world!\n"],
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

program2 :: [Func]
program2 =
  [ Func
      "_entry_"
      []
      [ ExprDrop $
          ExprCall
            (LabelIntrin IntrinPrintf)
            [ExprStr "%ld\n", ExprCall (LabelFunc "is_even") [ExprI64 456789]],
        ExprI64 0
      ],
    Func
      "is_even"
      ["n"]
      [ ExprIf
          (ExprEq (ExprVar "n") (ExprI64 0))
          [ExprI64 1]
          [ExprCall (LabelFunc "is_odd") [ExprSub (ExprVar "n") (ExprI64 1)]]
      ],
    Func
      "is_odd"
      ["n"]
      [ ExprIf
          (ExprEq (ExprVar "n") (ExprI64 0))
          [ExprI64 0]
          [ExprCall (LabelFunc "is_even") [ExprSub (ExprVar "n") (ExprI64 1)]]
      ]
  ]

program3 :: [Func]
program3 =
  [ Func
      "_entry_"
      []
      [ ExprAssign
          "x"
          ( ExprIf
              (ExprEq (ExprI64 0) (ExprI64 1))
              [ExprIf (ExprEq (ExprI64 1) (ExprI64 0)) [ExprI64 1] [ExprI64 2]]
              [ ExprIf
                  (ExprEq (ExprI64 0) (ExprI64 1))
                  [ ExprIf
                      (ExprEq (ExprI64 0) (ExprI64 1))
                      [ExprI64 3]
                      [ExprI64 5]
                  ]
                  [ExprI64 4]
              ]
          ),
        ExprDrop $
          ExprCall (LabelIntrin IntrinPrintf) [ExprStr "%ld\n", ExprVar "x"],
        ExprI64 0
      ]
  ]

program4 :: [Func]
program4 =
  [ Func
      "_entry_"
      []
      [ ExprDrop $
          ExprCall
            (LabelIntrin IntrinPrintf)
            [ ExprStr "%ld %ld %ld %ld\n",
              ExprI64 1,
              ExprI64 $ -2,
              ExprI64 3,
              ExprI64 $ -4
            ],
        ExprI64 0
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
        "section '.rodata'"
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
    (ContextFunc strings _, asm) = compileFuncs (ContextFunc [] 0) program4
