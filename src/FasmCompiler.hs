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

data Func = Func String [String] [Expr]

type Local = String

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

argRegs :: [Reg]
argRegs = [RegRdi, RegRsi, RegRdx, RegR10, RegR8, RegR9]

labelString :: Int -> String
labelString = printf "_s%d_"

compileExpr :: [Local] -> [String] -> Expr -> ([Local], [String], [Inst])
compileExpr locals strings (ExprI64 i) =
  (locals, strings, [InstPush (OpImm i)])
compileExpr locals strings (ExprStr s) =
  case elemIndex s strings of
    Nothing ->
      ( locals,
        s : strings,
        [InstPush (OpLabel $ labelString $ length strings)]
      )
    Just n ->
      ( locals,
        strings,
        [InstPush (OpLabel $ labelString $ length strings - n)]
      )
compileExpr locals strings (ExprVar v) =
  case elemIndex v locals of
    Nothing -> undefined
    Just n ->
      ( locals,
        strings,
        [InstPush (OpDeref RegRbp $ negate $ (length locals - n) * 8)]
      )
compileExpr locals0 strings0 (ExprAssign var expr) =
  (var : locals1, strings1, asm)
  where
    (locals1, strings1, asm) = compileExpr locals0 strings0 expr
compileExpr _ _ (ExprUpdate _ _) = undefined
compileExpr locals0 strings0 (ExprCall (LabelFunc label) exprs) =
  ( locals1,
    strings1,
    asm ++ [InstCall (OpLabel label), InstPush (OpReg RegRax)]
  )
  where
    (locals1, strings1, asm) = compileCallArgs argRegs locals0 strings0 exprs
compileExpr locals0 strings0 (ExprCall (LabelIntrin intrin) exprs) =
  (locals1, strings1, asm ++ compileIntrin intrin)
  where
    (locals1, strings1, asm) = compileCallArgs argRegs locals0 strings0 exprs

compileExprs :: [Local] -> [String] -> [Expr] -> ([Local], [String], [Inst])
compileExprs locals strings [] = (locals, strings, [])
compileExprs locals0 strings0 (expr : exprs) =
  (locals2, strings2, asm1 ++ asm2)
  where
    (locals2, strings2, asm2) = compileExprs locals1 strings1 exprs
    (locals1, strings1, asm1) = compileExpr locals0 strings0 expr

compileIntrin :: Intrin -> [Inst]
compileIntrin IntrinPrintf =
  [InstXor (OpReg RegEax) (OpReg RegEax), InstCall (OpLabel "printf")]

compileCallArgs ::
  [Reg] -> [Local] -> [String] -> [Expr] -> ([Local], [String], [Inst])
compileCallArgs [] _ _ _ = undefined
compileCallArgs _ locals strings [] = (locals, strings, [])
compileCallArgs (reg : regs) locals0 strings0 (expr : exprs) =
  (locals2, strings2, asm0 ++ [InstPop (OpReg reg)] ++ asm1)
  where
    (locals1, strings1, asm0) = compileExpr locals0 strings0 expr
    (locals2, strings2, asm1) = compileCallArgs regs locals1 strings1 exprs

compileFuncArgs :: [Reg] -> [String] -> [Inst]
compileFuncArgs [] _ = undefined
compileFuncArgs _ [] = []
compileFuncArgs (reg : regs) (_ : args) =
  InstPush (OpReg reg) : compileFuncArgs regs args

compileFunc :: [String] -> String -> [String] -> [Expr] -> ([String], [Inst])
compileFunc strings0 label args exprs =
  ( strings1,
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
    (_, strings1, asm) = compileExprs (reverse args) strings0 exprs
    needStack = not (null args) || any isAssignOrUpdate exprs

isAssignOrUpdate :: Expr -> Bool
isAssignOrUpdate (ExprAssign _ _) = True
isAssignOrUpdate (ExprUpdate _ _) = True
isAssignOrUpdate _ = False

compileFuncs :: [String] -> [Func] -> ([String], [Inst])
compileFuncs strings [] = (strings, [])
compileFuncs strings0 ((Func label args exprs) : funcs) =
  (strings2, ast1 ++ ast2)
  where
    (strings2, ast2) = compileFuncs strings1 funcs
    (strings1, ast1) = compileFunc strings0 label args exprs

optPushPop :: [Inst] -> [Inst]
optPushPop [] = []
optPushPop (InstPush opPush : InstPop opPop : insts)
  | opPush == opPop = optPushPop insts
  | otherwise = InstMov opPop opPush : optPushPop insts
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

ast :: [Func]
ast =
  [ Func
      "_entry_"
      []
      [ ExprAssign "x" (ExprCall (LabelFunc "f1") [ExprI64 $ -234]),
        ExprAssign "_" (ExprCall (LabelFunc "f3") []),
        ExprAssign "y" (ExprCall (LabelFunc "f4") []),
        ExprCall
          (LabelIntrin IntrinPrintf)
          [ExprStr "%d %d\n", ExprVar "x", ExprVar "y"],
        ExprAssign "s" (ExprStr "Hi there!"),
        ExprCall (LabelIntrin IntrinPrintf) [ExprStr "%s\n", ExprVar "s"],
        ExprI64 0
      ],
    Func "f1" ["x"] [ExprVar "x"],
    Func "f2" [] [ExprI64 456],
    Func
      "f3"
      []
      [ ExprCall (LabelIntrin IntrinPrintf) [ExprStr "Hello, world!\n"],
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
    (strings, asm) = compileFuncs [] ast
