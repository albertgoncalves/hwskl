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
  | ExprCall Label [Expr]

data Reg
  = RegRDI
  | RegRSI
  | RegRDX
  | RegR10
  | RegR8
  | RegR9

instance Show Reg where
  show RegRDI = "rdi"
  show RegRSI = "rsi"
  show RegRDX = "rdx"
  show RegR10 = "r10"
  show RegR8 = "r8"
  show RegR9 = "r9"

type Local = String

type Asm = String

argRegs :: [Reg]
argRegs = [RegRDI, RegRSI, RegRDX, RegR10, RegR8, RegR9]

labelString :: Int -> String
labelString = printf "_s%d_"

compileExpr :: [Local] -> [String] -> Expr -> ([Local], [String], [Asm])
compileExpr locals strings (ExprI64 i) =
  (locals, strings, [printf "\tpush %d" i])
compileExpr locals strings (ExprStr s) =
  case elemIndex s strings of
    Nothing ->
      ( locals,
        s : strings,
        [printf "\tpush %s" $ labelString $ length strings]
      )
    Just n ->
      ( locals,
        strings,
        [printf "\tpush %s" $ labelString $ length strings - n]
      )
compileExpr locals strings (ExprVar v) =
  case elemIndex v locals of
    Nothing -> undefined
    Just n ->
      ( locals,
        strings,
        [printf "\tpush qword [rbp - %d]" $ (length locals - n) * 8]
      )
compileExpr locals0 strings0 (ExprAssign var expr) =
  (var : locals1, strings1, asm)
  where
    (locals1, strings1, asm) = compileExpr locals0 strings0 expr
compileExpr locals0 strings0 (ExprCall (LabelFunc label) exprs) =
  (locals1, strings1, asm ++ [printf "\tcall %s" label, "\tpush rax", ""])
  where
    (locals1, strings1, asm) = compileArgs argRegs locals0 strings0 exprs
compileExpr locals0 strings0 (ExprCall (LabelIntrin intrin) exprs) =
  (locals1, strings1, asm ++ compileIntrin intrin)
  where
    (locals1, strings1, asm) = compileArgs argRegs locals0 strings0 exprs

compileExprs :: [Local] -> [String] -> [Expr] -> ([Local], [String], [Asm])
compileExprs locals strings [] = (locals, strings, [])
compileExprs locals0 strings0 (expr : exprs) =
  (locals2, strings2, asm1 ++ asm2)
  where
    (locals2, strings2, asm2) = compileExprs locals1 strings1 exprs
    (locals1, strings1, asm1) = compileExpr locals0 strings0 expr

compileIntrin :: Intrin -> [Asm]
compileIntrin IntrinPrintf = ["\txor eax, eax", "\tcall printf", ""]

compileArgs ::
  [Reg] -> [Local] -> [String] -> [Expr] -> ([Local], [String], [Asm])
compileArgs [] _ _ _ = undefined
compileArgs _ locals strings [] = (locals, strings, [])
compileArgs (reg : regs) locals0 strings0 (expr : exprs) =
  (locals2, strings2, asm0 ++ [printf "\tpop %s" $ show reg] ++ asm1)
  where
    (locals1, strings1, asm0) = compileExpr locals0 strings0 expr
    (locals2, strings2, asm1) = compileArgs regs locals1 strings1 exprs

compileFunc :: [String] -> String -> [Expr] -> ([String], [Asm])
compileFunc strings0 label exprs =
  ( strings1,
    [ "",
      printf "%s:" label
    ]
      ++ [ "\tpush rbp",
           "\tmov rbp, rsp",
           ""
         ]
      ++ asm
      ++ [ "\tpop rax",
           "",
           "\tmov rsp, rbp",
           "\tpop rbp",
           "\tret"
         ]
  )
  where
    (_, strings1, asm) = compileExprs [] strings0 exprs

compileFuncs :: [String] -> [(String, [Expr])] -> ([String], [Asm])
compileFuncs strings [] = (strings, [])
compileFuncs strings0 ((label, exprs) : funcs) = (strings2, ast1 ++ ast2)
  where
    (strings2, ast2) = compileFuncs strings1 funcs
    (strings1, ast1) = compileFunc strings0 label exprs

compileStrings :: [String] -> [Asm]
compileStrings =
  zipWith
    (printf "\t%s db %s,0" . labelString)
    [0 ..]
    . map (intercalate "," . map (show . ord))
    . reverse

ast :: [(String, [Expr])]
ast =
  [ ( "_entry_",
      [ ExprAssign "x" (ExprCall (LabelFunc "f1") []),
        ExprAssign "_" (ExprCall (LabelFunc "f3") []),
        ExprAssign "y" (ExprCall (LabelFunc "f2") []),
        ExprCall
          (LabelIntrin IntrinPrintf)
          [ExprStr "%d %d\n", ExprVar "x", ExprVar "y"],
        ExprAssign "s" (ExprStr "Hi there!"),
        ExprCall (LabelIntrin IntrinPrintf) [ExprStr "%s\n", ExprVar "s"],
        ExprI64 0
      ]
    ),
    ("f1", [ExprI64 $ -234]),
    ("f2", [ExprI64 456]),
    ( "f3",
      [ ExprCall (LabelIntrin IntrinPrintf) [ExprStr "Hello, world!\n"],
        ExprI64 0
      ]
    )
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
      asm
    ]
  where
    (strings, asm) = compileFuncs [] ast