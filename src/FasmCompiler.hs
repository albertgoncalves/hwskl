{-# LANGUAGE OverloadedStrings #-}

import Data.Char (ord)
import Data.List (elemIndex, foldl', intercalate)
import Data.Text (Text, pack, unpack)
import Data.Text.IO (putStrLn)
import Text.Printf (printf)
import Prelude hiding (putStrLn)

data Intrin
  = IntrinPrintf

data Label
  = LabelFunc Text
  | LabelIntrin Intrin

data Expr
  = ExprI64 Int
  | ExprVar Text
  | ExprStr Text
  | ExprAssign Text Expr
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

type Local = Text

type Asm = Text

argRegs :: [Reg]
argRegs = [RegRDI, RegRSI, RegRDX, RegR10, RegR8, RegR9]

labelString :: Int -> Text
labelString = pack . printf "_s%d_"

compile :: [Local] -> [Text] -> Expr -> ([Local], [Text], [Asm])
compile locals strings (ExprI64 i) =
  (locals, strings, [pack $ printf "\tpush %d" i])
compile locals strings (ExprStr s) =
  case elemIndex s strings of
    Nothing ->
      ( locals,
        s : strings,
        [pack $ printf "\tpush %s" $ labelString $ length strings]
      )
    Just n ->
      ( locals,
        strings,
        [pack $ printf "\tpush %s" $ labelString $ length strings - n]
      )
compile locals strings (ExprVar v) =
  case elemIndex v locals of
    Nothing -> undefined
    Just n ->
      ( locals,
        strings,
        [pack $ printf "\tpush qword [rbp - %d]" $ (length locals - n) * 8]
      )
compile locals0 strings0 (ExprAssign var expr) = (var : locals1, strings1, asm)
  where
    (locals1, strings1, asm) = compile locals0 strings0 expr
compile locals0 strings0 (ExprCall (LabelFunc label) exprs) =
  (locals1, strings1, "" : pack (printf "\tcall %s" label) : asm)
  where
    (locals1, strings1, asm) = compileArgs argRegs locals0 strings0 exprs
compile locals0 strings0 (ExprCall (LabelIntrin intrin) exprs) =
  (locals1, strings1, compileIntrin intrin ++ asm)
  where
    (locals1, strings1, asm) = compileArgs argRegs locals0 strings0 exprs

compileIntrin :: Intrin -> [Asm]
compileIntrin IntrinPrintf = ["", "\tcall printf", "\txor eax, eax"]

compileArgs ::
  [Reg] -> [Local] -> [Text] -> [Expr] -> ([Local], [Text], [Asm])
compileArgs [] _ _ _ = undefined
compileArgs _ locals strings [] = (locals, strings, [])
compileArgs (reg : regs) locals0 strings0 (expr : exprs) =
  (locals2, strings2, asm1 ++ [pack $ printf "\tpop %s" $ show reg] ++ asm0)
  where
    (locals1, strings1, asm0) = compile locals0 strings0 expr
    (locals2, strings2, asm1) = compileArgs regs locals1 strings1 exprs

dataHeader :: [Asm]
dataHeader = ["section '.data' writeable"]

compileStrings :: [Text] -> [Asm]
compileStrings =
  zipWith
    ((pack .) . (printf "\t%s db %s,0" . labelString))
    [0 ..]
    . map (pack . intercalate "," . map (show . ord) . unpack)
    . reverse

fileHeader :: [Asm]
fileHeader =
  [ "format ELF64",
    "public _start",
    "extrn printf"
  ]

funcHeader :: [Asm]
funcHeader =
  [ "\tpush rbp",
    "\tmov rbp, rsp",
    ""
  ]

funcFooter :: [Asm]
funcFooter =
  [ "\tmov rsp, rbp",
    "\tpop rbp",
    ""
  ]

instHeader :: [Asm]
instHeader =
  [ "section '.text' executable",
    "_start:",
    ""
  ]

instFooter :: [Asm]
instFooter =
  [ "\txor edi, edi",
    "\tmov eax, 60",
    "\tsyscall"
  ]

compileAll :: [Expr] -> [Asm]
compileAll exprs =
  foldr1
    (++)
    [ fileHeader,
      [""],
      dataHeader,
      compileStrings strings,
      [""],
      instHeader,
      funcHeader,
      reverse asm,
      funcFooter,
      instFooter
    ]
  where
    f :: ([Local], [Text], [Asm]) -> Expr -> ([Local], [Text], [Asm])
    f (locals0, strings0, asm0) expr = (locals1, strings1, asm1 ++ asm0)
      where
        (locals1, strings1, asm1) = compile locals0 strings0 expr
    (_, strings, asm) = foldl' f ([], [], []) exprs

ast :: [Expr]
ast =
  [ ExprAssign "x" (ExprI64 $ -123),
    ExprCall (LabelIntrin IntrinPrintf) [ExprStr "Hello, world!\n"],
    ExprAssign "y" (ExprI64 456),
    ExprCall
      (LabelIntrin IntrinPrintf)
      [ExprStr "%d %d\n", ExprVar "x", ExprVar "y"],
    ExprAssign "s" (ExprStr "Hi there!"),
    ExprCall (LabelIntrin IntrinPrintf) [ExprStr "%s\n", ExprVar "s"]
  ]

main :: IO ()
main = mapM_ putStrLn $ compileAll ast
