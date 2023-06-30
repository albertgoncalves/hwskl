import Data.Bifunctor (first)
import Data.List (intercalate, unfoldr)
import qualified Data.Map as M
import Text.Printf (printf)

data Inst
  = InstRet
  | InstLabel String
  | InstJmp String
  | InstJz String
  | InstLoad String
  | InstStore String
  | InstPush Int
  | InstEq
  | InstLt
  | InstAnd
  | InstAdd

instance Show Inst where
  show InstRet = "        ret"
  show (InstLabel label) = printf "    %s:" label
  show (InstJmp label) = printf "        jmp   %s" label
  show (InstJz label) = printf "        jz    %s" label
  show (InstLoad var) = printf "        load  %s" var
  show (InstStore var) = printf "        store %s" var
  show (InstPush int) = printf "        push  %d" int
  show InstEq = "        eq"
  show InstLt = "        lt"
  show InstAnd = "        and"
  show InstAdd = "        add"

data Expr
  = ExprIdent String
  | ExprInt Int
  | ExprFunc String [Expr]

instance Show Expr where
  show (ExprIdent ident) = ident
  show (ExprInt int) = show int
  show (ExprFunc label exprs) =
    printf "%s(%s)" label $ intercalate ", " $ map show exprs

data Asm
  = AsmRet
  | AsmLabel String
  | AsmMov AsmArg AsmArg
  | AsmJmp String
  | AsmJnz String
  | AsmJge String
  | AsmTest AsmArg AsmArg
  | AsmCmp AsmArg AsmArg
  | AsmAnd AsmArg AsmArg
  | AsmAdd AsmArg AsmArg

instance Show Asm where
  show AsmRet = "        ret"
  show (AsmLabel label) = printf "    %s:" label
  show (AsmMov arg0 arg1) = printf "        mov %s, %s" (show arg0) (show arg1)
  show (AsmJmp label) = printf "        jmp %s" label
  show (AsmJnz label) = printf "        jnz %s" label
  show (AsmJge label) = printf "        jge %s" label
  show (AsmTest arg0 arg1) =
    printf "        test %s, %s" (show arg0) (show arg1)
  show (AsmCmp arg0 arg1) = printf "        cmp %s, %s" (show arg0) (show arg1)
  show (AsmAnd arg0 arg1) = printf "        and %s, %s" (show arg0) (show arg1)
  show (AsmAdd arg0 arg1) = printf "        add %s, %s" (show arg0) (show arg1)

data AsmArg
  = AsmArgLabel String
  | AsmArgReg AsmReg
  | AsmArgAddr AsmArg Int
  | AsmArgInt Int

instance Show AsmArg where
  show (AsmArgLabel label) = label
  show (AsmArgReg reg) = show reg
  show (AsmArgAddr arg offset)
    | 0 < offset = printf "qword [%s + %d]" (show arg) offset
    | offset < 0 = printf "qword [%s - %d]" (show arg) $ -offset
    | otherwise = printf "qword [%s]" (show arg)
  show (AsmArgInt int) = show int

data AsmReg
  = AsmRegRdi
  | AsmRegR8

instance Show AsmReg where
  show AsmRegRdi = "rdi"
  show AsmRegR8 = "r8"

intoExprs2 :: [Inst] -> Maybe ((Expr, Expr), [Inst])
intoExprs2 insts0 =
  case intoExpr insts0 of
    Just (expr1, insts1) ->
      case intoExpr insts1 of
        Just (expr2, insts2) -> Just ((expr1, expr2), insts2)
        Nothing -> undefined
    Nothing -> undefined

intoExpr :: [Inst] -> Maybe (Expr, [Inst])
intoExpr [] = Nothing
intoExpr (InstRet : insts) = Just (ExprFunc "ret" [], insts)
intoExpr (InstLabel label : insts) =
  Just (ExprFunc "label" [ExprIdent label], insts)
intoExpr (InstJmp label : insts) =
  Just (ExprFunc "jmp" [ExprIdent label], insts)
intoExpr (InstJz label : insts) =
  first (\expr -> ExprFunc "jz" [ExprIdent label, expr]) <$> intoExpr insts
intoExpr (InstLoad var : insts) = Just (ExprFunc "load" [ExprIdent var], insts)
intoExpr (InstStore var : insts) =
  first (\expr -> ExprFunc "store" [ExprIdent var, expr]) <$> intoExpr insts
intoExpr (InstPush int : insts) = Just (ExprInt int, insts)
intoExpr (InstEq : insts) =
  first (\(expr0, expr1) -> ExprFunc "eq" [expr1, expr0]) <$> intoExprs2 insts
intoExpr (InstLt : insts) =
  first (\(expr0, expr1) -> ExprFunc "lt" [expr1, expr0]) <$> intoExprs2 insts
intoExpr (InstAnd : insts) =
  first (\(expr0, expr1) -> ExprFunc "and" [expr1, expr0]) <$> intoExprs2 insts
intoExpr (InstAdd : insts) =
  first (\(expr0, expr1) -> ExprFunc "add" [expr1, expr0]) <$> intoExprs2 insts

allocAsmReg :: [AsmReg] -> AsmArg -> (AsmArg, [Asm], [AsmReg])
allocAsmReg (reg : regs) arg0@(AsmArgAddr _ _) =
  (arg1, [AsmMov arg1 arg0], regs)
  where
    arg1 = AsmArgReg reg
allocAsmReg regs arg = (arg, [], regs)

intoAsmArg ::
  M.Map String AsmReg -> [AsmReg] -> Expr -> (AsmArg, [Asm], [AsmReg])
intoAsmArg vars regs (ExprFunc "load" [ExprIdent var]) =
  (AsmArgAddr (AsmArgReg reg) 0, [], regs)
  where
    reg = (M.!) vars var
intoAsmArg _ regs (ExprInt int) = (AsmArgInt int, [], regs)
intoAsmArg vars regs0 (ExprFunc "and" [expr0, expr2]) =
  (arg2, asm1 ++ asm2 ++ asm3 ++ [AsmAnd arg2 arg3], regs3)
  where
    (arg1, asm1, regs1) = intoAsmArg vars regs0 expr0
    (arg2, asm2, regs2) = allocAsmReg regs1 arg1
    (arg3, asm3, regs3) = intoAsmArg vars regs2 expr2
intoAsmArg _ _ _ = undefined

intoAsm :: M.Map String AsmReg -> [AsmReg] -> Expr -> [Asm]
intoAsm _ _ (ExprFunc "ret" []) = [AsmRet]
intoAsm _ _ (ExprFunc "label" [ExprIdent label]) = [AsmLabel label]
intoAsm _ _ (ExprFunc "jmp" [ExprIdent label]) = [AsmJmp label]
intoAsm
  vars
  regs0
  (ExprFunc "jz" [ExprIdent label, ExprFunc "lt" [expr0, expr1]]) =
    asm1 ++ asm2 ++ [AsmCmp arg1 arg2, AsmJge label]
    where
      (arg1, asm1, regs1) = intoAsmArg vars regs0 expr0
      (arg2, asm2, _) = intoAsmArg vars regs1 expr1
intoAsm
  vars
  regs
  (ExprFunc "jz" [ExprIdent label, ExprFunc "eq" [expr, ExprInt 0]]) =
    asm ++ [AsmTest arg arg, AsmJnz label]
    where
      (arg, asm, _) = intoAsmArg vars regs expr
intoAsm
  vars
  regs
  ( ExprFunc
      "store"
      [ExprIdent var0, ExprFunc "add" [ExprFunc "load" [ExprIdent var1], expr]]
    )
    | var0 == var1 =
        asm ++ [AsmAdd (AsmArgAddr (AsmArgReg $ (M.!) vars var0) 0) arg]
    | otherwise = undefined
    where
      (arg, asm, _) = intoAsmArg vars regs expr
intoAsm _ _ _ = undefined

main :: IO ()
main = do
  putChar '\n'
  mapM_ print insts

  putChar '\n'
  mapM_ print exprs

  putChar '\n'
  mapM_ print asm
  where
    insts :: [Inst]
    insts =
      [ InstLabel "while_start",
        InstLoad "x",
        InstPush 1000,
        InstLt,
        InstJz "while_end",
        InstLoad "x",
        InstPush 1,
        InstAnd,
        InstPush 0,
        InstEq,
        InstJz "if_else",
        InstLoad "x",
        InstPush 29,
        InstAdd,
        InstStore "x",
        InstJmp "if_end",
        InstLabel "if_else",
        InstLoad "x",
        InstPush $ -3,
        InstAdd,
        InstStore "x",
        InstLabel "if_end",
        InstJmp "while_start",
        InstLabel "while_end",
        InstRet
      ]
    exprs = reverse $ unfoldr intoExpr $ reverse insts
    asm = concatMap (intoAsm (M.singleton "x" AsmRegRdi) [AsmRegR8]) exprs
