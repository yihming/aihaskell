module AbstractSyntax where

import Data.List

data Program = Program1 ProcDefList VarSection Body
             | Program2 ProcDefList Body
instance Show Program where
  show (Program1 proc_l  v_sec body) = show proc_l ++ "\n" ++ show v_sec ++ "\n" ++ show body
  show (Program2 proc_l body) = show proc_l ++ "\n" ++ show body

type Identifier = [Char]

data ProcDefList = ProcDefList [ProcDef]
instance Show ProcDefList where
  show (ProcDefList pl) = 
    if null pl
       then ""
       else concat $ map show pl

data ProcDef = Proc1 Identifier VarDeclList VarDeclList VarSection Body
             | Proc2 Identifier VarDeclList VarDeclList Body
instance Show ProcDef where
  show (Proc1 p_name params rets v_sec body) = "proc " ++ p_name ++ " (" ++ show params ++ ") returns (" ++ show rets ++ ")\n" ++ show v_sec ++ "\n" ++ show body ++ "\n"
  show (Proc2 p_name params rets body) = "proc " ++ p_name ++ " (" ++ show params ++ ") returns (" ++ show rets ++ ")\n" ++ show body ++ "\n"

data VarSection = VarSection VarDeclList
instance Show VarSection where
  show (VarSection vl) = "var " ++ show vl ++ ";\n"

data VarDeclList = VarList [VarDecl]
instance Show VarDeclList where
  show (VarList (v:vs)) = 
    if null vs
       then show v
       else concat (intersperse ", " $ (show v):[show vs])

data VarDecl = Var Identifier Type
instance Show VarDecl where
  show (Var v t) = v ++ " : " ++ show t

data Type = IntType | RealType
instance Show Type where
  show IntType = "int"
  show RealType = "real"

data Body = ProgramBody [Stmt]
instance Show Body where
  show (ProgramBody (s:ss)) = 
    if null ss
       then "begin\n" ++ show s ++ "end\n"
       else "begin\n" ++ (concat $ (show s):[show ss]) ++ "end\n"


data Stmt = Skip
          | Halt
          | Fail
          | Assume BoolExpr
          | Assign LHSExpr RHSExpr
          | If1 BoolExpr ThenBlock ElseBlock
          | If2 BoolExpr ThenBlock
          | While BoolExpr [Stmt]
instance Show Stmt where
  show Skip = "skip;\n"
  show Halt = "halt;\n"
  show Fail = "fail;\n"
  show (Assume be) = "assume " ++ show be ++ ";\n"
  show (Assign lhs rhs) = show lhs ++ " = " ++ show rhs ++ ";\n"
  show (If1 be s1 s2) = "if " ++ show be ++ " then\n" ++ show s1 ++ "\n else\n" ++ show s2 ++ "\n endif;\n"
  show (If2 be s) = "if " ++ show be ++ " then\n" ++ show s ++ " endif;\n"
  show (While be (s:ss)) = "while " ++ show be ++ " do\n" ++ (concat $ (show s):[show ss]) ++ "done;\n"

data LHSExpr = LHS Identifier
             | ParLHS [Identifier]
instance Show LHSExpr where
  show (LHS v) = v
  show (ParLHS vs) = 
    if null vs
       then "( )"
       else "( " ++ (concat (intersperse ", " vs)) ++ " )"

data RHSExpr = RandRHS
             | NumRHS NumExpr
             | ProcRHS ProcInvocation
instance Show RHSExpr where
  show RandRHS = "random"
  show (NumRHS ne) = show ne
  show (ProcRHS p) = show p

data ProcInvocation = Call Identifier ParamList
instance Show ProcInvocation where
  show (Call func params) = func ++ " ( " ++ show params ++ " )"

data ParamList = Params [Identifier]
instance Show ParamList where
  show (Params vs) = 
    if null vs
       then ""
       else concat (intersperse ", " vs)

data ThenBlock = Then [Stmt]
instance Show ThenBlock where
  show (Then (s:ss)) = 
    if null ss
       then show s
       else concat $ (show s):[show ss]

data ElseBlock = Else [Stmt]
instance Show ElseBlock where
  show (Else (s:ss)) = 
    if null ss
       then show s
       else concat $ (show s):[show ss]

data BoolExpr = Tru
              | Fls
              | Brandom
              | Constraint ConstraintExpr
              | Not BoolExpr
              | Or BoolExpr BoolExpr
              | And BoolExpr BoolExpr
              | ParBool BoolExpr
instance Show BoolExpr where
  show Tru = "true"
  show Fls = "false"
  show Brandom = "brandom"
  show (Constraint ce) = show ce
  show (Not be) = "not " ++ show be
  show (Or be1 be2) = show be1 ++ " or " ++ show be2
  show (And be1 be2) = show be1 ++ " and " ++ show be2
  show (ParBool be) = "( " ++ show be ++ " )"

data ConstraintExpr = Relop Relop NumExpr NumExpr
instance Show ConstraintExpr where
  show (Relop rop ne1 ne2) = show ne1 ++ " " ++ show rop ++ " " ++ show ne2 

data Relop = EQUAL | GEQ | GT_Keyword | LEQ | LT_Keyword
instance Show Relop where
  show EQUAL = "=="
  show GEQ = ">="
  show GT_Keyword = ">"
  show LEQ = "<="
  show LT_Keyword = "<"

data NumExpr = NumInt Integer
             | NumReal Double
             | RandNum
             | ID Identifier
             | Bop Bop NumExpr NumExpr
             | ParNum NumExpr
instance Show NumExpr where
  show (NumInt n) = show n
  show (NumReal f) = show f
  show RandNum = " random "
  show (ID v) = v
  show (Bop bop ne1 ne2) = show ne1 ++ " " ++ show bop ++ " " ++ show ne2
  show (ParNum ne) = "( " ++ show ne ++ " )"

data Bop = Add | Sub | Mul | Div | Mod
instance Show Bop where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Mod = "%"

