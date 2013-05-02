module AbstractSyntax where

import Data.List
import qualified HaskellPPL as HP

showSepBy :: Show a => String -> [a] -> String
showSepBy s l = 
  concat $ intersperse s (map show l)

type Var = [Char]

data Program = Program VarSection Body

instance Show Program where
  show (Program vs b) = show vs ++ show b

data VarSection = VarSection [VarDecl]

instance Show VarSection where
  show (VarSection v_list) = "var " ++ (showSepBy ", " v_list) ++ ";\n"
            
data VarDecl = VarDecl Var TypeKeyword 

instance Show VarDecl where
  show (VarDecl v t) = v ++ " : " ++ show t

data TypeKeyword = IntType | RealType

instance Show TypeKeyword where
  show IntType = "int"
  show RealType = "real"

data Body = Body [Stmt]

instance Show Body where
  show (Body stmt_l) = "begin\n  " ++ (showSepBy "  " stmt_l) ++ "end"

data Stmt = Skip
          | Halt
          | Fail  
          | Assign Var RHSExpr  
          | While BoolExpr [Stmt]  
          | Assert HP.LinConSys
          | Comment String  
instance Show Stmt where
  show Skip = "skip;\n"
  show Halt = "halt;\n"
  show Fail = "fail;\n"
  show (Assign lhs rhs) = lhs ++ " = " ++ show rhs ++ ";\n"
  show (While be ss) = 
    if length ss == 1
       then "while " ++ show be ++ " do\n  " ++ (showSepBy "" ss) ++ "done;\n"
       else "while " ++ show be ++ " do\n" ++ (showSepBy "  " ss) ++ "done;\n"
  show (Assert cs) = 
    if null cs
       then "/* ⊤  */\n"
       else if cs == ([ 0 HP.%== 1] :: HP.LinConSys)
               then "/* ⊥  */\n"
               else "/* " ++ show cs ++ " */\n"     
  show (Comment str) = 
    "/** " ++ str ++ "**/\n"

data RHSExpr = NumRHS NumExpr
instance Show RHSExpr where
  show (NumRHS ne) = show ne
  
data BoolExpr = Tru
              | Fls
              | And ConstraintExpr ConstraintExpr
              | Constraint ConstraintExpr 
instance Show BoolExpr where
  show Tru = "true"
  show Fls = "false"
  show (Constraint ce) = show ce
  show (And ce1 ce2) = show ce1 ++ " and " ++ show ce2

data ConstraintExpr = Relop Relop NumExpr NumExpr
instance Show ConstraintExpr where
  show (Relop rop ne1 ne2) = "(" ++ show ne1 ++ " " ++ show rop ++ " " ++ show ne2 ++ ")"

data Relop = EQUAL | GEQ | GT_Keyword | LEQ | LT_Keyword
instance Show Relop where
  show EQUAL = "=="
  show GEQ = ">="
  show GT_Keyword = ">"
  show LEQ = "<="
  show LT_Keyword = "<"
  
data NumExpr = NumInt Integer
             | Var Var
             | Bop Bop NumExpr NumExpr  

instance Show NumExpr where
  show (NumInt n) = show n
  show (Var v) = v
  show (Bop bop ne1 ne2) = "(" ++ show ne1 ++ " " ++ show bop ++ " " ++ show ne2 ++ ")"
data Bop = Add | Sub | Mul | Div | Mod
instance Show Bop where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Mod = "%"