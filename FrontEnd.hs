module FrontEnd where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token


import AbstractSyntax as S

keywords :: [String]
keywords = ["proc", "returns", "var", "begin", "end", "int", "real",
            "skip", "halt", "fail", "assume", "random", "if", "then",
            "else", "endif", "while", "do", "done", "true", "false",
            "brandom", "not", "or", "and"]

symbols :: String
symbols = ":()+-*/%=><;"

interprocDef :: LanguageDef st
interprocDef = interprocDef
               {commentStart = "/*",
                commentEnd = "*/",
                nestedComments = False,
                commentLine = "//",
                reservedNames = keywords,
                caseSensitive = True,
                identStart = letter <|> char '_',
                identLetter = alphaNum <|> char '_',
                opStart = oneOf symbols,
                opLetter = oneOf symbols
               }

interproc :: TokenParser st
interproc = makeTokenParser interprocDef

-- This is the main function of Front-end.
parseInterproc :: String -> S.Program
parseInterproc input = 
  case parse parseProg "" input of 
    Left err -> error $ "Syntax Error: " ++ show err
    Right val -> val

parseProg = prog1 <|> prog2

prog1 = do
  proc_l <- procDefList
  v_sec <- varSection
  b <- parseBody
  return (S.Program1 proc_l v_sec b)
  
prog2 = do
  proc_l <- procDefList
  b <- parseBody
  return (S.Program2 proc_l b)
  
procDefList = do
  procs <- many procDef
  return (S.ProcDefList procs)
procDef = proc1 <|> proc2

proc1 = do
  symbol interproc "proc"
  name <- identifier interproc
  paramList <- between (symbol interproc "(") (symbol interproc ")") varDeclList  
  symbol interproc "returns"
  retList <- between (symbol interproc "(") (symbol interproc ")") varDeclList
  v_sec <- varSection
  b <- parseBody
  return (S.Proc1 name paramList retList v_sec b)
  
proc2 = do
  symbol interproc "proc"  
  name <- identifier interproc
  paramList <- between (symbol interproc "(") (symbol interproc ")") varDeclList
  symbol interproc "returns"
  retList <- between (symbol interproc "(") (symbol interproc ")") varDeclList
  b <- parseBody
  return (S.Proc2 name paramList retList b)
  
varDeclList = do
  symbol interproc "var"  
  decl_list <- varDecl `sepBy` (symbol interproc ",")
  symbol interproc ";"
  return (S.VarList decl_list)
  
varDecl = do  
  name <- identifier interproc  
  t <- parseType
  return (S.Var name t)
  
parseType = (do
  symbol interproc "int"
  return S.IntType)
  <|> (do
  symbol interproc "real"  
  return S.RealType)

varSection = do
  v_list <- varDeclList
  return (S.VarSection v_list)

parseBody = do
  symbol interproc "begin"
  stmt_list <- many1 statement
  symbol interproc "end"
  return (S.ProgramBody stmt_list)
  
-- Statements.
statement = s1 <|> s2 <|> s3 <|> s4 <|> s5

-- s1 ::= skip; | halt; | fail;
s1 = (do
  symbol interproc "skip"
  symbol interproc ";"
  return S.Skip)
  <|> (do
  symbol interproc "halt"        
  symbol interproc ";"
  return S.Halt)
  <|> (do   
  symbol interproc "fail"        
  symbol interproc ";"
  return S.Fail)

-- s2 ::= Assume BoolExpr;
s2 = do
  symbol interproc "assume"
  be <- parseBoolExpr
  symbol interproc ";"
  return (S.Assume be)

s3 = do
  lhs <- parseLHSExpr
  symbol interproc "="
  rhs <- parseRHSExpr
  symbol interproc ";"
  return (S.Assign lhs rhs)

parseLHSExpr = (do
  name <- identifier interproc
  return (S.LHS name))
  <|> (do
  symbol interproc "("        
  v_list <- (identifier interproc) `sepBy` (symbol interproc ",")
  symbol interproc ")"
  return (S.ParLHS v_list))
               
parseRHSExpr = (do
  symbol interproc "random"
  return (S.RandRHS))
  <|> (do             
  spaces        
  ne <- parseNumExpr        
  return (S.NumRHS ne))
  <|> (do            
  p <- parseProcInvocation
  return (S.ProcRHS p))

parseProcInvocation = do
  procName <- identifier interproc
  symbol interproc "("
  params <- (identifier interproc) `sepBy` (symbol interproc ",")
  symbol interproc ")"
  return (S.Call procName (S.Params params))

s4 = (do
  symbol interproc "if"
  be <- parseBoolExpr
  t2 <- parseThenBlock
  t3 <- parseElseBlock
  symbol interproc "endif"
  symbol interproc ";"
  return (S.If1 be t2 t3))
  <|> (do
  symbol interproc "if"        
  be <- parseBoolExpr
  t2 <- parseThenBlock
  symbol interproc "endif"
  symbol interproc ";"
  return (S.If2 be t2))

parseThenBlock = do
  symbol interproc "then"
  ss <- many1 statement
  return (S.Then ss)
  
parseElseBlock = do
  symbol interproc "else"
  ss <- many1 statement
  return (S.Else ss)
  
s5 = do
  symbol interproc "while"
  be <- parseBoolExpr
  symbol interproc "do"
  ss <- many1 statement
  symbol interproc "done"
  symbol interproc ";"
  return (S.While be ss)

-- Boolean Expressions.
parseBoolExpr = (do
  symbol interproc "true"
  return (S.Tru))
  <|> (do
  symbol interproc "false"        
  return (S.Fls))
  <|> (do
  symbol interproc "brandom"                      
  return (S.Brandom))
  <|> (do              
  ce <- parseConstraintExpr        
  return (S.Constraint ce))
  <|> (do              
  symbol interproc "not"        
  space
  be <- parseBoolExpr
  return (S.Not be))
  <|> (do              
  be1 <- parseBoolExpr        
  space
  symbol interproc "or"
  space
  be2 <- parseBoolExpr
  return (S.Or be1 be2))
  <|> (do              
  be1 <- parseBoolExpr        
  space
  symbol interproc "and"
  space
  be2 <- parseBoolExpr
  return (S.And be1 be2))
  <|> (do              
  be <- between (symbol interproc "(") (symbol interproc ")") parseBoolExpr        
  return (S.ParBool be))

parseConstraintExpr = do
  ne1 <- parseNumExpr
  rop <- relop
  ne2 <- parseNumExpr
  return (S.Relop rop ne1 ne2)

relop = (do
  symbol interproc "=="          
  return (EQUAL))
  <|> (do      
  symbol interproc ">="        
  return (GEQ))
  <|> (do      
  symbol interproc ">"        
  return (GT_Keyword))
  <|> (do      
  symbol interproc "<="        
  return (LEQ))
  <|> (do      
  symbol interproc "<"        
  return (LT_Keyword))

parseNumExpr = term `chainl1` addsubop          

term = factor `chainl1` muldivmodop

factor = (do
  n <- float interproc          
  return (S.NumReal n))
  <|> (do       
  n <- integer interproc       
  return (S.NumInt n))
  <|> (do       
  name <- identifier interproc       
  return (S.ID name))
  <|> (do       
  ne <- between (symbol interproc "(") (symbol interproc ")") parseNumExpr        
  return (S.ParNum ne))
         
addsubop = (do
  symbol interproc "+"             
  return (S.Bop S.Add))
  <|> (do         
  symbol interproc "-"        
  return (S.Bop S.Sub))

muldivmodop = (do
  symbol interproc "*"                
  return (S.Bop S.Mul))
  <|> (do            
  symbol interproc "/"        
  return (S.Bop S.Div))
  <|> (do
  symbol interproc "%"        
  return (S.Bop S.Mod))

