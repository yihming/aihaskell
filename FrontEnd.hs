module FrontEnd where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as T


import AbstractSyntax as S

keywords :: [String]
keywords = ["proc", "returns", "var", "begin", "end", "int", "real",
            "skip", "halt", "fail", "assume", "random", "if", "then",
            "else", "endif", "while", "do", "done", "true", "false",
            "brandom", "not", "or", "and"]

symbols :: String
symbols = ":()+-*/%=><;"

interprocDef :: T.LanguageDef st
interprocDef = interprocDef
               {T.commentStart = "/*",
                T.commentEnd = "*/",
                T.nestedComments = True,
                T.commentLine = "//",
                T.reservedNames = keywords,
                T.reservedOpNames = [],
                T.caseSensitive = True,
                T.identStart = letter <|> char '_',
                T.identLetter = alphaNum <|> char '_',
                T.opLetter = oneOf symbols,
                T.opStart = T.opLetter interprocDef
               }

lexer :: T.TokenParser st
lexer = T.makeTokenParser interprocDef

--identifier = T.identifier lexer
--symbol = T.symbol lexer
identifier = do
  c <- letter <|> char '_'
  cs <- many $ alphaNum <|> char '_'
  return (c:cs)

symbol s = do
  spaces
  string s
  spaces
reserved = T.reserved lexer
float = T.float lexer
integer = T.integer lexer

-- This is the main function of Front-end.
parseInterproc :: String -> S.Program
parseInterproc input = 
  case parse parseProg "" input of 
    Left err -> error $ "Syntax Error: " ++ show err
    Right val -> val

test :: IO ()
test = do
  src <- readFile "test/nn.input"
  let t = parse procDef "" src
  putStrLn $ show t

parseProg :: Parser S.Program
parseProg = try prog1 <|> prog2


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
procDef = try proc1 <|> proc2

proc1 = do
  symbol "proc"
  name <- identifier
  paramList <- between (symbol "(") (symbol ")") varDeclList  
  symbol "returns"
  retList <- between (symbol "(") (symbol ")") varDeclList
  v_sec <- varSection
  b <- parseBody
  return (S.Proc1 name paramList retList v_sec b)
  
proc2 = do
  symbol "proc"  
  name <- identifier
  paramList <- between (symbol "(") (symbol ")") varDeclList
  symbol "returns"
  retList <- between (symbol "(") (symbol ")") varDeclList
  b <- parseBody
  return (S.Proc2 name paramList retList b)
  
varDeclList = do
  decl_list <- varDecl `sepBy` (symbol ",")
  return (S.VarList decl_list)
  
varDecl = do  
  name <- identifier  
  symbol ":"
  t <- parseType
  return (S.Var name t)
  
parseType = (do
  symbol "int"
  return S.IntType)
  <|> (do
  symbol "real"  
  return S.RealType)

varSection = do
  symbol "var"
  v_list <- varDeclList
  symbol ";"
  return (S.VarSection v_list)

parseBody = do
  symbol"begin"
  stmt_list <- many1 statement
  symbol "end"
  return (S.ProgramBody stmt_list)
  
-- Statements.
statement = try s1 <|> try s2 <|> try s3 <|> try s4 <|> s5

-- s1 ::= skip; | halt; | fail;
s1 = (do
  symbol "skip"
  symbol ";"
  return S.Skip)
  <|> (do
  symbol "halt"        
  symbol ";"
  return S.Halt)
  <|> (do   
  symbol "fail"        
  symbol ";"
  return S.Fail)

-- s2 ::= Assume BoolExpr;
s2 = do
  symbol "assume"
  be <- parseBoolExpr
  symbol ";"
  return (S.Assume be)

s3 = do
  lhs <- parseLHSExpr
  symbol "="
  rhs <- parseRHSExpr
  symbol ";"
  return (S.Assign lhs rhs)

parseLHSExpr = (do
  name <- identifier
  return (S.LHS name))
  <|> (do
  symbol "("        
  v_list <- identifier `sepBy` (symbol ",")
  symbol ")"
  return (S.ParLHS v_list))
               
parseRHSExpr = (do
  symbol "random"
  return (S.RandRHS))
  <|> try (do             
  spaces        
  ne <- parseNumExpr        
  return (S.NumRHS ne))
  <|> (do            
  p <- parseProcInvocation
  return (S.ProcRHS p))

parseProcInvocation = do
  procName <- identifier
  symbol "("
  params <- identifier `sepBy` (symbol ",")
  symbol ")"
  return (S.Call procName (S.Params params))

s4 = try (do
  symbol "if"
  be <- parseBoolExpr
  t2 <- parseThenBlock
  t3 <- parseElseBlock
  symbol "endif"
  symbol ";"
  return (S.If1 be t2 t3))
  <|> (do
  symbol "if"        
  be <- parseBoolExpr
  t2 <- parseThenBlock
  symbol "endif"
  symbol ";"
  return (S.If2 be t2))

parseThenBlock = do
  symbol "then"
  ss <- many1 statement
  return (S.Then ss)
  
parseElseBlock = do
  symbol "else"
  ss <- many1 statement
  return (S.Else ss)
  
s5 = do
  symbol "while"
  be <- parseBoolExpr
  symbol "do"
  ss <- many1 statement
  symbol "done"
  symbol ";"
  return (S.While be ss)

-- Boolean Expressions.
parseBoolExpr = (do
  symbol "true"
  return (S.Tru))
  <|> (do
  symbol "false"        
  return (S.Fls))
  <|> (do
  symbol "brandom"                      
  return (S.Brandom))
  <|> (do              
  ce <- parseConstraintExpr        
  return (S.Constraint ce))
  <|> (do              
  symbol "not"        
  space
  be <- parseBoolExpr
  return (S.Not be))
  <|> try (do              
  be1 <- parseBoolExpr        
  space
  symbol "or"
  space
  be2 <- parseBoolExpr
  return (S.Or be1 be2))
  <|> (do              
  be1 <- parseBoolExpr        
  space
  symbol "and"
  space
  be2 <- parseBoolExpr
  return (S.And be1 be2))
  <|> (do              
  be <- between (symbol "(") (symbol ")") parseBoolExpr        
  return (S.ParBool be))

parseConstraintExpr = do
  ne1 <- parseNumExpr
  rop <- relop
  ne2 <- parseNumExpr
  return (S.Relop rop ne1 ne2)

relop = (do
  try (symbol "==")          
  return (EQUAL))
  <|> (do      
  try (symbol ">=")        
  return (GEQ))
  <|> (do      
  symbol ">"        
  return (GT_Keyword))
  <|> (do      
  try (symbol "<=")        
  return (LEQ))
  <|> (do      
  symbol "<"        
  return (LT_Keyword))

parseNumExpr = term `chainl1` addsubop          

term = factor `chainl1` muldivmodop

factor = try (do
  n <- float          
  return (S.NumReal n))
  <|> (do       
  n <- integer      
  return (S.NumInt n))
  <|> (do       
  name <- identifier       
  return (S.ID name))
  <|> (do       
  ne <- between (symbol "(") (symbol ")") parseNumExpr        
  return (S.ParNum ne))
         
addsubop = (do
  symbol "+"             
  return (S.Bop S.Add))
  <|> (do         
  symbol "-"        
  return (S.Bop S.Sub))

muldivmodop = (do
  symbol "*"                
  return (S.Bop S.Mul))
  <|> (do            
  symbol "/"        
  return (S.Bop S.Div))
  <|> (do
  symbol "%"        
  return (S.Bop S.Mod))

