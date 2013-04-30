module FrontEnd where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as T
import Data.Char
import Data.List

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
--integer = T.integer lexer
integer = many1 digit

-- This is the main function of Front-end.
parseInterproc :: String -> S.Program
parseInterproc input = 
  case parse parseProg "" input of 
    Left err -> error $ "Syntax Error: " ++ show err
    Right val -> val

test :: IO ()
test = do
  src <- readFile "test/nn.input"
  let t = parseInterproc src
  putStrLn $ show t

parseProg :: Parser S.Program
parseProg = prog

prog = do
  vs <- varSection
  b <- parseBody
  return (S.Program vs b)
  
varSection = do
  symbol "var"
  v_list <- varDecl `sepBy` (symbol ",")
  symbol ";"
  return (S.VarSection v_list)

varDecl = do
  name <- identifier
  symbol ":"
  typeName <- parseType
  return (S.VarDecl name typeName)
  
parseType = (do
  symbol "int"           
  return S.IntType) <|>
            (do
  symbol "real"              
  return S.RealType)

parseBody = do
  symbol "begin"
  bs <- many1 statement
  symbol "end"
  return (S.Body bs)

-- Statements.
statement = try s1 <|> try s2 <|> s3

-- s1 ::= skip; | halt; | fail;
s1 = (do
  symbol "skip"       
  symbol ";"
  return S.Skip) <|>
     (do
  symbol "halt"       
  symbol ";"
  return S.Halt) <|>
     (do
  symbol "fail"       
  symbol ";"
  return S.Fail)

-- s2 ::= Var = RHSExpr
s2 = do
  lhs <- identifier
  symbol "="
  rhs <- parseRHS
  symbol ";"
  return (S.Assign lhs rhs)

parseRHS = do
  rhs  <- parseNumExpr
  return (S.NumRHS rhs)

-- s3 ::= while BoolExpr do [Stmt] done;
s3 = do
  symbol "while"
  be <- parseBoolExpr
  symbol "do"
  ss <- many1 statement
  symbol "done"
  symbol ";"
  return (S.While be ss)

-- Boolean Expressions.
parseBoolExpr = try (do
  symbol "true"
  return (S.Tru))
  <|> try (do
  symbol "false"        
  return (S.Fls))
  <|> try (do
  be1 <- parseConstraintExpr            
  symbol "and"
  be2 <- parseConstraintExpr
  return (S.And be1 be2))
  <|> (do              
  ce <- parseConstraintExpr
  return (S.Constraint ce))


parseConstraintExpr = do
  ne1 <- parseNumExpr
  rop <- relop
  ne2 <- parseNumExpr
  return (S.Relop rop ne1 ne2)

relop = try (do
  symbol "=="          
  return (EQUAL))
  <|> try (do      
  symbol ">="        
  return (GEQ))
  <|> (do      
  symbol ">"        
  return (GT_Keyword))
  <|> try (do      
  symbol "<="        
  return (LEQ))
  <|> (do      
  symbol "<"        
  return (LT_Keyword))

parseNumExpr = term `chainl1` addsubop          

term = factor `chainl1` muldivmodop

factor = (do       
  spaces           
  n <- integer      
  spaces
  return (S.NumInt (read n::Integer)))
  <|> (do       
  spaces        
  name <- identifier       
  spaces
  return (S.Var name))
         
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

