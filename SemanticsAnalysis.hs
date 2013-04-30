module SemanticsAnalysis where

import Data.List

import qualified AbstractSyntax as S
import qualified HaskellPPL as HP
import qualified FrontEnd as FE

test :: IO ()
test = do 
  src <- readFile "./test/pres.input"
  let t@(S.Program vs b) = FE.parseInterproc src
  let symtb = genSymbolTable vs 0
  putStrLn $ show symtb

type Var = [Char]

data SymbolTable = SymT [(Var, Int)]
                 deriving Show

lookupSymIdx :: Var -> SymbolTable -> Maybe Int
lookupSymIdx _ (SymT []) = Nothing
lookupSymIdx v (SymT ((x, n):ts)) = 
  if v == x
     then Just n
     else lookupSymIdx v (SymT ts)     

aiProcess :: S.Program -> IO S.Program
aiProcess (S.Program vs body) = do 
  let symtable = genSymbolTable vs 0
  newBody <- processBody body symtable
  return (S.Program vs newBody)

genSymbolTable :: S.VarSection -> Int -> SymbolTable
genSymbolTable (S.VarSection vs) n = 
  SymT $ f vs n
  where
    f :: [S.VarDecl] -> Int -> [(Var, Int)]
    f [] _ = []
    f ((S.VarDecl v _):vs) n = (v, n) : (f vs (n+1))

processBody :: S.Body -> SymbolTable -> IO S.Body
processBody (S.Body ss) symtable = HP.withPPL $ do
  let cs = [1 HP.%== 1] :: HP.LinConSys
  p <- HP.polyhedron_from_consys cs
  print p
  let newSS = processStmt $ (S.Assert p) : ss
  return (S.Body newSS)
  
processStmt :: [S.Stmt] -> [S.Stmt]
processStmt (s:ss) = s:ss