module SemanticsAnalysis where

import Data.List
import Data.Maybe

import qualified AbstractSyntax as S
import HaskellPPL
import qualified FrontEnd as FE
import qualified PPL as PPL


test :: IO ()
test = do
  src <- readFile "./test/pres.input"
  let t@(S.Program vs b) = FE.parseInterproc src
  let symtb = genSymbolTable vs 0
  putStrLn $ show symtb

type Var = [Char]

-- SymbolTable is used for storing
--   the mapping from variable names to indices.
data SymbolTable = SymT [(Var, Integer, Value)]
                 deriving Show

printSymbolTable :: SymbolTable -> String
printSymbolTable (SymT ts) = 
  (concat $ intersperse ", " (map printSymbolTuple ts))

printSymbolTuple :: (Var, Integer, Value) -> String
printSymbolTuple (v, idx, _) = 
  v ++ " = X" ++ show idx

{-
printAssertion :: LinConSys -> SymbolTable -> IO ()
printAssertion [] smt = do
  return ()
printAssertion ((LinConstr (rel, lhs, rhs)):cs) smt = do
  let lhs' = mapLinExpr lhs smt
  let rhs' = mapLinExpr rhs smt
  let c'   = LinConstr (LinConstr (rel, lhs', rhs'))
  cs' <- printAssertion cs smt
  let newCS = c':cs'
  print newCS
  
mapLinExpr :: LinExpr -> SymbolTable -> LinExpr
mapLinExpr le smt = 
  case le of
    LinVar dimV -> 
-}

data Value = IntVal (Maybe Integer)
           | RealVal (Maybe Double)
instance Show Value where
  show (IntVal mi) = 
    case mi of
      Just v -> "(int " ++ show v ++ ")"
      Nothing-> "⊥ "
  show (RealVal md) = 
    case md of
      Just v -> "(real " ++ show v ++ ")"
      Nothing-> "⊥ "

lookupSym :: Var -> SymbolTable -> Maybe (Integer, Value)
lookupSym _ (SymT []) = Nothing
lookupSym v (SymT ((x, n, val):ts)) = 
  if v == x
     then Just (n, val)
     else lookupSym v (SymT ts)     

maxIdxSymTable :: SymbolTable -> Integer
maxIdxSymTable (SymT ts) = 
  let (_, idx, _) = last ts
  in  idx


maxDimensionAvailable :: SymbolTable -> Polyhedron -> IO Dimension
maxDimensionAvailable smt p = do
  let idx1 = maxIdxSymTable smt + 1
  let (LinVar dim1) = var idx1
  dim2 <- PPL.polyhedronSpaceDimension p
  case compare dim1 dim2 of
    EQ -> return dim1
    LT -> return dim2
    GT -> return dim1

-- This is the main semantics injection function.
aiProcess :: S.Program -> IO S.Program
aiProcess (S.Program vs body) = do 
  let symtable = genSymbolTable vs 0
  newBody <- processBody body symtable
  return (S.Program vs newBody)

-- Generate the symbol table based on "var" section.
genSymbolTable :: S.VarSection -> Integer -> SymbolTable
genSymbolTable (S.VarSection vs) n = 
  SymT $ f vs n
  where
    f :: [S.VarDecl] -> Integer -> [(Var, Integer, Value)]
    f [] _ = []
    f ((S.VarDecl v t):vs) n = 
      case t of
        S.IntType  -> (v, n, (IntVal Nothing)) : (f vs (n+1))
        S.RealType -> (v, n, (RealVal Nothing)) : (f vs (n+1))

-- Entry of processing the program body.
processBody :: S.Body -> SymbolTable -> IO S.Body
processBody (S.Body ss) symtable = withPPL $ do
  let st = printSymbolTable symtable
  let cs = topConsys
  s_remain <- processStmtList ss cs symtable 
  let newSS = (S.Comment st) : (S.Assert cs) : s_remain
  return (S.Body newSS)
  
-- Perform the semantics injection through statement
--   list one by one.
processStmtList :: [S.Stmt] -> LinConSys -> SymbolTable -> IO [S.Stmt]  
processStmtList s_list cs smt = 
  case s_list of
    []     -> do
      return []
    (s:ss) -> do
      (s', cs') <- processStmt s cs smt
      ss' <- processStmtList ss cs' smt
      case s' of
        S.Comment _ -> do
          return $ s' : ss'
        _           -> do
          return $ s':(S.Assert cs'):ss'
  
processStmt :: S.Stmt -> LinConSys -> SymbolTable -> IO (S.Stmt, LinConSys)
processStmt s cs smt = do
  case s of
    S.Skip           -> do
      return (s, cs)
    S.Halt           -> do
      return (s, bottomConsys)
    S.Fail           -> do
      return (s, bottomConsys)
    S.Assign lhs rhs -> do
      pBefore <- polyhedron_from_consys cs
      let (idx, _) = fromJust $ lookupSym lhs smt
      let (LinVar dimLHS) = var idx
      dimFresh <- maxDimensionAvailable smt pBefore  
      let cs' = consys_switch_dimension cs dimLHS dimFresh
      let le = build_linexpr_from_rhs rhs smt
      let le' = linexpr_switch_dimension le dimLHS dimFresh
      let newCS = [(var dimLHS) %== le']
      let finalCS = cs' ++ newCS
      --let finalCS' = consys_propagate_dimension dimFresh finalCS
      return (s, finalCS)
      
    S.While be ss    -> do
      let csTest = build_linconstr_from_boolexpr be smt
      pTest <- polyhedron_from_consys csTest
      pBegin <- polyhedron_from_consys cs
      polyhedron_intersect pBegin pTest
      csBegin <- consys_from_polyhedron pBegin
      let csEnd = csBegin
      -- Here to set the iterations before applying widening.
      let nIter = 2
      css <- processIteration ss csBegin smt nIter
      -- Apply widening to the last two polyhedra.
      let len = length css
      let cs1 = css !! (len - 2)
      p1 <- polyhedron_from_consys cs1
      let cs2 = css !! (len - 1)
      p2 <- polyhedron_from_consys cs2
      polyhedron_h79_widening p2 p1
      -- Intersect it with test polyhedron.
      let pWiden = p2
      polyhedron_intersect pWiden pTest
      csWiden <- consys_from_polyhedron pWiden
      -- Do the final process.
      ss' <- processStmtList ss csWiden smt 
      let (S.Assert csFinalEnd) = last ss'
      pFinalEnd <- polyhedron_from_consys csFinalEnd
      -- Intersect with Complement of Test.
      -- TODO: make the case of more than one test condition work.
      let csCompTest = complement_consys csTest
      pCompTest <- polyhedron_from_consys csCompTest
      -- pFinalBegin is the intersection of pFinalEnd and pTest.
      let pFinalBegin = pFinalEnd
      polyhedron_intersect pFinalBegin pTest
      csFinalBegin' <- consys_from_polyhedron pFinalBegin
      -- pFinalEnd is the intersection of itself and pCompTest.
      polyhedron_intersect pFinalEnd pCompTest
      csFinalEnd' <- consys_from_polyhedron pFinalEnd
      -- Return the whole thing.
      return (S.While be $ (S.Assert csFinalBegin') : ss', csFinalEnd')
      
      
    _                -> do
      return (s, cs)
    
processIteration :: [S.Stmt] -> LinConSys -> SymbolTable -> Integer -> IO [LinConSys]
processIteration ss csBegin smt n = do
  if n > 0
     then do
       ss' <- processStmtList ss csBegin smt
       let (S.Assert csEnd) = last ss'
       pEnd <- polyhedron_from_consys csEnd
       pBegin <- polyhedron_from_consys csBegin
       polyhedron_hull pBegin pEnd
       csBegin' <- consys_from_polyhedron pBegin
       cs_list <- processIteration ss csBegin' smt (n - 1)
       return $ csBegin' : cs_list
     else do  
       return []

-- TODO: make complicated boolean expressions also work.
build_linconstr_from_boolexpr :: S.BoolExpr -> SymbolTable -> LinConSys
build_linconstr_from_boolexpr be smt = 
  case be of
    S.Tru           -> topConsys
    S.Fls           -> bottomConsys
    S.Constraint ce -> case ce of
                         S.Relop relop ne1 ne2 ->
                           let le1 = build_linexpr_from_numexpr ne1 smt
                               le2 = build_linexpr_from_numexpr ne2 smt
                           in  case relop of
                                 S.EQUAL      -> [le1 %== le2] :: LinConSys
                                 S.GEQ        -> [le1 %>= le2] :: LinConSys
                                 S.GT_Keyword -> [le1 %> le2] :: LinConSys
                                 S.LEQ        -> [le1 %<= le2] :: LinConSys
                                 S.LT_Keyword -> [le1 %< le2] :: LinConSys
    S.And ce1 ce2   -> topConsys 

build_linexpr_from_rhs :: S.RHSExpr -> SymbolTable -> LinExpr
build_linexpr_from_rhs (S.NumRHS ne) smt = 
  build_linexpr_from_numexpr ne smt
    
build_linexpr_from_numexpr :: S.NumExpr -> SymbolTable -> LinExpr
build_linexpr_from_numexpr ne smt =
    case ne of
      S.NumInt n        -> LinCoef n
      S.Var v           -> let (idx, _) = fromJust $ lookupSym v smt
                           in  (var idx) :: LinExpr
      S.Bop bop ne1 ne2 -> let le1 = build_linexpr_from_numexpr ne1 smt
                               le2 = build_linexpr_from_numexpr ne2 smt
                           in  case bop of
                                 S.Add -> le1 + le2 :: LinExpr 
                                 S.Sub -> le1 - le2 :: LinExpr
                                 S.Mul -> le1 * le2 :: LinExpr
                                 _     -> error "Operator not implemented"

