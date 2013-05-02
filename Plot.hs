{-# LANGUAGE OverloadedStrings #-}
module Plot where

import Data.List
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Types.Monadic
import Data.Text.Lazy.Internal
import qualified Data.GraphViz.Types.Generalised as G
import Data.Text.Lazy
import Data.GraphViz.Commands.IO
import System.IO

import Debug.Trace

import qualified AbstractSyntax as S
--import qualified HaskellPPL as HP

constructDotGraph :: S.Program -> G.DotGraph Integer
constructDotGraph p = digraph' $ do
  let graphAttrList = [Ordering OutEdges,RankSep [0.4], BgColor [WC (X11Color LightGray) Nothing]]
  let nodeAttrList = [Shape BoxShape, FixedSize False, FontSize 12.0, FontName "Helvetica-bold", FontColor (X11Color Blue), 
                      Width 0.25, Height 0.25, Color [WC (X11Color Black) Nothing], FillColor [WC (X11Color White) Nothing],
                      Style [SItem Filled [], SItem Solid [], SItem Bold []]]
  let edgeAttrList = [ArrowSize 0.5, Color [WC (X11Color Black) Nothing], Style [SItem Bold []]]
  
  graphAttrs graphAttrList
  nodeAttrs nodeAttrList
  edgeAttrs edgeAttrList
  
  let (nodeList, edgeList) = genAssocsFromProgram p 0
  
  sequence [ node idx [textLabel (pack name)] | (idx, name) <- nodeList]
  sequence [ u --> v | (u, v) <- edgeList]



genAssocsFromProgram :: S.Program -> Integer -> ([(Integer, String)], [(Integer, Integer)])
genAssocsFromProgram (S.Program vs b) idx =
  let n1 = (idx, "Program")
      (nodeList1, edgeList1, retIdx) = genAssocsFromVarSection vs (idx + 1)
      (nodeList2, edgeList2, retIdx') = genAssocsFromBody b retIdx
      e1 = (idx, idx + 1)
      e2 = (idx, retIdx)
  in  ([n1] ++ nodeList1 ++ nodeList2, [e1, e2] ++ edgeList1 ++ edgeList2)
  
genAssocsFromVarSection :: S.VarSection -> Integer -> ([(Integer, String)], [(Integer, Integer)], Integer)
genAssocsFromVarSection (S.VarSection vd_list) idx = 
  let (nodeList, edgeList, retIdx) = genAssocsFromVarDecls vd_list idx ([], [], idx+1)
      n1 = (idx, "VarSection")
  in  (n1 : nodeList, edgeList, retIdx)

genAssocsFromVarDecls :: [S.VarDecl] -> Integer -> ([(Integer, String)], [(Integer, Integer)], Integer)
                            -> ([(Integer, String)], [(Integer, Integer)], Integer)
genAssocsFromVarDecls [] _ (nl, el, idx) = (nl, el, idx)
genAssocsFromVarDecls (vd@(S.VarDecl v t):vds) idxParent (nl, el, idxBegin) = 
  let nTag = (idxBegin, "VarDecl")
      nVar = (idxBegin + 1, v)
      nType = case t of
                S.IntType -> (idxBegin + 2, "Int")
                S.RealType-> (idxBegin + 2, "Real")
      e1 = (idxParent, idxBegin)
      e2 = (idxBegin, idxBegin + 1)
      e3 = (idxBegin, idxBegin + 2)
      nl' = nl ++ [nTag, nVar, nType]
      el' = el ++ [e1, e2, e3]
      idxBegin' = idxBegin + 3
  in  genAssocsFromVarDecls vds idxParent (nl', el', idxBegin')
      
genAssocsFromBody :: S.Body -> Integer -> ([(Integer, String)], [(Integer, Integer)], Integer)
genAssocsFromBody (S.Body stmt_list) idx = 
  let nTag = (idx, "Body")
      (nodeList, edgeList, retIdx) = genAssocsFromStatements stmt_list idx ([], [], idx + 1)
  in  (nTag : nodeList, edgeList, retIdx)

genAssocsFromStatements :: [S.Stmt] -> Integer -> ([(Integer, String)], [(Integer, Integer)], Integer)
                               -> ([(Integer, String)], [(Integer, Integer)], Integer)
genAssocsFromStatements [] _ (nl, el, idx) = (nl, el, idx)
genAssocsFromStatements (s:ss) idxParent (nl, el, idxBegin) = 
  case s of
    S.Skip -> let nTag = (idxBegin, "Skip")
                  e1 = (idxParent, idxBegin)
              in  genAssocsFromStatements ss idxParent (nl++[nTag], el++[e1], idxBegin + 1)
                  
    S.Halt -> let nTag = (idxBegin, "Halt")
                  e1 = (idxParent, idxBegin)
              in  genAssocsFromStatements ss idxParent (nl++[nTag], el++[e1], idxBegin + 1)    
                  
    S.Fail -> let nTag = (idxBegin, "Fail")              
                  e1 = (idxParent, idxBegin)
              in  genAssocsFromStatements ss idxParent (nl++[nTag], el++[e1], idxBegin + 1)    
                  
    S.Assign v rhs
           -> let nTag = (idxBegin, "Assign")
                  e1 = (idxParent, idxBegin)
                  nLHS = (idxBegin + 1, v)
                  e2 = (idxBegin, idxBegin + 1)
                  (nodeList, edgeList, retIdx) = genAssocsFromRHSExpr rhs idxBegin (idxBegin + 2)
                  nl' = nl ++ [nTag, nLHS] ++ nodeList
                  el' = el ++ [e1, e2] ++ edgeList
                  idxBegin' = retIdx
              in  genAssocsFromStatements ss idxParent (nl', el', idxBegin')
                  
    S.While be stmt_list
           -> let nTag = (idxBegin, "While")
                  e1 = (idxParent, idxBegin)
                  nTest = (idxBegin+1, "Test")
                  e2 = (idxBegin, idxBegin + 1)
                  (nodeList1, edgeList1, retIdx) = genAssocsFromBoolExpr be (idxBegin+2)
                  e3 = (idxBegin+1, idxBegin+2)
                  (nodeList2, edgeList2, retIdx') = genAssocsFromStatements stmt_list idxBegin ([], [], retIdx)
                  nl' = nl++[nTag, nTest]++nodeList1++nodeList2
                  el' = el++[e1, e2, e3]++edgeList1++edgeList2
                  idxBegin' = retIdx'
              in  genAssocsFromStatements ss idxParent (nl', el', idxBegin')
              

    _      -> (nl, el, idxBegin)

genAssocsFromRHSExpr :: S.RHSExpr -> Integer -> Integer -> ([(Integer, String)], [(Integer, Integer)], Integer)
genAssocsFromRHSExpr (S.NumRHS ne) idxParent idxBegin = 
  let nTag = (idxBegin, "NumRHS")
      e1 = (idxParent, idxBegin)
      e2 = (idxBegin, idxBegin + 1)
      (nodeList, edgeList, retIdx) = genAssocsFromNumExpr ne (idxBegin+1)
  in  (nTag:nodeList, [e1, e2]++edgeList, retIdx)

genAssocsFromNumExpr :: S.NumExpr -> Integer -> ([(Integer, String)], [(Integer, Integer)], Integer)
genAssocsFromNumExpr ne idxBegin = 
  case ne of
    S.NumInt val -> let nInt = (idxBegin, "NumInt")
                        nVal = (idxBegin + 1, show val)
                        e1 = (idxBegin, idxBegin + 1)
                    in  ([nInt, nVal], [e1], idxBegin + 2)
                        
    S.Var v      -> let nVar = (idxBegin, "Var")
                        nName = (idxBegin + 1, v)
                        e1 = (idxBegin, idxBegin + 1)
                    in  ([nVar, nName], [e1], idxBegin + 2)
    
    S.Bop bop ne1 ne2
                 -> let nOp = (idxBegin, show bop)
                        e1 = (idxBegin, idxBegin + 1)
                        e2 = (idxBegin, retIdx)
                        (nodeList1, edgeList1, retIdx) = genAssocsFromNumExpr ne1 (idxBegin + 1)
                        (nodeList2, edgeList2, retIdx') = genAssocsFromNumExpr ne2 retIdx
                    in  ([nOp] ++ nodeList1 ++ nodeList2, [e1, e2] ++ edgeList1++edgeList2, retIdx')
                        
genAssocsFromBoolExpr :: S.BoolExpr -> Integer -> ([(Integer, String)], [(Integer, Integer)], Integer)
genAssocsFromBoolExpr be idxBegin = 
  case be of
    S.Tru -> let nTru = (idxBegin, "True")
             in  ([nTru], [], idxBegin+1)
    S.Fls -> let nFls = (idxBegin, "False")
             in  ([nFls], [], idxBegin+1)    
    S.And ce1 ce2             
          -> let nOp = (idxBegin, "And")
                 (nodeList1, edgeList1, retIdx) = genAssocsFromConstraintExpr ce1 (idxBegin+1)
                 (nodeList2, edgeList2, retIdx') = genAssocsFromConstraintExpr ce2 retIdx
                 e1 = (idxBegin, idxBegin+1)
                 e2 = (idxBegin, retIdx)
             in  ([nOp]++nodeList1++nodeList2, [e1,e2]++edgeList1++edgeList2, retIdx')
    S.Constraint ce
          -> let (nodeList, edgeList, retIdx) = genAssocsFromConstraintExpr ce idxBegin
             in  (nodeList, edgeList, retIdx)    
  
genAssocsFromConstraintExpr :: S.ConstraintExpr -> Integer -> ([(Integer, String)], [(Integer, Integer)], Integer)
genAssocsFromConstraintExpr ce@(S.Relop relop ne1 ne2) idxBegin = 
  let nOp = (idxBegin, show relop)
      (nodeList1, edgeList1, retIdx) = genAssocsFromNumExpr ne1 (idxBegin + 1)
      (nodeList2, edgeList2, retIdx') = genAssocsFromNumExpr ne2 retIdx
      e1 = (idxBegin, idxBegin + 1)
      e2 = (idxBegin, retIdx)
  in  ([nOp]++nodeList1++nodeList2, [e1,e2]++edgeList1++edgeList2, retIdx')

genDotFile :: FilePath -> S.Program -> IO ()
genDotFile fp prog = do
  let g = constructDotGraph prog
  writeDotFile fp g