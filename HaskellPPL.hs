-- Haskell data types and API for the Parma Polyhedra Library
-- Build on top of Axel Simon's PPL library binding 
-- pbv, 2004, 2005, 2008

module HaskellPPL where
import System.IO.Unsafe
import qualified PPL as PPL
import Data.List
import Control.Monad
import Data.Maybe


type Dimension   = PPL.Dimension
type Coefficient = Integer

-- data type for linear expressions 
data LinExpr = LinVar Dimension              -- variable
	     | LinCoef Coefficient           -- coefficient
	     | LinPlus LinExpr LinExpr       -- sum
	     | LinProd Coefficient LinExpr   -- linear product 
	       deriving (Eq,Ord)


-- make a variable of a given dimension
var :: Integral a => a -> LinExpr
var = LinVar . fromIntegral 

-- data type for a linear constraint
-- relation operator, left hand side and right hand side (for pretty printing)
type LinConstrRel = PPL.ConstraintType

-- linear equality and inequality constraints
newtype LinConstr = LinConstr (LinConstrRel, LinExpr, LinExpr) 

instance Show LinConstr where
    showsPrec _ (LinConstr (rel,lhs,rhs)) = shows lhs . shows rel . shows rhs

instance Eq LinConstr where
  lhs@(LinConstr (rel1, le1, le2)) == rhs@(LinConstr (rel2, le3, le4)) =
    if rel1 == rel2 && le1 == le3 && le2 == le4
       then True
       else False     

-- infix operators for constraint relations
infix 4 %==, %<, %>, %<=, %>=

lhs %== rhs = LinConstr (PPL.Equal, lhs, rhs)
lhs %>= rhs = LinConstr (PPL.GreaterOrEqual, lhs, rhs)
lhs %<= rhs = LinConstr (PPL.LessOrEqual, lhs, rhs)
lhs %>  rhs = LinConstr (PPL.Greater, lhs, rhs)
lhs %<  rhs = LinConstr (PPL.Less, lhs, rhs)


instance Show LinExpr where
    showsPrec _ (LinVar i) = ('X':).shows i
    showsPrec _ (LinCoef c) = showParen (c<0) (shows c)
    showsPrec p (LinPlus e1 e2) 
	= showParen (p>10) $ shows e1.('+':).shows e2
    showsPrec _ (LinProd c e) 
	= showParen (c<0) (shows c).('*':). showsPrec 10 e


-- instances for arithmetic operators
-- performs some algebraic simplications 
instance Num LinExpr where
    LinCoef c1+LinCoef c2 = LinCoef (c1+c2)
    LinCoef 0 + e = e
    e + LinCoef 0 = e
    e1 + e2 = LinPlus e1 e2
    e1 - e2 = LinPlus e1 (LinProd (-1) e2)
    LinCoef c1*LinCoef c2 = LinCoef (c1*c2)
    LinCoef 0 * e = LinCoef 0
    e * LinCoef 0 = LinCoef 0
    LinCoef 1 * e = e
    e * LinCoef 1 = e
    LinCoef c*e = LinProd c e
    e*LinCoef c = LinProd c e
    e1 * e2 = error "LinExpr: multiplication by non-constant"
    signum  = error "LinExpr: signum not implemented"
    abs     = error "LinExpr: abs not implemented"
    fromInteger n = LinCoef n


-- system of linear constraints
type LinConSys = [LinConstr]


-- generators and generator systems
data LinGenerator = Point LinExpr Coefficient
		  | ClosurePoint LinExpr Coefficient
		  | Ray LinExpr
		  | Line LinExpr
		    deriving (Eq,Show)

-- system of generators
type LinGenSys = [LinGenerator]

-- class for types with PPL dimensions
class Dimensional a where
    dimensions :: a -> [Dimension]


instance Dimensional LinExpr where
    dimensions (LinVar x)  = [x]
    dimensions (LinCoef c) = []
    dimensions (LinPlus e1 e2) = dimensions e1 ++ dimensions e2
    dimensions (LinProd c e) = dimensions e
    

instance Dimensional LinGenerator where
    dimensions (Point le c) = dimensions le
    dimensions (ClosurePoint le c) = dimensions le
    dimensions (Ray le) = dimensions le
    dimensions (Line le) = dimensions le


instance Dimensional LinConstr where
    dimensions (LinConstr (_,l,r)) = dimensions l ++ dimensions r


instance Dimensional a => Dimensional [a] where
    dimensions xs = concatMap dimensions xs


-- Yiming
linconstr_contains_dimension :: Dimension -> LinConstr -> Bool    
linconstr_contains_dimension dim (LinConstr (_, le1, le2)) = 
  (linexpr_contains_dimension dim le1) || (linexpr_contains_dimension dim le2)
  
linexpr_contains_dimension :: Dimension -> LinExpr -> Bool
linexpr_contains_dimension dim le =
  case le of
    LinVar d -> case compare d dim of
                  EQ -> True
                  _  -> False
    LinCoef c-> False
    LinPlus le1 le2
             -> (linexpr_contains_dimension dim le1) || (linexpr_contains_dimension dim le2)
    LinProd c le'
             -> linexpr_contains_dimension dim le'
    
    
consys_propagate_dimension :: Dimension -> LinConSys -> LinConSys
consys_propagate_dimension dim [] = [] :: LinConSys
consys_propagate_dimension dim lcs@((LinConstr tp):cs) =
  case tp of
    (PPL.Equal,le1,le2) -> 
      case le1 of
        LinVar d -> case compare dim d of
                      EQ -> case le2 of
                              LinCoef n -> f dim n lcs
                              _         -> consys_propagate_dimension dim cs
                      _  -> consys_propagate_dimension dim cs
        LinCoef n-> case le2 of
                      LinVar d -> case compare dim d of
                                    EQ -> f dim n lcs
                                    _  -> consys_propagate_dimension dim cs
                      _        -> consys_propagate_dimension dim cs
        _        -> consys_propagate_dimension dim cs
        
    _                    -> consys_propagate_dimension dim cs
  where
    f :: Dimension -> Coefficient -> LinConSys -> LinConSys
    f dim n [] = [] :: LinConSys
    f dim n (c@(LinConstr (rel, le1, le2)):cs) = 
      if linconstr_contains_dimension dim c
         then let le1' = linexpr_propagate_dimension dim n le1
                  le2' = linexpr_propagate_dimension dim n le2
              in  (LinConstr (rel, le1', le2')):(f dim n cs)    
         else c:(f dim n cs)
    
linexpr_propagate_dimension :: Dimension -> Coefficient -> LinExpr -> LinExpr
linexpr_propagate_dimension dim n le =
  case le of
    LinVar d -> case compare d dim of
                  EQ -> LinCoef n
                  _  -> le
    LinCoef _-> le
    LinPlus le1 le2 -> let le1' = linexpr_propagate_dimension dim n le1
                           le2' = linexpr_propagate_dimension dim n le2
                       in  LinPlus le1' le2'
    LinProd v le' -> let le'' = linexpr_propagate_dimension dim n le'
                     in  LinProd v le''    

-- Yiming: get the complement of a linear constraint.
--   PPL.Equal doesn't work in it, since its complement 
--   is non-convex.
-- TODO: Make the case of more than one clauses works.
complement_consys :: LinConSys -> LinConSys
complement_consys [] = bottomConsys
complement_consys (lc : lcs) = 
  if [lc] == bottomConsys
     then topConsys
     else [complement_linconstr lc] :: LinConSys     

-- TODO: Resolve the problem with strict polyhedra.
complement_linconstr :: LinConstr -> LinConstr
complement_linconstr (LinConstr lc) = 
  case lc of
    (PPL.GreaterOrEqual, lhs, rhs) -> LinConstr (PPL.LessOrEqual, lhs, rhs)
    (PPL.LessOrEqual,    lhs, rhs) -> LinConstr (PPL.GreaterOrEqual, lhs, rhs)
    (PPL.Greater,        lhs, rhs) -> LinConstr (PPL.LessOrEqual, lhs, rhs)
    (PPL.Less,           lhs, rhs) -> LinConstr (PPL.GreaterOrEqual, lhs, rhs)
    _                          -> LinConstr lc

-- Yiming: Define the Bottom Linear Constraint System here.
bottomConsys :: LinConSys
bottomConsys = [ 0 %== 1 ] :: LinConSys

isBottom :: LinConSys -> Bool
isBottom cs = 
  if cs == bottomConsys
     then True
     else False     
          
-- Yiming: Define the Top Linear Constraint System here.
topConsys :: LinConSys
topConsys = [] :: LinConSys

isTop :: LinConSys -> Bool
isTop cs = 
  if cs == topConsys
     then True
     else False     
-- the space dimension of a structure is one plus the greatest size var
spaceDimension :: Dimensional a => a -> Dimension
spaceDimension a | null xs  = 0
		 | otherwise = 1 + maximum xs
		 where xs = dimensions a

-- Yiming
polyhedron_switch_dimension :: Polyhedron -> Dimension -> Dimension -> IO Polyhedron
polyhedron_switch_dimension p dim0 dim1 = do
  cs <- consys_from_polyhedron p
  let cs' = consys_switch_dimension cs dim0 dim1
  p' <- polyhedron_from_consys cs' 
  return p'

consys_switch_dimension :: LinConSys -> Dimension -> Dimension -> LinConSys
consys_switch_dimension [] _ _ = []
consys_switch_dimension ((LinConstr (rel, le1, le2)):cs) dim0 dim1 = 
  (LinConstr (rel, linexpr_switch_dimension le1 dim0 dim1,
                   linexpr_switch_dimension le2 dim0 dim1))
  : (consys_switch_dimension cs dim0 dim1)


linexpr_switch_dimension :: LinExpr -> Dimension -> Dimension -> LinExpr
linexpr_switch_dimension le dim0 dim1 = 
  case le of
    LinVar d        -> case compare d dim0 of
                         EQ -> LinVar dim1
                         _  -> le
    LinPlus le1 le2 -> let le1' = linexpr_switch_dimension le1 dim0 dim1
                           le2' = linexpr_switch_dimension le2 dim0 dim1
                       in  LinPlus le1' le2'
    LinProd coe le1 -> let le1' = linexpr_switch_dimension le1 dim0 dim1
                       in  LinProd coe le1'
    LinCoef coe     -> le
    
    


-- synonym to PPL convex polyhedra type 
type Polyhedron = PPL.Polyhedron

new_polyhedron :: Polyhedron -> IO Polyhedron
new_polyhedron = PPL.newCPolyhedronFromCPolyhedron
--polyhedron_empty = PPL.newCPolyhedronEmptyFromDimension 0
polyhedron_universe = polyhedron_from_consys []

-- construct a polyhedron 
polyhedron_from_polyhedron :: Polyhedron -> IO Polyhedron
polyhedron_from_polyhedron = PPL.newCPolyhedronFromCPolyhedron 

polyhedron_from_consys :: LinConSys -> IO Polyhedron
polyhedron_from_consys cs
    = do { ppl_cs <- mk_consys cs
         ; PPL.newCPolyhedronFromConstraintSystem ppl_cs 
         }

polyhedron_from_gensys :: LinGenSys -> IO Polyhedron
polyhedron_from_gensys gs 
    = do { ppl_gs <- mk_gensys gs
         ; PPL.newCPolyhedronFromGeneratorSystem ppl_gs 
         }


-- remove higher dimensions from a polyhedron
polyhedron_remove_higher_dimensions :: Polyhedron -> Dimension -> IO ()
polyhedron_remove_higher_dimensions p dim 
    = do { dim'<- PPL.polyhedronSpaceDimension p
	 ; when (dim'>dim) $ 
           PPL.polyhedronRemoveHigherSpaceDimensions p dim
	 }

-- check and adjust dimensions if necessary
polyhedron_adjust_dimensions :: Polyhedron -> Polyhedron -> IO ()
polyhedron_adjust_dimensions p0 p1
    = do { dim0 <- PPL.polyhedronSpaceDimension p0
	 ; dim1 <- PPL.polyhedronSpaceDimension p1
	 ; case compare dim0 dim1 of
	   EQ -> return ()
	   LT -> PPL.polyhedronAddSpaceDimensionsAndEmbed p0 (dim1-dim0)
	   GT -> PPL.polyhedronAddSpaceDimensionsAndEmbed p1 (dim0-dim1)
	 }

polyhedron_adjust_dimension :: Polyhedron -> Dimension -> IO ()
polyhedron_adjust_dimension p dim
    = do { dim' <- PPL.polyhedronSpaceDimension p
	 ; when (dim'<dim) $ 
	   PPL.polyhedronAddSpaceDimensionsAndEmbed p (dim-dim')
	 }


-- add constraints to a polyhedron
polyhedron_add_constraints :: Polyhedron -> LinConSys -> IO ()
polyhedron_add_constraints p cs
    = do { ppl_cs <- mk_consys cs
	 ; dim <- PPL.constraintSystemSpaceDimension ppl_cs
	 ; polyhedron_adjust_dimension p dim
	 ; PPL.polyhedronAddConstraints p ppl_cs
	 }


-- add generators to a polyhedron
polyhedron_add_generators :: Polyhedron -> LinGenSys -> IO ()
polyhedron_add_generators p gs
    | null gs = return ()
    | otherwise 
        = do { empty <- PPL.polyhedronIsEmpty p
             ; if empty then return ()
               else do { ppl_gs <- mk_gensys gs
                       ; -- dim <- PPL.generatorSystemSpaceDimension ppl_gs
                       ; -- polyhedron_adjust_dimension p dim
                       ; PPL.polyhedronAddGenerators p ppl_gs
                       }
             }


-- intersect two polyhedra
polyhedron_intersect :: Polyhedron -> Polyhedron -> IO ()
polyhedron_intersect p q
    = do { polyhedron_adjust_dimensions p q
	 ; PPL.polyhedronIntersectionAssign p q
	 }

polyhedra_intersect :: Polyhedron -> [Polyhedron] -> IO ()
polyhedra_intersect p qs
    = sequence_ [do { polyhedron_adjust_dimensions p q
		    ; PPL.polyhedronIntersectionAssign p q
		    } | q<-qs]


-- convex hull of two polyhedra
polyhedron_hull :: Polyhedron -> Polyhedron -> IO ()
polyhedron_hull p q
    = do { polyhedron_adjust_dimensions p q
	 ; PPL.polyhedronPolyHullAssign p q
	 }


-- widening operators
-- p2 ▿ p1
polyhedron_h79_widening :: Polyhedron -> Polyhedron -> IO ()
polyhedron_h79_widening p1 p2
    = do { polyhedron_adjust_dimensions p1 p2
         ; PPL.polyhedronH79WideningAssign p1 p2
	 }
-- p2 ▿ p1
polyhedron_bhrz03_widening :: Polyhedron -> Polyhedron -> IO ()
polyhedron_bhrz03_widening p1 p2
    = do { polyhedron_adjust_dimensions p1 p2
         ; PPL.polyhedronBHRZ03WideningAssign p1 p2
	 }

-- containment check
polyhedron_contains :: Polyhedron -> Polyhedron -> IO Bool
polyhedron_contains p1 p2
    = do { polyhedron_adjust_dimensions p1 p2
	 ; PPL.polyhedronContainsPolyhedron p1 p2
	 }



-- make a constraint system from a list of constraints 
mk_consys :: LinConSys -> IO PPL.ConstraintSystem
mk_consys clist
    | null clist = 
	do { pe<-PPL.newLinearExpressionWithDimension 0
 	   ; c<-PPL.newConstraint pe PPL.Equal
 	   ; PPL.newConstraintSystemFromConstraint c
 	   }
    | otherwise = 
        do { ppl_cs<-PPL.newConstraintSystem
	   ; sequence_ [ do { pe<-PPL.newLinearExpressionWithDimension dim
			    ; mk_linear_expr lhs pe 1
			    ; mk_linear_expr rhs pe (-1)
			    ; c<-PPL.newConstraint pe r
			    ; PPL.constraintSystemInsertConstraint ppl_cs c
			    } | LinConstr (r,lhs,rhs)<-clist ]
	   ; return ppl_cs
	   }
    where dim = spaceDimension clist

-- make a generator system from a list of generators
mk_gensys :: LinGenSys -> IO PPL.GeneratorSystem
mk_gensys gs = do { ppl_gs<-PPL.newGeneratorSystem
		  ; sequence_ [do { ppl_g<-mk_generator g
				  ; PPL.generatorSystemInsertGenerator ppl_gs ppl_g }
			       | g<-gs]
		  ; return ppl_gs 
		  }

mk_generator :: LinGenerator -> IO PPL.Generator
mk_generator (Line le) 
    = do { ppl_le<-PPL.newLinearExpression
	 ; mk_linear_expr le ppl_le 1
	 ; coeff<-PPL.newCoefficientFromInteger 0  -- dummy
	 ; PPL.newGenerator ppl_le PPL.Line coeff
	 }
mk_generator (Ray le) 
    = do { ppl_le<-PPL.newLinearExpression
	 ; mk_linear_expr le ppl_le 1
	 ; coeff<-PPL.newCoefficientFromInteger 0  -- dummy
	 ; PPL.newGenerator ppl_le PPL.Ray coeff
	 }
mk_generator (Point le d) 
    = do { ppl_le<-PPL.newLinearExpression
	 ; mk_linear_expr le ppl_le 1
	 ; coeff<-PPL.newCoefficientFromInteger d -- divisor
	 ; PPL.newGenerator ppl_le PPL.Point coeff
	 }
mk_generator (ClosurePoint le d) 
    = do { ppl_le<-PPL.newLinearExpression
	 ; mk_linear_expr le ppl_le 1
	 ; coeff<-PPL.newCoefficientFromInteger d  -- divisor
	 ; PPL.newGenerator ppl_le PPL.ClosurePoint coeff
	 }

-- make a PPL LinExpression from the Haskell datatype
mk_linear_expr :: LinExpr -> PPL.LinearExpression -> Integer -> IO ()
mk_linear_expr (LinVar x) linexp fact
    = do { coef<-PPL.newCoefficientFromInteger fact
	 ; PPL.linearExpressionAddToCoefficient linexp (fromIntegral x) coef
	 }
mk_linear_expr (LinCoef n) linexp fact
    = do { coef<-PPL.newCoefficientFromInteger (n*fact)
	 ; PPL.linearExpressionAddToInhomogeneous linexp coef
	 }
mk_linear_expr (LinPlus e1 e2) linexp fact
    = do { mk_linear_expr e1 linexp fact; mk_linear_expr e2 linexp fact }
mk_linear_expr (LinProd n e) linexp fact
    =  mk_linear_expr e linexp (n*fact)


gensys_from_polyhedron :: Polyhedron -> IO LinGenSys
gensys_from_polyhedron p
    = do { ppl_gs <- PPL.polyhedronGetMinimizedGenerators p
	 ; gs_list <- PPL.genSysGenerators ppl_gs
	 ; sequence [generatorFromPPL ppl_g | ppl_g<-gs_list]
	 }




-- construct a constraint structure from a PPL constraint
-- splits terms into left and right hand side to make sign positive
constraintFromPPL :: PPL.Constraint -> IO LinConstr
constraintFromPPL cptr
    = do { n <- PPL.constraintSpaceDimension cptr
	 ; c <- PPL.newCoefficient
	 ; coefs<- sequence [ do { PPL.constraintCoefficient cptr x c
				 ; a<-PPL.coefficientToInteger c
				 ; return (a,fromIntegral x) } 
			      | x<-[0..n-1]]
	 ; PPL.constraintInhomogeneousTerm cptr c
	 ; b <- PPL.coefficientToInteger c
	 ; rel<- PPL.constraintType cptr
	 ; let lhs = sum [LinCoef a  * LinVar x | (a,x)<-coefs, a>0]
	 ; let rhs = sum [LinCoef (-a) * LinVar x | (a,x)<-coefs, a<0]
	 ; return $ if b>=0 then
	                LinConstr (rel,LinCoef b+lhs,rhs)
	            else
	                LinConstr (rel,lhs,LinCoef (-b)+rhs)
	 }

-- ditto for generators
generatorFromPPL :: PPL.Generator -> IO LinGenerator
generatorFromPPL ppl_g
    = do { n<-PPL.generatorSpaceDimension ppl_g
	 ; c<-PPL.newCoefficient
	 ; coeffs<- sequence [do { PPL.generatorCoefficient ppl_g dim c
				 ; a<-PPL.coefficientToInteger c
                                 ; return (a, fromIntegral dim)
				 } | dim<-[0..n-1]]
         ; let le = sum [LinCoef a*LinVar x | (a,x)<-coeffs, a/=0]
	 ; t <- PPL.generatorType ppl_g
	 ; case t of
	   PPL.Line -> return (Line le)
	   PPL.Ray -> return (Ray le)
	   PPL.Point -> do { PPL.generatorDivisor ppl_g c
			   ; d <- PPL.coefficientToInteger c
			   ; return (Point le d)
			   }
	   PPL.ClosurePoint -> do { PPL.generatorDivisor ppl_g c
				  ; d <- PPL.coefficientToInteger c
				  ; return (ClosurePoint le d)
				  }
	 }


-- constraint system from polyhedron
consys_from_polyhedron :: Polyhedron -> IO LinConSys
consys_from_polyhedron p 
    = do { empty <- PPL.polyhedronIsEmpty p
	 ; if empty then return [LinConstr (PPL.Equal,0,1)]
	   else do { consys <- PPL.polyhedronGetMinimizedConstraints p
		   ; ppl_cs <- PPL.conSysConstraints consys
		   ; sequence [constraintFromPPL ppl_c | ppl_c<-ppl_cs]
		   }
	 }



-- library wrapper
withPPL :: IO a -> IO a
withPPL = PPL.withParmaPolyhedraLibrary


