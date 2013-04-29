{-# LANGUAGE ForeignFunctionInterface, CPP, MagicHash, UnboxedTuples  #-}
-- Binding to the Parma Polyhedra Library -*-haskell-*-
module PPL where

import Foreign
import Foreign.C
import Control.Monad        	(liftM, when)
--import Control.Exception(bracket_, throwIO, Exception(AssertionFailed))
import qualified Control.Exception as E
--import GHC.Prim
import qualified GHC.Base as B
import GHC.IO
import GHC.Num
import GHC.Word
import GHC.Ptr
import Numeric
import GHC.Integer.GMP.Internals
import GHC.Integer.GMP.Prim
#include "/usr/include/gmp.h"
#include <ppl_c.h>
#include "errhandler.h"

toBig :: Integer -> Integer
toBig (S## i) = 
  let (## s,d ##) = int2Integer## i
  in  J## s d
toBig i@(J## _ _) = i

checkRetVal :: String -> IO CInt -> IO ()
checkRetVal location act= do
  msg <- extractErr
  when (msg/="") $ putStrLn ("previous error: "++msg) 
  res <- act
  when (res<0) $ do
    msg <- extractErr
    throwIO $ E.AssertionFailed (makeErrMsg res++"\n"++msg)
  where
    makeErrMsg (#{const PPL_ERROR_OUT_OF_MEMORY}) =
	"PPL: out of memory in "++location
    makeErrMsg (#{const PPL_ERROR_INVALID_ARGUMENT}) =
	"PPL: invalid argument in "++location
    makeErrMsg (#{const PPL_ERROR_INTERNAL_ERROR}) =
	"PPL: internal error in "++location
    makeErrMsg (#{const PPL_ERROR_UNKNOWN_STANDARD_EXCEPTION}) =
	"PPL: unknown standard exception in "++location
    makeErrMsg (#{const PPL_ERROR_UNEXPECTED_ERROR}) =
	"PPL: unexpected error in "++location
    extractErr :: IO String
    extractErr = do
      lastErrorPtr <- peek lastError
      if lastErrorPtr==nullPtr then return "" else
	liftM (\str -> "("++str++")") $ peekCString lastErrorPtr

foreign import ccall "&lastError" lastError :: Ptr CString

type Dimension = #{type ppl_dimension_type}

-- | A Coefficient.
newtype Coefficient =
    Coefficient { unCoefficient :: ForeignPtr Coefficient }

-- | A linear expression.
newtype LinearExpression =
    LinearExpression { unLinearExpression :: ForeignPtr LinearExpression }

-- | An inequality.
newtype Constraint =
    Constraint { unConstraint :: ForeignPtr Constraint }

-- | A set of constraints.
newtype ConstraintSystem =
    ConstraintSystem { unConstraintSystem :: ForeignPtr ConstraintSystem }

-- | An interator for traversing a constraint system.
newtype ConstraintSystemIterator =
    ConstraintSystemIterator { unConstraintSystemIterator :: ForeignPtr ConstraintSystemIterator }

-- | A generator (i.e. a point, ray or line).
newtype Generator =
    Generator { unGenerator :: ForeignPtr Generator }

-- | A set of generators.
newtype GeneratorSystem =
    GeneratorSystem { unGeneratorSystem :: ForeignPtr GeneratorSystem }

-- | An iterator for traversion a set of generators.
newtype GeneratorSystemIterator =
    GeneratorSystemIterator { unGeneratorSystemIterator :: ForeignPtr GeneratorSystemIterator }

-- | A polyhedron.
newtype Polyhedron =
    Polyhedron { unPolyhedron :: ForeignPtr Polyhedron }


-- | A bounding box (added by pbv)
newtype BoundingBox = 
    BoundingBox { unBoundingBox :: ForeignPtr BoundingBox }

-- | Relational operator in constraints.
data ConstraintType
  = Less
  | LessOrEqual
  | Equal
  | GreaterOrEqual
  | Greater
    deriving (Eq,Ord)

{-Yiming:
    Change PPL_CONSTRAINT_TYPE_LESS_THAN_OR_EQUAL
        To PPL_CONSTRAINT_TYPE_LESS_OR_EQUAL;
    Change PPL_CONSTRAINT_TYPE_GREATER_THAN_OR_EQUAL
        To PPL_CONSTRAINT_TYPE_GREATER_OR_EQUAL;
-}
instance Enum ConstraintType where
  toEnum #{const PPL_CONSTRAINT_TYPE_LESS_THAN } = Less
  toEnum #{const PPL_CONSTRAINT_TYPE_LESS_OR_EQUAL } = LessOrEqual
  toEnum #{const PPL_CONSTRAINT_TYPE_EQUAL } = Equal
  toEnum #{const PPL_CONSTRAINT_TYPE_GREATER_OR_EQUAL } = GreaterOrEqual
  toEnum #{const PPL_CONSTRAINT_TYPE_GREATER_THAN } = Greater

  fromEnum Less = #{const PPL_CONSTRAINT_TYPE_LESS_THAN }
  fromEnum LessOrEqual = #{const PPL_CONSTRAINT_TYPE_LESS_OR_EQUAL }
  fromEnum Equal = #{const PPL_CONSTRAINT_TYPE_EQUAL }
  fromEnum GreaterOrEqual = #{const PPL_CONSTRAINT_TYPE_GREATER_OR_EQUAL }
  fromEnum Greater = #{const PPL_CONSTRAINT_TYPE_GREATER_THAN }

instance Show ConstraintType where
  show Less = "<"
  show LessOrEqual = "<="
  show Equal = "="
  show GreaterOrEqual = ">="
  show Greater = ">"

data GeneratorType
  = Line
  | Ray
  | Point
  | ClosurePoint
    deriving Eq

instance Enum GeneratorType where
  toEnum #{const PPL_GENERATOR_TYPE_LINE } = Line
  toEnum #{const PPL_GENERATOR_TYPE_RAY } = Ray
  toEnum #{const PPL_GENERATOR_TYPE_POINT } = Point
  toEnum #{const PPL_GENERATOR_TYPE_CLOSURE_POINT } = ClosurePoint

  fromEnum Line = #{const PPL_GENERATOR_TYPE_LINE }
  fromEnum Ray = #{const PPL_GENERATOR_TYPE_RAY }
  fromEnum Point = #{const PPL_GENERATOR_TYPE_POINT }
  fromEnum ClosurePoint = #{const PPL_GENERATOR_TYPE_CLOSURE_POINT }

-- dummy Show instance for polyhedrons 
-- (for convenience showing tuples, etc.)
instance Show Polyhedron where
    showsPrec _ _ = ("PPL.Polyhedron"++)


-- | All operations on the library must be done within this wrapper.
--
withParmaPolyhedraLibrary :: IO a -> IO a
withParmaPolyhedraLibrary action = do
  checkRetVal "Withparmapolyhedralibrary" ppl_initialize
  ppl_set_error_handler errHandler
  res <- action
  checkRetVal "withParmaPolyhedraLibrary" ppl_finalize
  return res

foreign import ccall unsafe "ppl_initialize" ppl_initialize :: IO CInt
foreign import ccall unsafe "ppl_finalize" ppl_finalize :: IO CInt
foreign import ccall unsafe "ppl_set_error_handler" 
  ppl_set_error_handler :: FunPtr () -> IO CInt
foreign import ccall unsafe "&errorHandler" errHandler :: FunPtr ()


-- A dummy data type for the external mpz_struct.
data MpzStruct = MpzStruct

-- To use the guts of an integer in C land, we need to copy the data of the
-- number from the GCed Haskell land to safer C land.
withInteger :: Integer -> (Ptr MpzStruct -> IO a) -> IO a
withInteger (J## size## barr##) act = do
  -- Make space for a mpz_t struct.
  allocaBytes #{const sizeof(__mpz_struct)} $ \mpzPtr -> do
    let bytesInLimbs = B.I## (B.sizeofByteArray## barr##)
    let limbs@(B.I## limbs##) = bytesInLimbs `div` #{const sizeof(mp_limb_t)}
    -- Make space for a limb as big as the one of the Integer.
    allocaBytes bytesInLimbs $ \limbPtr@(Ptr addr##) -> do
      -- Copy the limb of the Integer to the newly created mpz_t.
      let
	copy## :: B.Int## -> B.State## B.RealWorld -> (## B.State## B.RealWorld, () ##)
        copy## 0## s = (## s, () ##)
	copy## i## s =
          case i## B.-## 1## of { i'## ->
	    case B.indexWord#{const 8*sizeof(mp_limb_t)}Array## 
	      barr## i'## of { val## ->
	      case B.writeWord#{const 8*sizeof(mp_limb_t)}OffAddr##
		addr## i'## val## s of { s' ->
		copy## i'## s'
	      }
	    }
	  }
      IO $ \s -> copy## limbs## s
      -- Fill the structure
      #{poke __mpz_struct, _mp_alloc} (castPtr mpzPtr) (fromIntegral limbs :: CInt)
      #{poke __mpz_struct, _mp_size} (castPtr mpzPtr) (fromIntegral (B.I## size##) :: CInt)
      #{poke __mpz_struct, _mp_d} (castPtr mpzPtr) limbPtr
      act mpzPtr
withInteger small act = withInteger (toBig small) act


-- To transfer an integer to Haskell land we allocate an mpz_t
-- structure, initialize it, send it to the C function,
-- then copy its content to Haskell and finally free it.
extractInteger :: (Ptr MpzStruct -> IO a) -> IO (Integer, a)
extractInteger act = 
  -- Make space for a mpz_t struct.
  allocaBytes #{const sizeof(__mpz_struct)} $ \mpzPtr ->
    E.bracket_ (mpz_init mpzPtr) (mpz_clear mpzPtr) $ do
      res <- act mpzPtr
      -- Extract the contents of the mpz_t to a Haskell integer.
      c_alloc <- #{peek __mpz_struct, _mp_alloc} (castPtr mpzPtr) :: IO CInt
      c_size <- #{peek __mpz_struct, _mp_size} (castPtr mpzPtr) :: IO CInt
      (Ptr addr##) <- #{peek __mpz_struct, _mp_d} (castPtr mpzPtr)
      let (B.I## alloc##) = fromIntegral c_alloc
      let (B.I## size##) = fromIntegral c_size
      val <- IO $ \s## ->
	case B.newByteArray## (alloc## B.*## #{const sizeof(mp_limb_t)}##) s## of
          { (## s1##, mutarr## ##) ->
	    let
	      copy## :: B.Int## -> B.State## B.RealWorld -> B.State## B.RealWorld
	      copy## 0## s## = s##
	      copy## i## s##=
	        let
	          i'##  = i## B.-## 1##
	          val## = B.indexWord#{const 8*sizeof(mp_limb_t)}OffAddr## 
			    addr## i'##
	          s'##  = B.writeWord#{const 8*sizeof(mp_limb_t)}Array## 
			    mutarr## i'## val## s##
	        in copy## i'## s'##
            in
	      case copy## alloc## s1## of { s2## ->
	        case B.unsafeFreezeByteArray## mutarr## s2## of {
	          (## s3##, barr## ##) -> (## s3##, J## size## barr## ##)
	        }
	      }
           }
      return (val, res)

foreign import ccall unsafe "__gmpz_init" mpz_init :: Ptr MpzStruct -> IO ()
foreign import ccall unsafe "__gmpz_clear" mpz_clear :: Ptr MpzStruct -> IO ()


conSysConstraints :: ConstraintSystem -> IO [Constraint]
conSysConstraints (ConstraintSystem csFPtr) = withForeignPtr csFPtr $ \csPtr -> 
  alloca $ \iterPtrPtr -> alloca $ \endPtrPtr -> do
  ppl_new_Constraint_System_const_iterator iterPtrPtr
  iterPtr <- peek iterPtrPtr
  ppl_Constraint_System_begin csPtr iterPtr
  ppl_new_Constraint_System_const_iterator endPtrPtr
  endPtr <- peek endPtrPtr
  ppl_Constraint_System_end csPtr endPtr
  cons <- iterate iterPtr endPtr
  delete_Constraint_System_const_iterator iterPtr
  delete_Constraint_System_const_iterator endPtr
  mapM (\res -> liftM Constraint $ newForeignPtr deleteConstraint res) cons
  where
    iterate iterPtr endPtr = do
      equal <- ppl_Constraint_System_const_iterator_equal_test iterPtr endPtr
      if equal/=0 then return [] else alloca $ \cPtrPtr -> do
        ppl_Constraint_System_const_iterator_dereference iterPtr cPtrPtr
        cPtr <- peek cPtrPtr
	ppl_new_Constraint_from_Constraint cPtrPtr cPtr
	cPtr <- peek cPtrPtr
        ppl_Constraint_System_const_iterator_increment iterPtr
	cPtrs <- iterate iterPtr endPtr
        return (cPtr:cPtrs)

foreign import ccall unsafe "ppl_delete_Constraint_System_const_iterator"
  delete_Constraint_System_const_iterator :: Ptr ConstraintSystemIterator -> IO ()

genSysGenerators :: GeneratorSystem -> IO [Generator]
genSysGenerators (GeneratorSystem csFPtr) = withForeignPtr csFPtr $ \csPtr -> 
  alloca $ \iterPtrPtr -> alloca $ \endPtrPtr -> do
  ppl_new_Generator_System_const_iterator iterPtrPtr
  iterPtr <- peek iterPtrPtr
  ppl_Generator_System_begin csPtr iterPtr
  ppl_new_Generator_System_const_iterator endPtrPtr
  endPtr <- peek endPtrPtr
  ppl_Generator_System_end csPtr endPtr
  cons <- iterate iterPtr endPtr
  delete_Generator_System_const_iterator iterPtr
  delete_Generator_System_const_iterator endPtr
  mapM (\res -> liftM Generator $ newForeignPtr deleteGenerator res) cons
  where
    iterate iterPtr endPtr = do
      equal <- ppl_Generator_System_const_iterator_equal_test iterPtr endPtr
      if equal/=0 then return [] else alloca $ \cPtrPtr -> do
        ppl_Generator_System_const_iterator_dereference iterPtr cPtrPtr
        cPtr <- peek cPtrPtr
	ppl_new_Generator_from_Generator cPtrPtr cPtr
	cPtr <- peek cPtrPtr
        ppl_Generator_System_const_iterator_increment iterPtr
	cPtrs <- iterate iterPtr endPtr
        return (cPtr:cPtrs)

foreign import ccall unsafe "ppl_delete_Generator_System_const_iterator"
  delete_Generator_System_const_iterator :: Ptr GeneratorSystemIterator -> IO ()

-- TODO: write binding for initialize
-- TODO: write binding for finalize
-- TODO: write binding for set_rounding_for_PPL
-- TODO: write binding for restore_pre_PPL_rounding
irrationalPrecision :: 
  IO Int
irrationalPrecision = do
  alloca $ \mArg0 -> do
  checkRetVal "irrationalPrecision" $ 
    ppl_irrational_precision mArg0
  liftM fromIntegral $ peek mArg0

foreign import ccall unsafe "ppl_irrational_precision"
  ppl_irrational_precision :: Ptr (#{type unsigned}) -> IO CInt


setIrrationalPrecision :: 
  Int ->
  IO ()
setIrrationalPrecision arg0 = do
  let mArg0 = fromIntegral arg0
  checkRetVal "setIrrationalPrecision" $ 
    ppl_set_irrational_precision mArg0

foreign import ccall unsafe "ppl_set_irrational_precision"
  ppl_set_irrational_precision :: #{type unsigned} -> IO CInt


-- TODO: write binding for version_major
-- TODO: write binding for version_minor
-- TODO: write binding for version_revision
-- TODO: write binding for version_beta
-- TODO: write binding for version
-- TODO: write binding for banner
-- TODO: write binding for set_error_handler
setTimeout :: 
  Int ->
  IO ()
setTimeout arg0 = do
  let mArg0 = fromIntegral arg0
  checkRetVal "setTimeout" $ 
    ppl_set_timeout mArg0

foreign import ccall unsafe "ppl_set_timeout"
  ppl_set_timeout :: #{type unsigned} -> IO CInt


-- TODO: write binding for reset_timeout
setDeterministicTimeout :: 
  Int ->
  IO ()
setDeterministicTimeout arg0 = do
  let mArg0 = fromIntegral arg0
  checkRetVal "setDeterministicTimeout" $ 
    ppl_set_deterministic_timeout mArg0

foreign import ccall unsafe "ppl_set_deterministic_timeout"
  ppl_set_deterministic_timeout :: #{type unsigned} -> IO CInt


-- TODO: write binding for reset_deterministic_timeout
maxSpaceDimension :: 
  IO Dimension
maxSpaceDimension = do
  alloca $ \mArg0 -> do
  checkRetVal "maxSpaceDimension" $ 
    ppl_max_space_dimension mArg0
  liftM fromIntegral $ peek mArg0

foreign import ccall unsafe "ppl_max_space_dimension"
  ppl_max_space_dimension :: Ptr (#{type ppl_dimension_type}) -> IO CInt


notADimension :: 
  IO Dimension
notADimension = do
  alloca $ \mArg0 -> do
  checkRetVal "notADimension" $ 
    ppl_not_a_dimension mArg0
  liftM fromIntegral $ peek mArg0

foreign import ccall unsafe "ppl_not_a_dimension"
  ppl_not_a_dimension :: Ptr (#{type ppl_dimension_type}) -> IO CInt


ioPrintVariable :: 
  Dimension ->
  IO ()
ioPrintVariable arg0 = do
  let mArg0 = fromIntegral arg0
  checkRetVal "ioPrintVariable" $ 
    ppl_io_print_variable mArg0

foreign import ccall unsafe "ppl_io_print_variable"
  ppl_io_print_variable :: #{type ppl_dimension_type} -> IO CInt


-- TODO: write binding for io_fprint_variable
-- TODO: write binding for io_asprint_variable
-- TODO: write binding for io_set_variable_output_function
-- TODO: write binding for io_get_variable_output_function
newCoefficient :: 
  IO Coefficient
newCoefficient = do
  alloca $ \mArg0 -> do
  checkRetVal "newCoefficient" $ 
    ppl_new_Coefficient mArg0
  res <- peek mArg0
  liftM Coefficient $ newForeignPtr deleteCoefficient res

foreign import ccall unsafe "ppl_new_Coefficient"
  ppl_new_Coefficient :: Ptr (Ptr Coefficient) -> IO CInt


newCoefficientFromInteger :: Integer -> IO Coefficient
newCoefficientFromInteger arg1 =
  alloca $ \mArg0 -> withInteger arg1 $ \mArg1 -> do
  checkRetVal "newCoefficientFromInteger" $ 
    ppl_new_Coefficient_from_mpz_t mArg0 mArg1
  res <- peek mArg0
  liftM Coefficient $ newForeignPtr deleteCoefficient res

foreign import ccall unsafe "ppl_new_Coefficient_from_mpz_t"
  ppl_new_Coefficient_from_mpz_t :: Ptr (Ptr Coefficient) ->
                                    Ptr MpzStruct -> IO CInt
newCoefficientFromCoefficient :: 
  Coefficient ->
  IO Coefficient
newCoefficientFromCoefficient (Coefficient arg1) = do
  alloca $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "newCoefficientFromCoefficient" $ 
    ppl_new_Coefficient_from_Coefficient mArg0 mArg1
  res <- peek mArg0
  liftM Coefficient $ newForeignPtr deleteCoefficient res

foreign import ccall unsafe "ppl_new_Coefficient_from_Coefficient"
  ppl_new_Coefficient_from_Coefficient :: Ptr (Ptr Coefficient) -> Ptr Coefficient -> IO CInt


assignCoefficientFromInteger :: Coefficient -> Integer -> IO ()
assignCoefficientFromInteger (Coefficient arg0) arg1 =
  withForeignPtr arg0 $ \mArg0 -> withInteger arg1 $ \mArg1 ->
  checkRetVal "assignCoefficientFromInteger" $ 
    ppl_assign_Coefficient_from_mpz_t mArg0 mArg1

foreign import ccall unsafe "ppl_assign_Coefficient_from_mpz_t"
  ppl_assign_Coefficient_from_mpz_t :: Ptr Coefficient ->
                                       Ptr MpzStruct -> IO CInt
assignCoefficientFromCoefficient :: 
  Coefficient ->
  Coefficient ->
  IO ()
assignCoefficientFromCoefficient (Coefficient arg0) (Coefficient arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "assignCoefficientFromCoefficient" $ 
    ppl_assign_Coefficient_from_Coefficient mArg0 mArg1

foreign import ccall unsafe "ppl_assign_Coefficient_from_Coefficient"
  ppl_assign_Coefficient_from_Coefficient :: Ptr Coefficient -> Ptr Coefficient -> IO CInt


foreign import ccall unsafe "&ppl_delete_Coefficient"
  deleteCoefficient :: FinalizerPtr Coefficient

coefficientToInteger :: Coefficient -> IO Integer
coefficientToInteger (Coefficient arg0) =
  withForeignPtr arg0 $ \mArg0 -> do
    (val, res) <- extractInteger (ppl_Coefficient_to_mpz_t mArg0)
    checkRetVal "coefficientToInteger" $ return res
    return val

foreign import ccall unsafe "ppl_Coefficient_to_mpz_t"
  ppl_Coefficient_to_mpz_t :: Ptr Coefficient ->
                              Ptr MpzStruct -> IO CInt
coefficientOK :: 
  Coefficient ->
  IO Bool
coefficientOK (Coefficient arg0) = do
  withForeignPtr arg0 $ \mArg0 -> do
  liftM (toBool.fromIntegral) $ 
    ppl_Coefficient_OK mArg0

foreign import ccall unsafe "ppl_Coefficient_OK"
  ppl_Coefficient_OK :: Ptr Coefficient -> IO CInt


-- TODO: write binding for Coefficient_is_bounded
-- TODO: write binding for Coefficient_min
-- TODO: write binding for Coefficient_max
newLinearExpression :: 
  IO LinearExpression
newLinearExpression = do
  alloca $ \mArg0 -> do
  checkRetVal "newLinearExpression" $ 
    ppl_new_Linear_Expression mArg0
  res <- peek mArg0
  liftM LinearExpression $ newForeignPtr deleteLinearExpression res

foreign import ccall unsafe "ppl_new_Linear_Expression"
  ppl_new_Linear_Expression :: Ptr (Ptr LinearExpression) -> IO CInt


newLinearExpressionWithDimension :: 
  Dimension ->
  IO LinearExpression
newLinearExpressionWithDimension arg1 = do
  alloca $ \mArg0 -> do
  let mArg1 = fromIntegral arg1
  checkRetVal "newLinearExpressionWithDimension" $ 
    ppl_new_Linear_Expression_with_dimension mArg0 mArg1
  res <- peek mArg0
  liftM LinearExpression $ newForeignPtr deleteLinearExpression res

foreign import ccall unsafe "ppl_new_Linear_Expression_with_dimension"
  ppl_new_Linear_Expression_with_dimension :: Ptr (Ptr LinearExpression) -> #{type ppl_dimension_type} -> IO CInt


newLinearExpressionFromLinearExpression :: 
  LinearExpression ->
  IO LinearExpression
newLinearExpressionFromLinearExpression (LinearExpression arg1) = do
  alloca $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "newLinearExpressionFromLinearExpression" $ 
    ppl_new_Linear_Expression_from_Linear_Expression mArg0 mArg1
  res <- peek mArg0
  liftM LinearExpression $ newForeignPtr deleteLinearExpression res

foreign import ccall unsafe "ppl_new_Linear_Expression_from_Linear_Expression"
  ppl_new_Linear_Expression_from_Linear_Expression :: Ptr (Ptr LinearExpression) -> Ptr LinearExpression -> IO CInt


newLinearExpressionFromConstraint :: 
  Constraint ->
  IO LinearExpression
newLinearExpressionFromConstraint (Constraint arg1) = do
  alloca $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "newLinearExpressionFromConstraint" $ 
    ppl_new_Linear_Expression_from_Constraint mArg0 mArg1
  res <- peek mArg0
  liftM LinearExpression $ newForeignPtr deleteLinearExpression res

foreign import ccall unsafe "ppl_new_Linear_Expression_from_Constraint"
  ppl_new_Linear_Expression_from_Constraint :: Ptr (Ptr LinearExpression) -> Ptr Constraint -> IO CInt


newLinearExpressionFromGenerator :: 
  Generator ->
  IO LinearExpression
newLinearExpressionFromGenerator (Generator arg1) = do
  alloca $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "newLinearExpressionFromGenerator" $ 
    ppl_new_Linear_Expression_from_Generator mArg0 mArg1
  res <- peek mArg0
  liftM LinearExpression $ newForeignPtr deleteLinearExpression res

foreign import ccall unsafe "ppl_new_Linear_Expression_from_Generator"
  ppl_new_Linear_Expression_from_Generator :: Ptr (Ptr LinearExpression) -> Ptr Generator -> IO CInt


-- TODO: write binding for new_Linear_Expression_from_Congruence
-- TODO: write binding for new_Linear_Expression_from_Grid_Generator
assignLinearExpressionFromLinearExpression :: 
  LinearExpression ->
  LinearExpression ->
  IO ()
assignLinearExpressionFromLinearExpression (LinearExpression arg0) (LinearExpression arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "assignLinearExpressionFromLinearExpression" $ 
    ppl_assign_Linear_Expression_from_Linear_Expression mArg0 mArg1

foreign import ccall unsafe "ppl_assign_Linear_Expression_from_Linear_Expression"
  ppl_assign_Linear_Expression_from_Linear_Expression :: Ptr LinearExpression -> Ptr LinearExpression -> IO CInt


foreign import ccall unsafe "&ppl_delete_Linear_Expression"
  deleteLinearExpression :: FinalizerPtr LinearExpression

linearExpressionSpaceDimension :: 
  LinearExpression ->
  IO Dimension
linearExpressionSpaceDimension (LinearExpression arg0) = do
  withForeignPtr arg0 $ \mArg0 -> do
  alloca $ \mArg1 -> do
  checkRetVal "linearExpressionSpaceDimension" $ 
    ppl_Linear_Expression_space_dimension mArg0 mArg1
  liftM fromIntegral $ peek mArg1

foreign import ccall unsafe "ppl_Linear_Expression_space_dimension"
  ppl_Linear_Expression_space_dimension :: Ptr LinearExpression -> Ptr (#{type ppl_dimension_type}) -> IO CInt


linearExpressionCoefficient :: 
  LinearExpression ->
  Dimension ->
  Coefficient ->
  IO ()
linearExpressionCoefficient (LinearExpression arg0) arg1 (Coefficient arg2) = do
  withForeignPtr arg0 $ \mArg0 -> do
  let mArg1 = fromIntegral arg1
  withForeignPtr arg2 $ \mArg2 -> do
  checkRetVal "linearExpressionCoefficient" $ 
    ppl_Linear_Expression_coefficient mArg0 mArg1 mArg2

foreign import ccall unsafe "ppl_Linear_Expression_coefficient"
  ppl_Linear_Expression_coefficient :: Ptr LinearExpression -> #{type ppl_dimension_type} -> Ptr Coefficient -> IO CInt


linearExpressionInhomogeneousTerm :: 
  LinearExpression ->
  Coefficient ->
  IO ()
linearExpressionInhomogeneousTerm (LinearExpression arg0) (Coefficient arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "linearExpressionInhomogeneousTerm" $ 
    ppl_Linear_Expression_inhomogeneous_term mArg0 mArg1

foreign import ccall unsafe "ppl_Linear_Expression_inhomogeneous_term"
  ppl_Linear_Expression_inhomogeneous_term :: Ptr LinearExpression -> Ptr Coefficient -> IO CInt


linearExpressionOK :: 
  LinearExpression ->
  IO Bool
linearExpressionOK (LinearExpression arg0) = do
  withForeignPtr arg0 $ \mArg0 -> do
  liftM (toBool.fromIntegral) $ 
    ppl_Linear_Expression_OK mArg0

foreign import ccall unsafe "ppl_Linear_Expression_OK"
  ppl_Linear_Expression_OK :: Ptr LinearExpression -> IO CInt


linearExpressionIsZero :: 
  LinearExpression ->
  IO ()
linearExpressionIsZero (LinearExpression arg0) = do
  withForeignPtr arg0 $ \mArg0 -> do
  checkRetVal "linearExpressionIsZero" $ 
    ppl_Linear_Expression_is_zero mArg0

foreign import ccall unsafe "ppl_Linear_Expression_is_zero"
  ppl_Linear_Expression_is_zero :: Ptr LinearExpression -> IO CInt


linearExpressionAllHomogeneousTermsAreZero :: 
  LinearExpression ->
  IO ()
linearExpressionAllHomogeneousTermsAreZero (LinearExpression arg0) = do
  withForeignPtr arg0 $ \mArg0 -> do
  checkRetVal "linearExpressionAllHomogeneousTermsAreZero" $ 
    ppl_Linear_Expression_all_homogeneous_terms_are_zero mArg0

foreign import ccall unsafe "ppl_Linear_Expression_all_homogeneous_terms_are_zero"
  ppl_Linear_Expression_all_homogeneous_terms_are_zero :: Ptr LinearExpression -> IO CInt


linearExpressionAddToCoefficient :: 
  LinearExpression ->
  Dimension ->
  Coefficient ->
  IO ()
linearExpressionAddToCoefficient (LinearExpression arg0) arg1 (Coefficient arg2) = do
  withForeignPtr arg0 $ \mArg0 -> do
  let mArg1 = fromIntegral arg1
  withForeignPtr arg2 $ \mArg2 -> do
  checkRetVal "linearExpressionAddToCoefficient" $ 
    ppl_Linear_Expression_add_to_coefficient mArg0 mArg1 mArg2

foreign import ccall unsafe "ppl_Linear_Expression_add_to_coefficient"
  ppl_Linear_Expression_add_to_coefficient :: Ptr LinearExpression -> #{type ppl_dimension_type} -> Ptr Coefficient -> IO CInt


linearExpressionAddToInhomogeneous :: 
  LinearExpression ->
  Coefficient ->
  IO ()
linearExpressionAddToInhomogeneous (LinearExpression arg0) (Coefficient arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "linearExpressionAddToInhomogeneous" $ 
    ppl_Linear_Expression_add_to_inhomogeneous mArg0 mArg1

foreign import ccall unsafe "ppl_Linear_Expression_add_to_inhomogeneous"
  ppl_Linear_Expression_add_to_inhomogeneous :: Ptr LinearExpression -> Ptr Coefficient -> IO CInt


addLinearExpressionToLinearExpression :: 
  LinearExpression ->
  LinearExpression ->
  IO ()
addLinearExpressionToLinearExpression (LinearExpression arg0) (LinearExpression arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "addLinearExpressionToLinearExpression" $ 
    ppl_add_Linear_Expression_to_Linear_Expression mArg0 mArg1

foreign import ccall unsafe "ppl_add_Linear_Expression_to_Linear_Expression"
  ppl_add_Linear_Expression_to_Linear_Expression :: Ptr LinearExpression -> Ptr LinearExpression -> IO CInt


subtractLinearExpressionFromLinearExpression :: 
  LinearExpression ->
  LinearExpression ->
  IO ()
subtractLinearExpressionFromLinearExpression (LinearExpression arg0) (LinearExpression arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "subtractLinearExpressionFromLinearExpression" $ 
    ppl_subtract_Linear_Expression_from_Linear_Expression mArg0 mArg1

foreign import ccall unsafe "ppl_subtract_Linear_Expression_from_Linear_Expression"
  ppl_subtract_Linear_Expression_from_Linear_Expression :: Ptr LinearExpression -> Ptr LinearExpression -> IO CInt


multiplyLinearExpressionByCoefficient :: 
  LinearExpression ->
  Coefficient ->
  IO ()
multiplyLinearExpressionByCoefficient (LinearExpression arg0) (Coefficient arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "multiplyLinearExpressionByCoefficient" $ 
    ppl_multiply_Linear_Expression_by_Coefficient mArg0 mArg1

foreign import ccall unsafe "ppl_multiply_Linear_Expression_by_Coefficient"
  ppl_multiply_Linear_Expression_by_Coefficient :: Ptr LinearExpression -> Ptr Coefficient -> IO CInt


newConstraint :: 
  LinearExpression ->
  ConstraintType ->
  IO Constraint
newConstraint (LinearExpression arg1) arg2 = do
  alloca $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  let mArg2 = (fromIntegral . fromEnum) arg2
  checkRetVal "newConstraint" $ 
    ppl_new_Constraint mArg0 mArg1 mArg2
  res <- peek mArg0
  liftM Constraint $ newForeignPtr deleteConstraint res

foreign import ccall unsafe "ppl_new_Constraint"
  ppl_new_Constraint :: Ptr (Ptr Constraint) -> Ptr LinearExpression -> #{type int} -> IO CInt


newConstraintZeroDimFalse :: 
  IO Constraint
newConstraintZeroDimFalse = do
  alloca $ \mArg0 -> do
  checkRetVal "newConstraintZeroDimFalse" $ 
    ppl_new_Constraint_zero_dim_false mArg0
  res <- peek mArg0
  liftM Constraint $ newForeignPtr deleteConstraint res

foreign import ccall unsafe "ppl_new_Constraint_zero_dim_false"
  ppl_new_Constraint_zero_dim_false :: Ptr (Ptr Constraint) -> IO CInt


newConstraintZeroDimPositivity :: 
  IO Constraint
newConstraintZeroDimPositivity = do
  alloca $ \mArg0 -> do
  checkRetVal "newConstraintZeroDimPositivity" $ 
    ppl_new_Constraint_zero_dim_positivity mArg0
  res <- peek mArg0
  liftM Constraint $ newForeignPtr deleteConstraint res

foreign import ccall unsafe "ppl_new_Constraint_zero_dim_positivity"
  ppl_new_Constraint_zero_dim_positivity :: Ptr (Ptr Constraint) -> IO CInt


newConstraintFromConstraint :: 
  Constraint ->
  IO Constraint
newConstraintFromConstraint (Constraint arg1) = do
  alloca $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "newConstraintFromConstraint" $ 
    ppl_new_Constraint_from_Constraint mArg0 mArg1
  res <- peek mArg0
  liftM Constraint $ newForeignPtr deleteConstraint res

foreign import ccall unsafe "ppl_new_Constraint_from_Constraint"
  ppl_new_Constraint_from_Constraint :: Ptr (Ptr Constraint) -> Ptr Constraint -> IO CInt


assignConstraintFromConstraint :: 
  Constraint ->
  Constraint ->
  IO ()
assignConstraintFromConstraint (Constraint arg0) (Constraint arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "assignConstraintFromConstraint" $ 
    ppl_assign_Constraint_from_Constraint mArg0 mArg1

foreign import ccall unsafe "ppl_assign_Constraint_from_Constraint"
  ppl_assign_Constraint_from_Constraint :: Ptr Constraint -> Ptr Constraint -> IO CInt


foreign import ccall unsafe "&ppl_delete_Constraint"
  deleteConstraint :: FinalizerPtr Constraint

constraintSpaceDimension :: 
  Constraint ->
  IO Dimension
constraintSpaceDimension (Constraint arg0) = do
  withForeignPtr arg0 $ \mArg0 -> do
  alloca $ \mArg1 -> do
  checkRetVal "constraintSpaceDimension" $ 
    ppl_Constraint_space_dimension mArg0 mArg1
  liftM fromIntegral $ peek mArg1

foreign import ccall unsafe "ppl_Constraint_space_dimension"
  ppl_Constraint_space_dimension :: Ptr Constraint -> Ptr (#{type ppl_dimension_type}) -> IO CInt


constraintType ::
  Constraint ->
  IO ConstraintType
constraintType (Constraint arg0) = do
  withForeignPtr arg0 $ \mArg0 -> do
  liftM (toEnum . fromIntegral) $ 
    ppl_Constraint_type mArg0

foreign import ccall unsafe "ppl_Constraint_type"
  ppl_Constraint_type :: Ptr Constraint -> IO CInt
constraintCoefficient :: 
  Constraint ->
  Dimension ->
  Coefficient ->
  IO ()
constraintCoefficient (Constraint arg0) arg1 (Coefficient arg2) = do
  withForeignPtr arg0 $ \mArg0 -> do
  let mArg1 = fromIntegral arg1
  withForeignPtr arg2 $ \mArg2 -> do
  checkRetVal "constraintCoefficient" $ 
    ppl_Constraint_coefficient mArg0 mArg1 mArg2

foreign import ccall unsafe "ppl_Constraint_coefficient"
  ppl_Constraint_coefficient :: Ptr Constraint -> #{type ppl_dimension_type} -> Ptr Coefficient -> IO CInt


constraintInhomogeneousTerm :: 
  Constraint ->
  Coefficient ->
  IO ()
constraintInhomogeneousTerm (Constraint arg0) (Coefficient arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "constraintInhomogeneousTerm" $ 
    ppl_Constraint_inhomogeneous_term mArg0 mArg1

foreign import ccall unsafe "ppl_Constraint_inhomogeneous_term"
  ppl_Constraint_inhomogeneous_term :: Ptr Constraint -> Ptr Coefficient -> IO CInt


constraintOK :: 
  Constraint ->
  IO Bool
constraintOK (Constraint arg0) = do
  withForeignPtr arg0 $ \mArg0 -> do
  liftM (toBool.fromIntegral) $ 
    ppl_Constraint_OK mArg0

foreign import ccall unsafe "ppl_Constraint_OK"
  ppl_Constraint_OK :: Ptr Constraint -> IO CInt


newConstraintSystem :: 
  IO ConstraintSystem
newConstraintSystem = do
  alloca $ \mArg0 -> do
  checkRetVal "newConstraintSystem" $ 
    ppl_new_Constraint_System mArg0
  res <- peek mArg0
  liftM ConstraintSystem $ newForeignPtr deleteConstraintSystem res

foreign import ccall unsafe "ppl_new_Constraint_System"
  ppl_new_Constraint_System :: Ptr (Ptr ConstraintSystem) -> IO CInt


newConstraintSystemZeroDimEmpty :: 
  IO ConstraintSystem
newConstraintSystemZeroDimEmpty = do
  alloca $ \mArg0 -> do
  checkRetVal "newConstraintSystemZeroDimEmpty" $ 
    ppl_new_Constraint_System_zero_dim_empty mArg0
  res <- peek mArg0
  liftM ConstraintSystem $ newForeignPtr deleteConstraintSystem res

foreign import ccall unsafe "ppl_new_Constraint_System_zero_dim_empty"
  ppl_new_Constraint_System_zero_dim_empty :: Ptr (Ptr ConstraintSystem) -> IO CInt


newConstraintSystemFromConstraint :: 
  Constraint ->
  IO ConstraintSystem
newConstraintSystemFromConstraint (Constraint arg1) = do
  alloca $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "newConstraintSystemFromConstraint" $ 
    ppl_new_Constraint_System_from_Constraint mArg0 mArg1
  res <- peek mArg0
  liftM ConstraintSystem $ newForeignPtr deleteConstraintSystem res

foreign import ccall unsafe "ppl_new_Constraint_System_from_Constraint"
  ppl_new_Constraint_System_from_Constraint :: Ptr (Ptr ConstraintSystem) -> Ptr Constraint -> IO CInt


newConstraintSystemFromConstraintSystem :: 
  ConstraintSystem ->
  IO ConstraintSystem
newConstraintSystemFromConstraintSystem (ConstraintSystem arg1) = do
  alloca $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "newConstraintSystemFromConstraintSystem" $ 
    ppl_new_Constraint_System_from_Constraint_System mArg0 mArg1
  res <- peek mArg0
  liftM ConstraintSystem $ newForeignPtr deleteConstraintSystem res

foreign import ccall unsafe "ppl_new_Constraint_System_from_Constraint_System"
  ppl_new_Constraint_System_from_Constraint_System :: Ptr (Ptr ConstraintSystem) -> Ptr ConstraintSystem -> IO CInt


assignConstraintSystemFromConstraintSystem :: 
  ConstraintSystem ->
  ConstraintSystem ->
  IO ()
assignConstraintSystemFromConstraintSystem (ConstraintSystem arg0) (ConstraintSystem arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "assignConstraintSystemFromConstraintSystem" $ 
    ppl_assign_Constraint_System_from_Constraint_System mArg0 mArg1

foreign import ccall unsafe "ppl_assign_Constraint_System_from_Constraint_System"
  ppl_assign_Constraint_System_from_Constraint_System :: Ptr ConstraintSystem -> Ptr ConstraintSystem -> IO CInt


foreign import ccall unsafe "&ppl_delete_Constraint_System"
  deleteConstraintSystem :: FinalizerPtr ConstraintSystem

constraintSystemSpaceDimension :: 
  ConstraintSystem ->
  IO Dimension
constraintSystemSpaceDimension (ConstraintSystem arg0) = do
  withForeignPtr arg0 $ \mArg0 -> do
  alloca $ \mArg1 -> do
  checkRetVal "constraintSystemSpaceDimension" $ 
    ppl_Constraint_System_space_dimension mArg0 mArg1
  liftM fromIntegral $ peek mArg1

foreign import ccall unsafe "ppl_Constraint_System_space_dimension"
  ppl_Constraint_System_space_dimension :: Ptr ConstraintSystem -> Ptr (#{type ppl_dimension_type}) -> IO CInt


constraintSystemEmpty :: 
  ConstraintSystem ->
  IO ()
constraintSystemEmpty (ConstraintSystem arg0) = do
  withForeignPtr arg0 $ \mArg0 -> do
  checkRetVal "constraintSystemEmpty" $ 
    ppl_Constraint_System_empty mArg0

foreign import ccall unsafe "ppl_Constraint_System_empty"
  ppl_Constraint_System_empty :: Ptr ConstraintSystem -> IO CInt


constraintSystemHasStrictInequalities :: 
  ConstraintSystem ->
  IO ()
constraintSystemHasStrictInequalities (ConstraintSystem arg0) = do
  withForeignPtr arg0 $ \mArg0 -> do
  checkRetVal "constraintSystemHasStrictInequalities" $ 
    ppl_Constraint_System_has_strict_inequalities mArg0

foreign import ccall unsafe "ppl_Constraint_System_has_strict_inequalities"
  ppl_Constraint_System_has_strict_inequalities :: Ptr ConstraintSystem -> IO CInt


constraintSystemBegin :: 
  ConstraintSystem ->
  ConstraintSystemIterator ->
  IO ()
constraintSystemBegin (ConstraintSystem arg0) (ConstraintSystemIterator arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "constraintSystemBegin" $ 
    ppl_Constraint_System_begin mArg0 mArg1

foreign import ccall unsafe "ppl_Constraint_System_begin"
  ppl_Constraint_System_begin :: Ptr ConstraintSystem -> Ptr ConstraintSystemIterator -> IO CInt


constraintSystemEnd :: 
  ConstraintSystem ->
  ConstraintSystemIterator ->
  IO ()
constraintSystemEnd (ConstraintSystem arg0) (ConstraintSystemIterator arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "constraintSystemEnd" $ 
    ppl_Constraint_System_end mArg0 mArg1

foreign import ccall unsafe "ppl_Constraint_System_end"
  ppl_Constraint_System_end :: Ptr ConstraintSystem -> Ptr ConstraintSystemIterator -> IO CInt


constraintSystemOK :: 
  ConstraintSystem ->
  IO Bool
constraintSystemOK (ConstraintSystem arg0) = do
  withForeignPtr arg0 $ \mArg0 -> do
  liftM (toBool.fromIntegral) $ 
    ppl_Constraint_System_OK mArg0

foreign import ccall unsafe "ppl_Constraint_System_OK"
  ppl_Constraint_System_OK :: Ptr ConstraintSystem -> IO CInt


constraintSystemClear :: 
  ConstraintSystem ->
  IO ()
constraintSystemClear (ConstraintSystem arg0) = do
  withForeignPtr arg0 $ \mArg0 -> do
  checkRetVal "constraintSystemClear" $ 
    ppl_Constraint_System_clear mArg0

foreign import ccall unsafe "ppl_Constraint_System_clear"
  ppl_Constraint_System_clear :: Ptr ConstraintSystem -> IO CInt


constraintSystemInsertConstraint :: 
  ConstraintSystem ->
  Constraint ->
  IO ()
constraintSystemInsertConstraint (ConstraintSystem arg0) (Constraint arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "constraintSystemInsertConstraint" $ 
    ppl_Constraint_System_insert_Constraint mArg0 mArg1

foreign import ccall unsafe "ppl_Constraint_System_insert_Constraint"
  ppl_Constraint_System_insert_Constraint :: Ptr ConstraintSystem -> Ptr Constraint -> IO CInt


newConstraintSystemIterator :: 
  IO ConstraintSystemIterator
newConstraintSystemIterator = do
  alloca $ \mArg0 -> do
  checkRetVal "newConstraintSystemIterator" $ 
    ppl_new_Constraint_System_const_iterator mArg0
  res <- peek mArg0
  liftM ConstraintSystemIterator $ newForeignPtr deleteConstraintSystemIterator res

foreign import ccall unsafe "ppl_new_Constraint_System_const_iterator"
  ppl_new_Constraint_System_const_iterator :: Ptr (Ptr ConstraintSystemIterator) -> IO CInt


newConstraintSystemIteratorFromConstraintSystemIterator :: 
  ConstraintSystemIterator ->
  IO ConstraintSystemIterator
newConstraintSystemIteratorFromConstraintSystemIterator (ConstraintSystemIterator arg1) = do
  alloca $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "newConstraintSystemIteratorFromConstraintSystemIterator" $ 
    ppl_new_Constraint_System_const_iterator_from_Constraint_System_const_iterator mArg0 mArg1
  res <- peek mArg0
  liftM ConstraintSystemIterator $ newForeignPtr deleteConstraintSystemIterator res

foreign import ccall unsafe "ppl_new_Constraint_System_const_iterator_from_Constraint_System_const_iterator"
  ppl_new_Constraint_System_const_iterator_from_Constraint_System_const_iterator :: Ptr (Ptr ConstraintSystemIterator) -> Ptr ConstraintSystemIterator -> IO CInt


assignConstraintSystemIteratorFromConstraintSystemIterator :: 
  ConstraintSystemIterator ->
  ConstraintSystemIterator ->
  IO ()
assignConstraintSystemIteratorFromConstraintSystemIterator (ConstraintSystemIterator arg0) (ConstraintSystemIterator arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "assignConstraintSystemIteratorFromConstraintSystemIterator" $ 
    ppl_assign_Constraint_System_const_iterator_from_Constraint_System_const_iterator mArg0 mArg1

foreign import ccall unsafe "ppl_assign_Constraint_System_const_iterator_from_Constraint_System_const_iterator"
  ppl_assign_Constraint_System_const_iterator_from_Constraint_System_const_iterator :: Ptr ConstraintSystemIterator -> Ptr ConstraintSystemIterator -> IO CInt


foreign import ccall unsafe "&ppl_delete_Constraint_System_const_iterator"
  deleteConstraintSystemIterator :: FinalizerPtr ConstraintSystemIterator

constraintSystemIteratorDereference :: 
  ConstraintSystemIterator ->
  IO Constraint
constraintSystemIteratorDereference (ConstraintSystemIterator arg0) = do
  withForeignPtr arg0 $ \mArg0 -> do
  alloca $ \mArg1 -> do
  checkRetVal "constraintSystemIteratorDereference" $ 
    ppl_Constraint_System_const_iterator_dereference mArg0 mArg1
  res <- peek mArg1
  alloca $ \copyPtr -> do
    ppl_new_Constraint_from_Constraint copyPtr res
    res <- peek copyPtr
    liftM Constraint $ newForeignPtr deleteConstraint res

foreign import ccall unsafe "ppl_Constraint_System_const_iterator_dereference"
  ppl_Constraint_System_const_iterator_dereference :: Ptr ConstraintSystemIterator -> Ptr (Ptr Constraint) -> IO CInt


constraintSystemIteratorIncrement :: 
  ConstraintSystemIterator ->
  IO ()
constraintSystemIteratorIncrement (ConstraintSystemIterator arg0) = do
  withForeignPtr arg0 $ \mArg0 -> do
  checkRetVal "constraintSystemIteratorIncrement" $ 
    ppl_Constraint_System_const_iterator_increment mArg0

foreign import ccall unsafe "ppl_Constraint_System_const_iterator_increment"
  ppl_Constraint_System_const_iterator_increment :: Ptr ConstraintSystemIterator -> IO CInt


constraintSystemIteratorEqualTest :: 
  ConstraintSystemIterator ->
  ConstraintSystemIterator ->
  IO Bool
constraintSystemIteratorEqualTest (ConstraintSystemIterator arg0) (ConstraintSystemIterator arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  liftM (toBool.fromIntegral) $ 
    ppl_Constraint_System_const_iterator_equal_test mArg0 mArg1

foreign import ccall unsafe "ppl_Constraint_System_const_iterator_equal_test"
  ppl_Constraint_System_const_iterator_equal_test :: Ptr ConstraintSystemIterator -> Ptr ConstraintSystemIterator -> IO CInt


newGenerator :: 
  LinearExpression ->
  GeneratorType ->
  Coefficient ->
  IO Generator
newGenerator (LinearExpression arg1) arg2 (Coefficient arg3) = do
  alloca $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  let mArg2 = (fromIntegral . fromEnum) arg2
  withForeignPtr arg3 $ \mArg3 -> do
  checkRetVal "newGenerator" $ 
    ppl_new_Generator mArg0 mArg1 mArg2 mArg3
  res <- peek mArg0
  liftM Generator $ newForeignPtr deleteGenerator res

foreign import ccall unsafe "ppl_new_Generator"
  ppl_new_Generator :: Ptr (Ptr Generator) -> Ptr LinearExpression -> #{type int} -> Ptr Coefficient -> IO CInt


newGeneratorZeroDimPoint :: 
  IO Generator
newGeneratorZeroDimPoint = do
  alloca $ \mArg0 -> do
  checkRetVal "newGeneratorZeroDimPoint" $ 
    ppl_new_Generator_zero_dim_point mArg0
  res <- peek mArg0
  liftM Generator $ newForeignPtr deleteGenerator res

foreign import ccall unsafe "ppl_new_Generator_zero_dim_point"
  ppl_new_Generator_zero_dim_point :: Ptr (Ptr Generator) -> IO CInt


newGeneratorZeroDimClosurePoint :: 
  IO Generator
newGeneratorZeroDimClosurePoint = do
  alloca $ \mArg0 -> do
  checkRetVal "newGeneratorZeroDimClosurePoint" $ 
    ppl_new_Generator_zero_dim_closure_point mArg0
  res <- peek mArg0
  liftM Generator $ newForeignPtr deleteGenerator res

foreign import ccall unsafe "ppl_new_Generator_zero_dim_closure_point"
  ppl_new_Generator_zero_dim_closure_point :: Ptr (Ptr Generator) -> IO CInt


newGeneratorFromGenerator :: 
  Generator ->
  IO Generator
newGeneratorFromGenerator (Generator arg1) = do
  alloca $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "newGeneratorFromGenerator" $ 
    ppl_new_Generator_from_Generator mArg0 mArg1
  res <- peek mArg0
  liftM Generator $ newForeignPtr deleteGenerator res

foreign import ccall unsafe "ppl_new_Generator_from_Generator"
  ppl_new_Generator_from_Generator :: Ptr (Ptr Generator) -> Ptr Generator -> IO CInt


assignGeneratorFromGenerator :: 
  Generator ->
  Generator ->
  IO ()
assignGeneratorFromGenerator (Generator arg0) (Generator arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "assignGeneratorFromGenerator" $ 
    ppl_assign_Generator_from_Generator mArg0 mArg1

foreign import ccall unsafe "ppl_assign_Generator_from_Generator"
  ppl_assign_Generator_from_Generator :: Ptr Generator -> Ptr Generator -> IO CInt


foreign import ccall unsafe "&ppl_delete_Generator"
  deleteGenerator :: FinalizerPtr Generator

generatorSpaceDimension :: 
  Generator ->
  IO Dimension
generatorSpaceDimension (Generator arg0) = do
  withForeignPtr arg0 $ \mArg0 -> do
  alloca $ \mArg1 -> do
  checkRetVal "generatorSpaceDimension" $ 
    ppl_Generator_space_dimension mArg0 mArg1
  liftM fromIntegral $ peek mArg1

foreign import ccall unsafe "ppl_Generator_space_dimension"
  ppl_Generator_space_dimension :: Ptr Generator -> Ptr (#{type ppl_dimension_type}) -> IO CInt


generatorType ::
  Generator ->
  IO GeneratorType
generatorType (Generator arg0) = do
  withForeignPtr arg0 $ \mArg0 -> do
  liftM (toEnum . fromIntegral) $ 
    ppl_Generator_type mArg0

foreign import ccall unsafe "ppl_Generator_type"
  ppl_Generator_type :: Ptr Generator -> IO CInt
generatorCoefficient :: 
  Generator ->
  Dimension ->
  Coefficient ->
  IO ()
generatorCoefficient (Generator arg0) arg1 (Coefficient arg2) = do
  withForeignPtr arg0 $ \mArg0 -> do
  let mArg1 = fromIntegral arg1
  withForeignPtr arg2 $ \mArg2 -> do
  checkRetVal "generatorCoefficient" $ 
    ppl_Generator_coefficient mArg0 mArg1 mArg2

foreign import ccall unsafe "ppl_Generator_coefficient"
  ppl_Generator_coefficient :: Ptr Generator -> #{type ppl_dimension_type} -> Ptr Coefficient -> IO CInt


generatorDivisor :: 
  Generator ->
  Coefficient ->
  IO ()
generatorDivisor (Generator arg0) (Coefficient arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "generatorDivisor" $ 
    ppl_Generator_divisor mArg0 mArg1

foreign import ccall unsafe "ppl_Generator_divisor"
  ppl_Generator_divisor :: Ptr Generator -> Ptr Coefficient -> IO CInt


generatorOK :: 
  Generator ->
  IO Bool
generatorOK (Generator arg0) = do
  withForeignPtr arg0 $ \mArg0 -> do
  liftM (toBool.fromIntegral) $ 
    ppl_Generator_OK mArg0

foreign import ccall unsafe "ppl_Generator_OK"
  ppl_Generator_OK :: Ptr Generator -> IO CInt


newGeneratorSystem :: 
  IO GeneratorSystem
newGeneratorSystem = do
  alloca $ \mArg0 -> do
  checkRetVal "newGeneratorSystem" $ 
    ppl_new_Generator_System mArg0
  res <- peek mArg0
  liftM GeneratorSystem $ newForeignPtr deleteGeneratorSystem res

foreign import ccall unsafe "ppl_new_Generator_System"
  ppl_new_Generator_System :: Ptr (Ptr GeneratorSystem) -> IO CInt


newGeneratorSystemZeroDimUniv :: 
  IO GeneratorSystem
newGeneratorSystemZeroDimUniv = do
  alloca $ \mArg0 -> do
  checkRetVal "newGeneratorSystemZeroDimUniv" $ 
    ppl_new_Generator_System_zero_dim_univ mArg0
  res <- peek mArg0
  liftM GeneratorSystem $ newForeignPtr deleteGeneratorSystem res

foreign import ccall unsafe "ppl_new_Generator_System_zero_dim_univ"
  ppl_new_Generator_System_zero_dim_univ :: Ptr (Ptr GeneratorSystem) -> IO CInt


newGeneratorSystemFromGenerator :: 
  Generator ->
  IO GeneratorSystem
newGeneratorSystemFromGenerator (Generator arg1) = do
  alloca $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "newGeneratorSystemFromGenerator" $ 
    ppl_new_Generator_System_from_Generator mArg0 mArg1
  res <- peek mArg0
  liftM GeneratorSystem $ newForeignPtr deleteGeneratorSystem res

foreign import ccall unsafe "ppl_new_Generator_System_from_Generator"
  ppl_new_Generator_System_from_Generator :: Ptr (Ptr GeneratorSystem) -> Ptr Generator -> IO CInt


newGeneratorSystemFromGeneratorSystem :: 
  GeneratorSystem ->
  IO GeneratorSystem
newGeneratorSystemFromGeneratorSystem (GeneratorSystem arg1) = do
  alloca $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "newGeneratorSystemFromGeneratorSystem" $ 
    ppl_new_Generator_System_from_Generator_System mArg0 mArg1
  res <- peek mArg0
  liftM GeneratorSystem $ newForeignPtr deleteGeneratorSystem res

foreign import ccall unsafe "ppl_new_Generator_System_from_Generator_System"
  ppl_new_Generator_System_from_Generator_System :: Ptr (Ptr GeneratorSystem) -> Ptr GeneratorSystem -> IO CInt


assignGeneratorSystemFromGeneratorSystem :: 
  GeneratorSystem ->
  GeneratorSystem ->
  IO ()
assignGeneratorSystemFromGeneratorSystem (GeneratorSystem arg0) (GeneratorSystem arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "assignGeneratorSystemFromGeneratorSystem" $ 
    ppl_assign_Generator_System_from_Generator_System mArg0 mArg1

foreign import ccall unsafe "ppl_assign_Generator_System_from_Generator_System"
  ppl_assign_Generator_System_from_Generator_System :: Ptr GeneratorSystem -> Ptr GeneratorSystem -> IO CInt


foreign import ccall unsafe "&ppl_delete_Generator_System"
  deleteGeneratorSystem :: FinalizerPtr GeneratorSystem

generatorSystemSpaceDimension :: 
  GeneratorSystem ->
  IO Dimension
generatorSystemSpaceDimension (GeneratorSystem arg0) = do
  withForeignPtr arg0 $ \mArg0 -> do
  alloca $ \mArg1 -> do
  checkRetVal "generatorSystemSpaceDimension" $ 
    ppl_Generator_System_space_dimension mArg0 mArg1
  liftM fromIntegral $ peek mArg1

foreign import ccall unsafe "ppl_Generator_System_space_dimension"
  ppl_Generator_System_space_dimension :: Ptr GeneratorSystem -> Ptr (#{type ppl_dimension_type}) -> IO CInt


generatorSystemEmpty :: 
  GeneratorSystem ->
  IO ()
generatorSystemEmpty (GeneratorSystem arg0) = do
  withForeignPtr arg0 $ \mArg0 -> do
  checkRetVal "generatorSystemEmpty" $ 
    ppl_Generator_System_empty mArg0

foreign import ccall unsafe "ppl_Generator_System_empty"
  ppl_Generator_System_empty :: Ptr GeneratorSystem -> IO CInt


generatorSystemBegin :: 
  GeneratorSystem ->
  GeneratorSystemIterator ->
  IO ()
generatorSystemBegin (GeneratorSystem arg0) (GeneratorSystemIterator arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "generatorSystemBegin" $ 
    ppl_Generator_System_begin mArg0 mArg1

foreign import ccall unsafe "ppl_Generator_System_begin"
  ppl_Generator_System_begin :: Ptr GeneratorSystem -> Ptr GeneratorSystemIterator -> IO CInt


generatorSystemEnd :: 
  GeneratorSystem ->
  GeneratorSystemIterator ->
  IO ()
generatorSystemEnd (GeneratorSystem arg0) (GeneratorSystemIterator arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "generatorSystemEnd" $ 
    ppl_Generator_System_end mArg0 mArg1

foreign import ccall unsafe "ppl_Generator_System_end"
  ppl_Generator_System_end :: Ptr GeneratorSystem -> Ptr GeneratorSystemIterator -> IO CInt


generatorSystemOK :: 
  GeneratorSystem ->
  IO Bool
generatorSystemOK (GeneratorSystem arg0) = do
  withForeignPtr arg0 $ \mArg0 -> do
  liftM (toBool.fromIntegral) $ 
    ppl_Generator_System_OK mArg0

foreign import ccall unsafe "ppl_Generator_System_OK"
  ppl_Generator_System_OK :: Ptr GeneratorSystem -> IO CInt


generatorSystemClear :: 
  GeneratorSystem ->
  IO ()
generatorSystemClear (GeneratorSystem arg0) = do
  withForeignPtr arg0 $ \mArg0 -> do
  checkRetVal "generatorSystemClear" $ 
    ppl_Generator_System_clear mArg0

foreign import ccall unsafe "ppl_Generator_System_clear"
  ppl_Generator_System_clear :: Ptr GeneratorSystem -> IO CInt


generatorSystemInsertGenerator :: 
  GeneratorSystem ->
  Generator ->
  IO ()
generatorSystemInsertGenerator (GeneratorSystem arg0) (Generator arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "generatorSystemInsertGenerator" $ 
    ppl_Generator_System_insert_Generator mArg0 mArg1

foreign import ccall unsafe "ppl_Generator_System_insert_Generator"
  ppl_Generator_System_insert_Generator :: Ptr GeneratorSystem -> Ptr Generator -> IO CInt


newGeneratorSystemIterator :: 
  IO GeneratorSystemIterator
newGeneratorSystemIterator = do
  alloca $ \mArg0 -> do
  checkRetVal "newGeneratorSystemIterator" $ 
    ppl_new_Generator_System_const_iterator mArg0
  res <- peek mArg0
  liftM GeneratorSystemIterator $ newForeignPtr deleteGeneratorSystemIterator res

foreign import ccall unsafe "ppl_new_Generator_System_const_iterator"
  ppl_new_Generator_System_const_iterator :: Ptr (Ptr GeneratorSystemIterator) -> IO CInt


newGeneratorSystemIteratorFromGeneratorSystemIterator :: 
  GeneratorSystemIterator ->
  IO GeneratorSystemIterator
newGeneratorSystemIteratorFromGeneratorSystemIterator (GeneratorSystemIterator arg1) = do
  alloca $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "newGeneratorSystemIteratorFromGeneratorSystemIterator" $ 
    ppl_new_Generator_System_const_iterator_from_Generator_System_const_iterator mArg0 mArg1
  res <- peek mArg0
  liftM GeneratorSystemIterator $ newForeignPtr deleteGeneratorSystemIterator res

foreign import ccall unsafe "ppl_new_Generator_System_const_iterator_from_Generator_System_const_iterator"
  ppl_new_Generator_System_const_iterator_from_Generator_System_const_iterator :: Ptr (Ptr GeneratorSystemIterator) -> Ptr GeneratorSystemIterator -> IO CInt


assignGeneratorSystemIteratorFromGeneratorSystemIterator :: 
  GeneratorSystemIterator ->
  GeneratorSystemIterator ->
  IO ()
assignGeneratorSystemIteratorFromGeneratorSystemIterator (GeneratorSystemIterator arg0) (GeneratorSystemIterator arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "assignGeneratorSystemIteratorFromGeneratorSystemIterator" $ 
    ppl_assign_Generator_System_const_iterator_from_Generator_System_const_iterator mArg0 mArg1

foreign import ccall unsafe "ppl_assign_Generator_System_const_iterator_from_Generator_System_const_iterator"
  ppl_assign_Generator_System_const_iterator_from_Generator_System_const_iterator :: Ptr GeneratorSystemIterator -> Ptr GeneratorSystemIterator -> IO CInt


foreign import ccall unsafe "&ppl_delete_Generator_System_const_iterator"
  deleteGeneratorSystemIterator :: FinalizerPtr GeneratorSystemIterator

generatorSystemIteratorDereference :: 
  GeneratorSystemIterator ->
  IO Generator
generatorSystemIteratorDereference (GeneratorSystemIterator arg0) = do
  withForeignPtr arg0 $ \mArg0 -> do
  alloca $ \mArg1 -> do
  checkRetVal "generatorSystemIteratorDereference" $ 
    ppl_Generator_System_const_iterator_dereference mArg0 mArg1
  res <- peek mArg1
  alloca $ \copyPtr -> do
    ppl_new_Generator_from_Generator copyPtr res
    res <- peek copyPtr
    liftM Generator $ newForeignPtr deleteGenerator res

foreign import ccall unsafe "ppl_Generator_System_const_iterator_dereference"
  ppl_Generator_System_const_iterator_dereference :: Ptr GeneratorSystemIterator -> Ptr (Ptr Generator) -> IO CInt


generatorSystemIteratorIncrement :: 
  GeneratorSystemIterator ->
  IO ()
generatorSystemIteratorIncrement (GeneratorSystemIterator arg0) = do
  withForeignPtr arg0 $ \mArg0 -> do
  checkRetVal "generatorSystemIteratorIncrement" $ 
    ppl_Generator_System_const_iterator_increment mArg0

foreign import ccall unsafe "ppl_Generator_System_const_iterator_increment"
  ppl_Generator_System_const_iterator_increment :: Ptr GeneratorSystemIterator -> IO CInt


generatorSystemIteratorEqualTest :: 
  GeneratorSystemIterator ->
  GeneratorSystemIterator ->
  IO Bool
generatorSystemIteratorEqualTest (GeneratorSystemIterator arg0) (GeneratorSystemIterator arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  liftM (toBool.fromIntegral) $ 
    ppl_Generator_System_const_iterator_equal_test mArg0 mArg1

foreign import ccall unsafe "ppl_Generator_System_const_iterator_equal_test"
  ppl_Generator_System_const_iterator_equal_test :: Ptr GeneratorSystemIterator -> Ptr GeneratorSystemIterator -> IO CInt


-- TODO: write binding for new_Congruence
-- TODO: write binding for new_Congruence_zero_dim_false
-- TODO: write binding for new_Congruence_zero_dim_integrality
-- TODO: write binding for new_Congruence_from_Congruence
-- TODO: write binding for assign_Congruence_from_Congruence
-- TODO: write binding for delete_Congruence
-- TODO: write binding for Congruence_space_dimension
-- TODO: write binding for Congruence_coefficient
-- TODO: write binding for Congruence_inhomogeneous_term
-- TODO: write binding for Congruence_modulus
-- TODO: write binding for Congruence_OK
-- TODO: write binding for new_Congruence_System
-- TODO: write binding for new_Congruence_System_zero_dim_empty
-- TODO: write binding for new_Congruence_System_from_Congruence
-- TODO: write binding for new_Congruence_System_from_Congruence_System
-- TODO: write binding for assign_Congruence_System_from_Congruence_System
-- TODO: write binding for delete_Congruence_System
-- TODO: write binding for Congruence_System_space_dimension
-- TODO: write binding for Congruence_System_empty
-- TODO: write binding for Congruence_System_begin
-- TODO: write binding for Congruence_System_end
-- TODO: write binding for Congruence_System_OK
-- TODO: write binding for Congruence_System_clear
-- TODO: write binding for Congruence_System_insert_Congruence
-- TODO: write binding for new_Congruence_System_const_iterator
-- TODO: write binding for new_Congruence_System_const_iterator_from_Congruence_System_const_iterator
-- TODO: write binding for assign_Congruence_System_const_iterator_from_Congruence_System_const_iterator
-- TODO: write binding for delete_Congruence_System_const_iterator
-- TODO: write binding for Congruence_System_const_iterator_dereference
-- TODO: write binding for Congruence_System_const_iterator_increment
-- TODO: write binding for Congruence_System_const_iterator_equal_test
-- TODO: write binding for new_Grid_Generator
-- TODO: write binding for new_Grid_Generator_zero_dim_point
-- TODO: write binding for new_Grid_Generator_from_Grid_Generator
-- TODO: write binding for assign_Grid_Generator_from_Grid_Generator
-- TODO: write binding for delete_Grid_Generator
-- TODO: write binding for Grid_Generator_space_dimension
-- TODO: write binding for Grid_Generator_type
-- TODO: write binding for Grid_Generator_coefficient
-- TODO: write binding for Grid_Generator_divisor
-- TODO: write binding for Grid_Generator_OK
-- TODO: write binding for new_Grid_Generator_System
-- TODO: write binding for new_Grid_Generator_System_zero_dim_univ
-- TODO: write binding for new_Grid_Generator_System_from_Grid_Generator
-- TODO: write binding for new_Grid_Generator_System_from_Grid_Generator_System
-- TODO: write binding for assign_Grid_Generator_System_from_Grid_Generator_System
-- TODO: write binding for delete_Grid_Generator_System
-- TODO: write binding for Grid_Generator_System_space_dimension
-- TODO: write binding for Grid_Generator_System_empty
-- TODO: write binding for Grid_Generator_System_begin
-- TODO: write binding for Grid_Generator_System_end
-- TODO: write binding for Grid_Generator_System_OK
-- TODO: write binding for Grid_Generator_System_clear
-- TODO: write binding for Grid_Generator_System_insert_Grid_Generator
-- TODO: write binding for new_Grid_Generator_System_const_iterator
-- TODO: write binding for new_Grid_Generator_System_const_iterator_from_Grid_Generator_System_const_iterator
-- TODO: write binding for assign_Grid_Generator_System_const_iterator_from_Grid_Generator_System_const_iterator
-- TODO: write binding for delete_Grid_Generator_System_const_iterator
-- TODO: write binding for Grid_Generator_System_const_iterator_dereference
-- TODO: write binding for Grid_Generator_System_const_iterator_increment
-- TODO: write binding for Grid_Generator_System_const_iterator_equal_test
-- TODO: write binding for new_MIP_Problem_from_space_dimension
-- TODO: write binding for new_MIP_Problem
-- TODO: write binding for new_MIP_Problem_from_MIP_Problem
-- TODO: write binding for assign_MIP_Problem_from_MIP_Problem
-- TODO: write binding for delete_MIP_Problem
-- TODO: write binding for MIP_Problem_space_dimension
-- TODO: write binding for MIP_Problem_number_of_integer_space_dimensions
-- TODO: write binding for MIP_Problem_integer_space_dimensions
-- TODO: write binding for MIP_Problem_number_of_constraints
-- TODO: write binding for MIP_Problem_constraint_at_index
-- TODO: write binding for MIP_Problem_objective_function
-- TODO: write binding for MIP_Problem_optimization_mode
-- TODO: write binding for MIP_Problem_OK
-- TODO: write binding for MIP_Problem_clear
-- TODO: write binding for MIP_Problem_add_space_dimensions_and_embed
-- TODO: write binding for MIP_Problem_add_to_integer_space_dimensions
-- TODO: write binding for MIP_Problem_add_constraint
-- TODO: write binding for MIP_Problem_add_constraints
-- TODO: write binding for MIP_Problem_set_objective_function
-- TODO: write binding for MIP_Problem_set_optimization_mode
-- TODO: write binding for MIP_Problem_is_satisfiable
-- TODO: write binding for MIP_Problem_solve
-- TODO: write binding for MIP_Problem_evaluate_objective_function
-- TODO: write binding for MIP_Problem_feasible_point
-- TODO: write binding for MIP_Problem_optimizing_point
-- TODO: write binding for MIP_Problem_optimal_value
-- TODO: write binding for MIP_Problem_get_control_parameter
-- TODO: write binding for MIP_Problem_set_control_parameter
-- TODO: write binding for MIP_Problem_total_memory_in_bytes
-- TODO: write binding for MIP_Problem_external_memory_in_bytes
-- TODO: write binding for new_PIP_Problem_from_space_dimension
-- TODO: write binding for new_PIP_Problem_from_PIP_Problem
-- TODO: write binding for assign_PIP_Problem_from_PIP_Problem
-- TODO: write binding for new_PIP_Problem_from_constraints
-- TODO: write binding for delete_PIP_Problem
-- TODO: write binding for PIP_Problem_space_dimension
-- TODO: write binding for PIP_Problem_number_of_parameter_space_dimensions
-- TODO: write binding for PIP_Problem_parameter_space_dimensions
-- TODO: write binding for PIP_Problem_get_big_parameter_dimension
-- TODO: write binding for PIP_Problem_number_of_constraints
-- TODO: write binding for PIP_Problem_constraint_at_index
-- TODO: write binding for PIP_Problem_total_memory_in_bytes
-- TODO: write binding for PIP_Problem_external_memory_in_bytes
-- TODO: write binding for PIP_Problem_OK
-- TODO: write binding for PIP_Problem_clear
-- TODO: write binding for PIP_Problem_add_space_dimensions_and_embed
-- TODO: write binding for PIP_Problem_add_to_parameter_space_dimensions
-- TODO: write binding for PIP_Problem_set_big_parameter_dimension
-- TODO: write binding for PIP_Problem_add_constraint
-- TODO: write binding for PIP_Problem_add_constraints
-- TODO: write binding for PIP_Problem_is_satisfiable
-- TODO: write binding for PIP_Problem_solve
-- TODO: write binding for PIP_Problem_solution
-- TODO: write binding for PIP_Problem_optimizing_solution
-- TODO: write binding for PIP_Problem_get_control_parameter
-- TODO: write binding for PIP_Problem_set_control_parameter
-- TODO: write binding for PIP_Tree_Node_as_solution
-- TODO: write binding for PIP_Tree_Node_as_decision
-- TODO: write binding for PIP_Tree_Node_get_constraints
-- TODO: write binding for PIP_Tree_Node_OK
-- TODO: write binding for PIP_Tree_Node_number_of_artificials
-- TODO: write binding for PIP_Tree_Node_begin
-- TODO: write binding for PIP_Tree_Node_end
-- TODO: write binding for PIP_Solution_Node_get_parametric_values
-- TODO: write binding for PIP_Decision_Node_get_child_node
-- TODO: write binding for Artificial_Parameter_get_Linear_Expression
-- TODO: write binding for Artificial_Parameter_coefficient
-- TODO: write binding for Artificial_Parameter_get_inhomogeneous_term
-- TODO: write binding for Artificial_Parameter_denominator
-- TODO: write binding for new_Artificial_Parameter_Sequence_const_iterator
-- TODO: write binding for new_Artificial_Parameter_Sequence_const_iterator_from_Artificial_Parameter_Sequence_const_iterator
-- TODO: write binding for assign_Artificial_Parameter_Sequence_const_iterator_from_Artificial_Parameter_Sequence_const_iterator
-- TODO: write binding for delete_Artificial_Parameter_Sequence_const_iterator
-- TODO: write binding for Artificial_Parameter_Sequence_const_iterator_dereference
-- TODO: write binding for Artificial_Parameter_Sequence_const_iterator_increment
-- TODO: write binding for Artificial_Parameter_Sequence_const_iterator_equal_test
foreign import ccall unsafe "&ppl_delete_Polyhedron"
  deletePolyhedron :: FinalizerPtr Polyhedron

-- TODO: write binding for new_C_Polyhedron_from_space_dimension
newCPolyhedronFromDimension ::
  Dimension ->
  IO Polyhedron
newCPolyhedronFromDimension arg1 = do
  alloca $ \mArg0 -> do
    let mArg1 = fromIntegral arg1  
    checkRetVal "newCPolyhedronFromDimension" $
      ppl_new_C_Polyhedron_from_dimension mArg0 mArg1
    res <- peek mArg0
    liftM Polyhedron $ newForeignPtr deletePolyhedron res
    
foreign import ccall unsafe "ppl_new_C_Polyhedron_from_space_dimension"
  ppl_new_C_Polyhedron_from_dimension :: Ptr (Ptr Polyhedron) -> Word64 -> IO CInt

-- TODO: write binding for new_NNC_Polyhedron_from_space_dimension
newNNCPolyhedronFromDimension ::
  Dimension ->
  IO Polyhedron
newNNCPolyhedronFromDimension arg1 = do
  alloca $ \mArg0 -> do
    let mArg1 = fromIntegral arg1
    checkRetVal "newNNCPolyhedronFromDimension" $
      ppl_new_NNC_Polyhedron_from_dimension mArg0 mArg1
    res <- peek mArg0
    liftM Polyhedron $ newForeignPtr deletePolyhedron res
    
foreign import ccall unsafe "ppl_new_NNC_Polyhedron_from_space_dimension"
  ppl_new_NNC_Polyhedron_from_dimension :: Ptr (Ptr Polyhedron) -> Word64 -> IO CInt

{-
newCPolyhedronEmptyFromDimension ::
  Dimension ->
  IO Polyhedron
newCPolyhedronEmptyFromDimension arg1 = do
  alloca $ \mArg0 -> do
    let mArg1 = fromIntegral arg1
    checkRetVal "newCPolyhedronEmptyFromDimension" $
      ppl_new_C_Polyhedron_empty_from_dimension mArg0 mArg1
    res <- peek mArg0
    liftM Polyhedron $ newForeignPtr deletePolyhedron res

foreign import ccall unsafe "ppl_new_C_Polyhedron_empty_from_space_dimension"
  ppl_new_C_Polyhedron_empty_from_dimension :: Ptr (Ptr Polyhedron) -> Word64 -> IO CInt
                                               
newNNCPolyhedronEmptyFromDimension ::
  Dimension ->
  IO Polyhedron
newNNCPolyhedronEmptyFromDimension arg1 = do
  alloca $ \mArg0 -> do
    let mArg1 = fromIntegral arg1
    checkRetVal "newNNCPolyhedronEmptyFromDimension" $
      ppl_new_NNC_Polyhedron_empty_from_dimension mArg0 mArg1
    res <- peek mArg0
    liftM Polyhedron $ newForeignPtr deletePolyhedron res
  
foreign import ccall unsafe "ppl_new_NNC_Polyhedron_empty_from_space_dimension"
  ppl_new_NNC_Polyhedron_empty_from_dimension :: Ptr (Ptr Polyhedron) -> Word64 -> IO CInt
-}

newCPolyhedronFromCPolyhedron :: 
  Polyhedron ->
  IO Polyhedron
newCPolyhedronFromCPolyhedron (Polyhedron arg1) = do
  alloca $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "newCPolyhedronFromCPolyhedron" $ 
    ppl_new_C_Polyhedron_from_C_Polyhedron mArg0 mArg1
  res <- peek mArg0
  liftM Polyhedron $ newForeignPtr deletePolyhedron res

foreign import ccall unsafe "ppl_new_C_Polyhedron_from_C_Polyhedron"
  ppl_new_C_Polyhedron_from_C_Polyhedron :: Ptr (Ptr Polyhedron) -> Ptr Polyhedron -> IO CInt


newNNCPolyhedronFromCPolyhedron :: 
  Polyhedron ->
  IO Polyhedron
newNNCPolyhedronFromCPolyhedron (Polyhedron arg1) = do
  alloca $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "newNNCPolyhedronFromCPolyhedron" $ 
    ppl_new_NNC_Polyhedron_from_C_Polyhedron mArg0 mArg1
  res <- peek mArg0
  liftM Polyhedron $ newForeignPtr deletePolyhedron res

foreign import ccall unsafe "ppl_new_NNC_Polyhedron_from_C_Polyhedron"
  ppl_new_NNC_Polyhedron_from_C_Polyhedron :: Ptr (Ptr Polyhedron) -> Ptr Polyhedron -> IO CInt


newCPolyhedronFromNNCPolyhedron :: 
  Polyhedron ->
  IO Polyhedron
newCPolyhedronFromNNCPolyhedron (Polyhedron arg1) = do
  alloca $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "newCPolyhedronFromNNCPolyhedron" $ 
    ppl_new_C_Polyhedron_from_NNC_Polyhedron mArg0 mArg1
  res <- peek mArg0
  liftM Polyhedron $ newForeignPtr deletePolyhedron res

foreign import ccall unsafe "ppl_new_C_Polyhedron_from_NNC_Polyhedron"
  ppl_new_C_Polyhedron_from_NNC_Polyhedron :: Ptr (Ptr Polyhedron) -> Ptr Polyhedron -> IO CInt


newNNCPolyhedronFromNNCPolyhedron :: 
  Polyhedron ->
  IO Polyhedron
newNNCPolyhedronFromNNCPolyhedron (Polyhedron arg1) = do
  alloca $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "newNNCPolyhedronFromNNCPolyhedron" $ 
    ppl_new_NNC_Polyhedron_from_NNC_Polyhedron mArg0 mArg1
  res <- peek mArg0
  liftM Polyhedron $ newForeignPtr deletePolyhedron res

foreign import ccall unsafe "ppl_new_NNC_Polyhedron_from_NNC_Polyhedron"
  ppl_new_NNC_Polyhedron_from_NNC_Polyhedron :: Ptr (Ptr Polyhedron) -> Ptr Polyhedron -> IO CInt


-- TODO: write binding for new_C_Polyhedron_from_Grid
-- TODO: write binding for new_NNC_Polyhedron_from_Grid
-- TODO: write binding for new_C_Polyhedron_from_Rational_Box
-- TODO: write binding for new_NNC_Polyhedron_from_Rational_Box
-- TODO: write binding for new_C_Polyhedron_from_BD_Shape_mpz_class
-- TODO: write binding for new_NNC_Polyhedron_from_BD_Shape_mpz_class
-- TODO: write binding for new_C_Polyhedron_from_BD_Shape_mpq_class
-- TODO: write binding for new_NNC_Polyhedron_from_BD_Shape_mpq_class
-- TODO: write binding for new_C_Polyhedron_from_Octagonal_Shape_mpz_class
-- TODO: write binding for new_NNC_Polyhedron_from_Octagonal_Shape_mpz_class
-- TODO: write binding for new_C_Polyhedron_from_Octagonal_Shape_mpq_class
-- TODO: write binding for new_NNC_Polyhedron_from_Octagonal_Shape_mpq_class
-- TODO: write binding for new_C_Polyhedron_from_Double_Box
-- TODO: write binding for new_NNC_Polyhedron_from_Double_Box
-- TODO: write binding for new_C_Polyhedron_from_BD_Shape_double
-- TODO: write binding for new_NNC_Polyhedron_from_BD_Shape_double
-- TODO: write binding for new_C_Polyhedron_from_Octagonal_Shape_double
-- TODO: write binding for new_NNC_Polyhedron_from_Octagonal_Shape_double
-- TODO: write binding for new_C_Polyhedron_from_C_Polyhedron_with_complexity
-- TODO: write binding for new_NNC_Polyhedron_from_C_Polyhedron_with_complexity
-- TODO: write binding for new_C_Polyhedron_from_NNC_Polyhedron_with_complexity
-- TODO: write binding for new_NNC_Polyhedron_from_NNC_Polyhedron_with_complexity
-- TODO: write binding for new_C_Polyhedron_from_Grid_with_complexity
-- TODO: write binding for new_NNC_Polyhedron_from_Grid_with_complexity
-- TODO: write binding for new_C_Polyhedron_from_Rational_Box_with_complexity
-- TODO: write binding for new_NNC_Polyhedron_from_Rational_Box_with_complexity
-- TODO: write binding for new_C_Polyhedron_from_BD_Shape_mpz_class_with_complexity
-- TODO: write binding for new_NNC_Polyhedron_from_BD_Shape_mpz_class_with_complexity
-- TODO: write binding for new_C_Polyhedron_from_BD_Shape_mpq_class_with_complexity
-- TODO: write binding for new_NNC_Polyhedron_from_BD_Shape_mpq_class_with_complexity
-- TODO: write binding for new_C_Polyhedron_from_Octagonal_Shape_mpz_class_with_complexity
-- TODO: write binding for new_NNC_Polyhedron_from_Octagonal_Shape_mpz_class_with_complexity
-- TODO: write binding for new_C_Polyhedron_from_Octagonal_Shape_mpq_class_with_complexity
-- TODO: write binding for new_NNC_Polyhedron_from_Octagonal_Shape_mpq_class_with_complexity
-- TODO: write binding for new_C_Polyhedron_from_Double_Box_with_complexity
-- TODO: write binding for new_NNC_Polyhedron_from_Double_Box_with_complexity
-- TODO: write binding for new_C_Polyhedron_from_BD_Shape_double_with_complexity
-- TODO: write binding for new_NNC_Polyhedron_from_BD_Shape_double_with_complexity
-- TODO: write binding for new_C_Polyhedron_from_Octagonal_Shape_double_with_complexity
-- TODO: write binding for new_NNC_Polyhedron_from_Octagonal_Shape_double_with_complexity
newCPolyhedronFromConstraintSystem :: 
  ConstraintSystem ->
  IO Polyhedron
newCPolyhedronFromConstraintSystem (ConstraintSystem arg1) = do
  alloca $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "newCPolyhedronFromConstraintSystem" $ 
    ppl_new_C_Polyhedron_from_Constraint_System mArg0 mArg1
  res <- peek mArg0
  liftM Polyhedron $ newForeignPtr deletePolyhedron res

foreign import ccall unsafe "ppl_new_C_Polyhedron_from_Constraint_System"
  ppl_new_C_Polyhedron_from_Constraint_System :: Ptr (Ptr Polyhedron) -> Ptr ConstraintSystem -> IO CInt


newNNCPolyhedronFromConstraintSystem :: 
  ConstraintSystem ->
  IO Polyhedron
newNNCPolyhedronFromConstraintSystem (ConstraintSystem arg1) = do
  alloca $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "newNNCPolyhedronFromConstraintSystem" $ 
    ppl_new_NNC_Polyhedron_from_Constraint_System mArg0 mArg1
  res <- peek mArg0
  liftM Polyhedron $ newForeignPtr deletePolyhedron res

foreign import ccall unsafe "ppl_new_NNC_Polyhedron_from_Constraint_System"
  ppl_new_NNC_Polyhedron_from_Constraint_System :: Ptr (Ptr Polyhedron) -> Ptr ConstraintSystem -> IO CInt


-- TODO: write binding for new_C_Polyhedron_from_Congruence_System
-- TODO: write binding for new_NNC_Polyhedron_from_Congruence_System
newCPolyhedronFromGeneratorSystem :: 
  GeneratorSystem ->
  IO Polyhedron
newCPolyhedronFromGeneratorSystem (GeneratorSystem arg1) = do
  alloca $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "newCPolyhedronFromGeneratorSystem" $ 
    ppl_new_C_Polyhedron_from_Generator_System mArg0 mArg1
  res <- peek mArg0
  liftM Polyhedron $ newForeignPtr deletePolyhedron res

foreign import ccall unsafe "ppl_new_C_Polyhedron_from_Generator_System"
  ppl_new_C_Polyhedron_from_Generator_System :: Ptr (Ptr Polyhedron) -> Ptr GeneratorSystem -> IO CInt


newNNCPolyhedronFromGeneratorSystem :: 
  GeneratorSystem ->
  IO Polyhedron
newNNCPolyhedronFromGeneratorSystem (GeneratorSystem arg1) = do
  alloca $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "newNNCPolyhedronFromGeneratorSystem" $ 
    ppl_new_NNC_Polyhedron_from_Generator_System mArg0 mArg1
  res <- peek mArg0
  liftM Polyhedron $ newForeignPtr deletePolyhedron res

foreign import ccall unsafe "ppl_new_NNC_Polyhedron_from_Generator_System"
  ppl_new_NNC_Polyhedron_from_Generator_System :: Ptr (Ptr Polyhedron) -> Ptr GeneratorSystem -> IO CInt


polyhedronSpaceDimension :: 
  Polyhedron ->
  IO Dimension
polyhedronSpaceDimension (Polyhedron arg0) = do
  withForeignPtr arg0 $ \mArg0 -> do
  alloca $ \mArg1 -> do
  checkRetVal "polyhedronSpaceDimension" $ 
    ppl_Polyhedron_space_dimension mArg0 mArg1
  liftM fromIntegral $ peek mArg1

foreign import ccall unsafe "ppl_Polyhedron_space_dimension"
  ppl_Polyhedron_space_dimension :: Ptr Polyhedron -> Ptr (#{type ppl_dimension_type}) -> IO CInt


-- TODO: write binding for ppl_Polyhedron_affine_dimension
polyhedronRelationWithConstraint :: 
  Polyhedron ->
  Constraint ->
  IO ()
polyhedronRelationWithConstraint (Polyhedron arg0) (Constraint arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "polyhedronRelationWithConstraint" $ 
    ppl_Polyhedron_relation_with_Constraint mArg0 mArg1

foreign import ccall unsafe "ppl_Polyhedron_relation_with_Constraint"
  ppl_Polyhedron_relation_with_Constraint :: Ptr Polyhedron -> Ptr Constraint -> IO CInt


polyhedronRelationWithGenerator :: 
  Polyhedron ->
  Generator ->
  IO ()
polyhedronRelationWithGenerator (Polyhedron arg0) (Generator arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "polyhedronRelationWithGenerator" $ 
    ppl_Polyhedron_relation_with_Generator mArg0 mArg1

foreign import ccall unsafe "ppl_Polyhedron_relation_with_Generator"
  ppl_Polyhedron_relation_with_Generator :: Ptr Polyhedron -> Ptr Generator -> IO CInt


-- TODO: write binding for Polyhedron_relation_with_Congruence
polyhedronGetConstraints :: 
  Polyhedron ->
  IO ConstraintSystem
polyhedronGetConstraints (Polyhedron arg0) = do
  withForeignPtr arg0 $ \mArg0 -> do
  alloca $ \mArg1 -> do
  checkRetVal "polyhedronGetConstraints" $ 
    ppl_Polyhedron_get_constraints mArg0 mArg1
  res <- peek mArg1
  alloca $ \copyPtr -> do
    ppl_new_Constraint_System_from_Constraint_System copyPtr res
    res <- peek copyPtr
    liftM ConstraintSystem $ newForeignPtr deleteConstraintSystem res

foreign import ccall unsafe "ppl_Polyhedron_get_constraints"
  ppl_Polyhedron_get_constraints :: Ptr Polyhedron -> Ptr (Ptr ConstraintSystem) -> IO CInt


-- TODO: write binding for Polyhedron_get_congruences
polyhedronGetGenerators :: 
  Polyhedron ->
  IO GeneratorSystem
polyhedronGetGenerators (Polyhedron arg0) = do
  withForeignPtr arg0 $ \mArg0 -> do
  alloca $ \mArg1 -> do
  checkRetVal "polyhedronGetGenerators" $ 
    ppl_Polyhedron_get_generators mArg0 mArg1
  res <- peek mArg1
  alloca $ \copyPtr -> do
    ppl_new_Generator_System_from_Generator_System copyPtr res
    res <- peek copyPtr
    liftM GeneratorSystem $ newForeignPtr deleteGeneratorSystem res

foreign import ccall unsafe "ppl_Polyhedron_get_generators"
  ppl_Polyhedron_get_generators :: Ptr Polyhedron -> Ptr (Ptr GeneratorSystem) -> IO CInt


polyhedronGetMinimizedConstraints :: 
  Polyhedron ->
  IO ConstraintSystem
polyhedronGetMinimizedConstraints (Polyhedron arg0) = do
  withForeignPtr arg0 $ \mArg0 -> do
  alloca $ \mArg1 -> do
  checkRetVal "polyhedronGetMinimizedConstraints" $ 
    ppl_Polyhedron_get_minimized_constraints mArg0 mArg1
  res <- peek mArg1
  alloca $ \copyPtr -> do
    ppl_new_Constraint_System_from_Constraint_System copyPtr res
    res <- peek copyPtr
    liftM ConstraintSystem $ newForeignPtr deleteConstraintSystem res

foreign import ccall unsafe "ppl_Polyhedron_get_minimized_constraints"
  ppl_Polyhedron_get_minimized_constraints :: Ptr Polyhedron -> Ptr (Ptr ConstraintSystem) -> IO CInt


-- TODO: write binding for Polyhedron_get_minimized_congruences
polyhedronGetMinimizedGenerators :: 
  Polyhedron ->
  IO GeneratorSystem
polyhedronGetMinimizedGenerators (Polyhedron arg0) = do
  withForeignPtr arg0 $ \mArg0 -> do
  alloca $ \mArg1 -> do
  checkRetVal "polyhedronGetMinimizedGenerators" $ 
    ppl_Polyhedron_get_minimized_generators mArg0 mArg1
  res <- peek mArg1
  alloca $ \copyPtr -> do
    ppl_new_Generator_System_from_Generator_System copyPtr res
    res <- peek copyPtr
    liftM GeneratorSystem $ newForeignPtr deleteGeneratorSystem res

foreign import ccall unsafe "ppl_Polyhedron_get_minimized_generators"
  ppl_Polyhedron_get_minimized_generators :: Ptr Polyhedron -> Ptr (Ptr GeneratorSystem) -> IO CInt


polyhedronIsEmpty :: 
  Polyhedron ->
  IO Bool
polyhedronIsEmpty (Polyhedron arg0) = do
  withForeignPtr arg0 $ \mArg0 -> do
  liftM (toBool.fromIntegral) $ 
    ppl_Polyhedron_is_empty mArg0

foreign import ccall unsafe "ppl_Polyhedron_is_empty"
  ppl_Polyhedron_is_empty :: Ptr Polyhedron -> IO CInt


polyhedronIsUniverse :: 
  Polyhedron ->
  IO Bool
polyhedronIsUniverse (Polyhedron arg0) = do
  withForeignPtr arg0 $ \mArg0 -> do
  liftM (toBool.fromIntegral) $ 
    ppl_Polyhedron_is_universe mArg0

foreign import ccall unsafe "ppl_Polyhedron_is_universe"
  ppl_Polyhedron_is_universe :: Ptr Polyhedron -> IO CInt


polyhedronIsBounded :: 
  Polyhedron ->
  IO Bool
polyhedronIsBounded (Polyhedron arg0) = do
  withForeignPtr arg0 $ \mArg0 -> do
  liftM (toBool.fromIntegral) $ 
    ppl_Polyhedron_is_bounded mArg0

foreign import ccall unsafe "ppl_Polyhedron_is_bounded"
  ppl_Polyhedron_is_bounded :: Ptr Polyhedron -> IO CInt


polyhedronContainsIntegerPoint :: 
  Polyhedron ->
  IO ()
polyhedronContainsIntegerPoint (Polyhedron arg0) = do
  withForeignPtr arg0 $ \mArg0 -> do
  checkRetVal "polyhedronContainsIntegerPoint" $ 
    ppl_Polyhedron_contains_integer_point mArg0

foreign import ccall unsafe "ppl_Polyhedron_contains_integer_point"
  ppl_Polyhedron_contains_integer_point :: Ptr Polyhedron -> IO CInt


polyhedronIsTopologicallyClosed :: 
  Polyhedron ->
  IO Bool
polyhedronIsTopologicallyClosed (Polyhedron arg0) = do
  withForeignPtr arg0 $ \mArg0 -> do
  liftM (toBool.fromIntegral) $ 
    ppl_Polyhedron_is_topologically_closed mArg0

foreign import ccall unsafe "ppl_Polyhedron_is_topologically_closed"
  ppl_Polyhedron_is_topologically_closed :: Ptr Polyhedron -> IO CInt


polyhedronIsDiscrete :: 
  Polyhedron ->
  IO ()
polyhedronIsDiscrete (Polyhedron arg0) = do
  withForeignPtr arg0 $ \mArg0 -> do
  checkRetVal "polyhedronIsDiscrete" $ 
    ppl_Polyhedron_is_discrete mArg0

foreign import ccall unsafe "ppl_Polyhedron_is_discrete"
  ppl_Polyhedron_is_discrete :: Ptr Polyhedron -> IO CInt


polyhedronTopologicalClosureAssign :: 
  Polyhedron ->
  IO ()
polyhedronTopologicalClosureAssign (Polyhedron arg0) = do
  withForeignPtr arg0 $ \mArg0 -> do
  checkRetVal "polyhedronTopologicalClosureAssign" $ 
    ppl_Polyhedron_topological_closure_assign mArg0

foreign import ccall unsafe "ppl_Polyhedron_topological_closure_assign"
  ppl_Polyhedron_topological_closure_assign :: Ptr Polyhedron -> IO CInt


polyhedronBoundsFromAbove :: 
  Polyhedron ->
  LinearExpression ->
  IO Bool
polyhedronBoundsFromAbove (Polyhedron arg0) (LinearExpression arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  liftM (toBool.fromIntegral) $ 
    ppl_Polyhedron_bounds_from_above mArg0 mArg1

foreign import ccall unsafe "ppl_Polyhedron_bounds_from_above"
  ppl_Polyhedron_bounds_from_above :: Ptr Polyhedron -> Ptr LinearExpression -> IO CInt


polyhedronBoundsFromBelow :: 
  Polyhedron ->
  LinearExpression ->
  IO Bool
polyhedronBoundsFromBelow (Polyhedron arg0) (LinearExpression arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  liftM (toBool.fromIntegral) $ 
    ppl_Polyhedron_bounds_from_below mArg0 mArg1

foreign import ccall unsafe "ppl_Polyhedron_bounds_from_below"
  ppl_Polyhedron_bounds_from_below :: Ptr Polyhedron -> Ptr LinearExpression -> IO CInt


-- TODO: write binding for Polyhedron_maximize
-- TODO: write binding for Polyhedron_minimize
-- TODO: write binding for Polyhedron_maximize_with_point
-- TODO: write binding for Polyhedron_minimize_with_point
-- TODO: write binding for Polyhedron_frequency
polyhedronContainsPolyhedron :: 
  Polyhedron ->
  Polyhedron ->
  IO Bool
polyhedronContainsPolyhedron (Polyhedron arg0) (Polyhedron arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  liftM (toBool.fromIntegral) $ 
    ppl_Polyhedron_contains_Polyhedron mArg0 mArg1

foreign import ccall unsafe "ppl_Polyhedron_contains_Polyhedron"
  ppl_Polyhedron_contains_Polyhedron :: Ptr Polyhedron -> Ptr Polyhedron -> IO CInt


polyhedronStrictlyContainsPolyhedron :: 
  Polyhedron ->
  Polyhedron ->
  IO Bool
polyhedronStrictlyContainsPolyhedron (Polyhedron arg0) (Polyhedron arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  liftM (toBool.fromIntegral) $ 
    ppl_Polyhedron_strictly_contains_Polyhedron mArg0 mArg1

foreign import ccall unsafe "ppl_Polyhedron_strictly_contains_Polyhedron"
  ppl_Polyhedron_strictly_contains_Polyhedron :: Ptr Polyhedron -> Ptr Polyhedron -> IO CInt


polyhedronIsDisjointFromPolyhedron :: 
  Polyhedron ->
  Polyhedron ->
  IO Bool
polyhedronIsDisjointFromPolyhedron (Polyhedron arg0) (Polyhedron arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  liftM (toBool.fromIntegral) $ 
    ppl_Polyhedron_is_disjoint_from_Polyhedron mArg0 mArg1

foreign import ccall unsafe "ppl_Polyhedron_is_disjoint_from_Polyhedron"
  ppl_Polyhedron_is_disjoint_from_Polyhedron :: Ptr Polyhedron -> Ptr Polyhedron -> IO CInt


polyhedronEqualsPolyhedron :: 
  Polyhedron ->
  Polyhedron ->
  IO Bool
polyhedronEqualsPolyhedron (Polyhedron arg0) (Polyhedron arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  liftM (toBool.fromIntegral) $ 
    ppl_Polyhedron_equals_Polyhedron mArg0 mArg1

foreign import ccall unsafe "ppl_Polyhedron_equals_Polyhedron"
  ppl_Polyhedron_equals_Polyhedron :: Ptr Polyhedron -> Ptr Polyhedron -> IO CInt


polyhedronOK :: 
  Polyhedron ->
  IO Bool
polyhedronOK (Polyhedron arg0) = do
  withForeignPtr arg0 $ \mArg0 -> do
  liftM (toBool.fromIntegral) $ 
    ppl_Polyhedron_OK mArg0

foreign import ccall unsafe "ppl_Polyhedron_OK"
  ppl_Polyhedron_OK :: Ptr Polyhedron -> IO CInt


polyhedronAddConstraint :: 
  Polyhedron ->
  Constraint ->
  IO ()
polyhedronAddConstraint (Polyhedron arg0) (Constraint arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "polyhedronAddConstraint" $ 
    ppl_Polyhedron_add_constraint mArg0 mArg1

foreign import ccall unsafe "ppl_Polyhedron_add_constraint"
  ppl_Polyhedron_add_constraint :: Ptr Polyhedron -> Ptr Constraint -> IO CInt


-- TODO: write binding for Polyhedron_add_congruence
polyhedronAddGenerator :: 
  Polyhedron ->
  Generator ->
  IO ()
polyhedronAddGenerator (Polyhedron arg0) (Generator arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "polyhedronAddGenerator" $ 
    ppl_Polyhedron_add_generator mArg0 mArg1

foreign import ccall unsafe "ppl_Polyhedron_add_generator"
  ppl_Polyhedron_add_generator :: Ptr Polyhedron -> Ptr Generator -> IO CInt


polyhedronAddConstraints :: 
  Polyhedron ->
  ConstraintSystem ->
  IO ()
polyhedronAddConstraints (Polyhedron arg0) (ConstraintSystem arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "polyhedronAddConstraints" $ 
    ppl_Polyhedron_add_constraints mArg0 mArg1

foreign import ccall unsafe "ppl_Polyhedron_add_constraints"
  ppl_Polyhedron_add_constraints :: Ptr Polyhedron -> Ptr ConstraintSystem -> IO CInt


-- TODO: write binding for Polyhedron_add_congruences
polyhedronAddGenerators :: 
  Polyhedron ->
  GeneratorSystem ->
  IO ()
polyhedronAddGenerators (Polyhedron arg0) (GeneratorSystem arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "polyhedronAddGenerators" $ 
    ppl_Polyhedron_add_generators mArg0 mArg1

foreign import ccall unsafe "ppl_Polyhedron_add_generators"
  ppl_Polyhedron_add_generators :: Ptr Polyhedron -> Ptr GeneratorSystem -> IO CInt


polyhedronRefineWithConstraint :: 
  Polyhedron ->
  Constraint ->
  IO ()
polyhedronRefineWithConstraint (Polyhedron arg0) (Constraint arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "polyhedronRefineWithConstraint" $ 
    ppl_Polyhedron_refine_with_constraint mArg0 mArg1

foreign import ccall unsafe "ppl_Polyhedron_refine_with_constraint"
  ppl_Polyhedron_refine_with_constraint :: Ptr Polyhedron -> Ptr Constraint -> IO CInt


-- TODO: write binding for Polyhedron_refine_with_congruence
polyhedronRefineWithConstraints :: 
  Polyhedron ->
  ConstraintSystem ->
  IO ()
polyhedronRefineWithConstraints (Polyhedron arg0) (ConstraintSystem arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "polyhedronRefineWithConstraints" $ 
    ppl_Polyhedron_refine_with_constraints mArg0 mArg1

foreign import ccall unsafe "ppl_Polyhedron_refine_with_constraints"
  ppl_Polyhedron_refine_with_constraints :: Ptr Polyhedron -> Ptr ConstraintSystem -> IO CInt


-- TODO: write binding for Polyhedron_refine_with_congruences
polyhedronIntersectionAssign :: 
  Polyhedron ->
  Polyhedron ->
  IO ()
polyhedronIntersectionAssign (Polyhedron arg0) (Polyhedron arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "polyhedronIntersectionAssign" $ 
    ppl_Polyhedron_intersection_assign mArg0 mArg1

foreign import ccall unsafe "ppl_Polyhedron_intersection_assign"
  ppl_Polyhedron_intersection_assign :: Ptr Polyhedron -> Ptr Polyhedron -> IO CInt


polyhedronUpperBoundAssign :: 
  Polyhedron ->
  Polyhedron ->
  IO ()
polyhedronUpperBoundAssign (Polyhedron arg0) (Polyhedron arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "polyhedronUpperBoundAssign" $ 
    ppl_Polyhedron_upper_bound_assign mArg0 mArg1

foreign import ccall unsafe "ppl_Polyhedron_upper_bound_assign"
  ppl_Polyhedron_upper_bound_assign :: Ptr Polyhedron -> Ptr Polyhedron -> IO CInt


polyhedronDifferenceAssign :: 
  Polyhedron ->
  Polyhedron ->
  IO ()
polyhedronDifferenceAssign (Polyhedron arg0) (Polyhedron arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "polyhedronDifferenceAssign" $ 
    ppl_Polyhedron_difference_assign mArg0 mArg1

foreign import ccall unsafe "ppl_Polyhedron_difference_assign"
  ppl_Polyhedron_difference_assign :: Ptr Polyhedron -> Ptr Polyhedron -> IO CInt


polyhedronConcatenateAssign :: 
  Polyhedron ->
  Polyhedron ->
  IO ()
polyhedronConcatenateAssign (Polyhedron arg0) (Polyhedron arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "polyhedronConcatenateAssign" $ 
    ppl_Polyhedron_concatenate_assign mArg0 mArg1

foreign import ccall unsafe "ppl_Polyhedron_concatenate_assign"
  ppl_Polyhedron_concatenate_assign :: Ptr Polyhedron -> Ptr Polyhedron -> IO CInt


polyhedronTimeElapseAssign :: 
  Polyhedron ->
  Polyhedron ->
  IO ()
polyhedronTimeElapseAssign (Polyhedron arg0) (Polyhedron arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "polyhedronTimeElapseAssign" $ 
    ppl_Polyhedron_time_elapse_assign mArg0 mArg1

foreign import ccall unsafe "ppl_Polyhedron_time_elapse_assign"
  ppl_Polyhedron_time_elapse_assign :: Ptr Polyhedron -> Ptr Polyhedron -> IO CInt


polyhedronPolyHullAssign :: 
  Polyhedron ->
  Polyhedron ->
  IO ()
polyhedronPolyHullAssign (Polyhedron arg0) (Polyhedron arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "polyhedronPolyHullAssign" $ 
    ppl_Polyhedron_poly_hull_assign mArg0 mArg1

foreign import ccall unsafe "ppl_Polyhedron_poly_hull_assign"
  ppl_Polyhedron_poly_hull_assign :: Ptr Polyhedron -> Ptr Polyhedron -> IO CInt


polyhedronPolyDifferenceAssign :: 
  Polyhedron ->
  Polyhedron ->
  IO ()
polyhedronPolyDifferenceAssign (Polyhedron arg0) (Polyhedron arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "polyhedronPolyDifferenceAssign" $ 
    ppl_Polyhedron_poly_difference_assign mArg0 mArg1

foreign import ccall unsafe "ppl_Polyhedron_poly_difference_assign"
  ppl_Polyhedron_poly_difference_assign :: Ptr Polyhedron -> Ptr Polyhedron -> IO CInt


polyhedronUpperBoundAssignIfExact :: 
  Polyhedron ->
  Polyhedron ->
  IO ()
polyhedronUpperBoundAssignIfExact (Polyhedron arg0) (Polyhedron arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "polyhedronUpperBoundAssignIfExact" $ 
    ppl_Polyhedron_upper_bound_assign_if_exact mArg0 mArg1

foreign import ccall unsafe "ppl_Polyhedron_upper_bound_assign_if_exact"
  ppl_Polyhedron_upper_bound_assign_if_exact :: Ptr Polyhedron -> Ptr Polyhedron -> IO CInt


polyhedronPolyHullAssignIfExact :: 
  Polyhedron ->
  Polyhedron ->
  IO ()
polyhedronPolyHullAssignIfExact (Polyhedron arg0) (Polyhedron arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "polyhedronPolyHullAssignIfExact" $ 
    ppl_Polyhedron_poly_hull_assign_if_exact mArg0 mArg1

foreign import ccall unsafe "ppl_Polyhedron_poly_hull_assign_if_exact"
  ppl_Polyhedron_poly_hull_assign_if_exact :: Ptr Polyhedron -> Ptr Polyhedron -> IO CInt


polyhedronSimplifyUsingContextAssign :: 
  Polyhedron ->
  Polyhedron ->
  IO ()
polyhedronSimplifyUsingContextAssign (Polyhedron arg0) (Polyhedron arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "polyhedronSimplifyUsingContextAssign" $ 
    ppl_Polyhedron_simplify_using_context_assign mArg0 mArg1

foreign import ccall unsafe "ppl_Polyhedron_simplify_using_context_assign"
  ppl_Polyhedron_simplify_using_context_assign :: Ptr Polyhedron -> Ptr Polyhedron -> IO CInt


polyhedronConstrains :: 
  Polyhedron ->
  Dimension ->
  IO ()
polyhedronConstrains (Polyhedron arg0) arg1 = do
  withForeignPtr arg0 $ \mArg0 -> do
  let mArg1 = fromIntegral arg1
  checkRetVal "polyhedronConstrains" $ 
    ppl_Polyhedron_constrains mArg0 mArg1

foreign import ccall unsafe "ppl_Polyhedron_constrains"
  ppl_Polyhedron_constrains :: Ptr Polyhedron -> #{type ppl_dimension_type} -> IO CInt


polyhedronUnconstrainSpaceDimension :: 
  Polyhedron ->
  Dimension ->
  IO Dimension
polyhedronUnconstrainSpaceDimension (Polyhedron arg0) arg1 = do
  withForeignPtr arg0 $ \mArg0 -> do
  let mArg1 = fromIntegral arg1
  liftM fromIntegral $ 
    ppl_Polyhedron_unconstrain_space_dimension mArg0 mArg1

foreign import ccall unsafe "ppl_Polyhedron_unconstrain_space_dimension"
  ppl_Polyhedron_unconstrain_space_dimension :: Ptr Polyhedron -> #{type ppl_dimension_type} -> IO CInt


-- TODO: write binding for Polyhedron_unconstrain_space_dimensions
polyhedronAffineImage :: 
  Polyhedron ->
  Dimension ->
  LinearExpression ->
  Coefficient ->
  IO ()
polyhedronAffineImage (Polyhedron arg0) arg1 (LinearExpression arg2) (Coefficient arg3) = do
  withForeignPtr arg0 $ \mArg0 -> do
  let mArg1 = fromIntegral arg1
  withForeignPtr arg2 $ \mArg2 -> do
  withForeignPtr arg3 $ \mArg3 -> do
  checkRetVal "polyhedronAffineImage" $ 
    ppl_Polyhedron_affine_image mArg0 mArg1 mArg2 mArg3

foreign import ccall unsafe "ppl_Polyhedron_affine_image"
  ppl_Polyhedron_affine_image :: Ptr Polyhedron -> #{type ppl_dimension_type} -> Ptr LinearExpression -> Ptr Coefficient -> IO CInt


polyhedronAffinePreimage :: 
  Polyhedron ->
  Dimension ->
  LinearExpression ->
  Coefficient ->
  IO ()
polyhedronAffinePreimage (Polyhedron arg0) arg1 (LinearExpression arg2) (Coefficient arg3) = do
  withForeignPtr arg0 $ \mArg0 -> do
  let mArg1 = fromIntegral arg1
  withForeignPtr arg2 $ \mArg2 -> do
  withForeignPtr arg3 $ \mArg3 -> do
  checkRetVal "polyhedronAffinePreimage" $ 
    ppl_Polyhedron_affine_preimage mArg0 mArg1 mArg2 mArg3

foreign import ccall unsafe "ppl_Polyhedron_affine_preimage"
  ppl_Polyhedron_affine_preimage :: Ptr Polyhedron -> #{type ppl_dimension_type} -> Ptr LinearExpression -> Ptr Coefficient -> IO CInt


polyhedronBoundedAffineImage :: 
  Polyhedron ->
  Dimension ->
  LinearExpression ->
  LinearExpression ->
  Coefficient ->
  IO ()
polyhedronBoundedAffineImage (Polyhedron arg0) arg1 (LinearExpression arg2) (LinearExpression arg3) (Coefficient arg4) = do
  withForeignPtr arg0 $ \mArg0 -> do
  let mArg1 = fromIntegral arg1
  withForeignPtr arg2 $ \mArg2 -> do
  withForeignPtr arg3 $ \mArg3 -> do
  withForeignPtr arg4 $ \mArg4 -> do
  checkRetVal "polyhedronBoundedAffineImage" $ 
    ppl_Polyhedron_bounded_affine_image mArg0 mArg1 mArg2 mArg3 mArg4

foreign import ccall unsafe "ppl_Polyhedron_bounded_affine_image"
  ppl_Polyhedron_bounded_affine_image :: Ptr Polyhedron -> #{type ppl_dimension_type} -> Ptr LinearExpression -> Ptr LinearExpression -> Ptr Coefficient -> IO CInt


polyhedronBoundedAffinePreimage :: 
  Polyhedron ->
  Dimension ->
  LinearExpression ->
  LinearExpression ->
  Coefficient ->
  IO ()
polyhedronBoundedAffinePreimage (Polyhedron arg0) arg1 (LinearExpression arg2) (LinearExpression arg3) (Coefficient arg4) = do
  withForeignPtr arg0 $ \mArg0 -> do
  let mArg1 = fromIntegral arg1
  withForeignPtr arg2 $ \mArg2 -> do
  withForeignPtr arg3 $ \mArg3 -> do
  withForeignPtr arg4 $ \mArg4 -> do
  checkRetVal "polyhedronBoundedAffinePreimage" $ 
    ppl_Polyhedron_bounded_affine_preimage mArg0 mArg1 mArg2 mArg3 mArg4

foreign import ccall unsafe "ppl_Polyhedron_bounded_affine_preimage"
  ppl_Polyhedron_bounded_affine_preimage :: Ptr Polyhedron -> #{type ppl_dimension_type} -> Ptr LinearExpression -> Ptr LinearExpression -> Ptr Coefficient -> IO CInt


polyhedronGeneralizedAffineImage :: 
  Polyhedron ->
  Dimension ->
  ConstraintType ->
  LinearExpression ->
  Coefficient ->
  IO ()
polyhedronGeneralizedAffineImage (Polyhedron arg0) arg1 arg2 (LinearExpression arg3) (Coefficient arg4) = do
  withForeignPtr arg0 $ \mArg0 -> do
  let mArg1 = fromIntegral arg1
  let mArg2 = (fromIntegral . fromEnum) arg2
  withForeignPtr arg3 $ \mArg3 -> do
  withForeignPtr arg4 $ \mArg4 -> do
  checkRetVal "polyhedronGeneralizedAffineImage" $ 
    ppl_Polyhedron_generalized_affine_image mArg0 mArg1 mArg2 mArg3 mArg4

foreign import ccall unsafe "ppl_Polyhedron_generalized_affine_image"
  ppl_Polyhedron_generalized_affine_image :: Ptr Polyhedron -> #{type ppl_dimension_type} -> #{type int} -> Ptr LinearExpression -> Ptr Coefficient -> IO CInt


polyhedronGeneralizedAffinePreimage :: 
  Polyhedron ->
  Dimension ->
  ConstraintType ->
  LinearExpression ->
  Coefficient ->
  IO ()
polyhedronGeneralizedAffinePreimage (Polyhedron arg0) arg1 arg2 (LinearExpression arg3) (Coefficient arg4) = do
  withForeignPtr arg0 $ \mArg0 -> do
  let mArg1 = fromIntegral arg1
  let mArg2 = (fromIntegral . fromEnum) arg2
  withForeignPtr arg3 $ \mArg3 -> do
  withForeignPtr arg4 $ \mArg4 -> do
  checkRetVal "polyhedronGeneralizedAffinePreimage" $ 
    ppl_Polyhedron_generalized_affine_preimage mArg0 mArg1 mArg2 mArg3 mArg4

foreign import ccall unsafe "ppl_Polyhedron_generalized_affine_preimage"
  ppl_Polyhedron_generalized_affine_preimage :: Ptr Polyhedron -> #{type ppl_dimension_type} -> #{type int} -> Ptr LinearExpression -> Ptr Coefficient -> IO CInt


polyhedronGeneralizedAffineImageLhsRhs :: 
  Polyhedron ->
  LinearExpression ->
  ConstraintType ->
  LinearExpression ->
  IO ()
polyhedronGeneralizedAffineImageLhsRhs (Polyhedron arg0) (LinearExpression arg1) arg2 (LinearExpression arg3) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  let mArg2 = (fromIntegral . fromEnum) arg2
  withForeignPtr arg3 $ \mArg3 -> do
  checkRetVal "polyhedronGeneralizedAffineImageLhsRhs" $ 
    ppl_Polyhedron_generalized_affine_image_lhs_rhs mArg0 mArg1 mArg2 mArg3

foreign import ccall unsafe "ppl_Polyhedron_generalized_affine_image_lhs_rhs"
  ppl_Polyhedron_generalized_affine_image_lhs_rhs :: Ptr Polyhedron -> Ptr LinearExpression -> #{type int} -> Ptr LinearExpression -> IO CInt


polyhedronGeneralizedAffinePreimageLhsRhs :: 
  Polyhedron ->
  LinearExpression ->
  ConstraintType ->
  LinearExpression ->
  IO ()
polyhedronGeneralizedAffinePreimageLhsRhs (Polyhedron arg0) (LinearExpression arg1) arg2 (LinearExpression arg3) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  let mArg2 = (fromIntegral . fromEnum) arg2
  withForeignPtr arg3 $ \mArg3 -> do
  checkRetVal "polyhedronGeneralizedAffinePreimageLhsRhs" $ 
    ppl_Polyhedron_generalized_affine_preimage_lhs_rhs mArg0 mArg1 mArg2 mArg3

foreign import ccall unsafe "ppl_Polyhedron_generalized_affine_preimage_lhs_rhs"
  ppl_Polyhedron_generalized_affine_preimage_lhs_rhs :: Ptr Polyhedron -> Ptr LinearExpression -> #{type int} -> Ptr LinearExpression -> IO CInt


polyhedronAddSpaceDimensionsAndEmbed :: 
  Polyhedron ->
  Dimension ->
  IO ()
polyhedronAddSpaceDimensionsAndEmbed (Polyhedron arg0) arg1 = do
  withForeignPtr arg0 $ \mArg0 -> do
  let mArg1 = fromIntegral arg1
  checkRetVal "polyhedronAddSpaceDimensionsAndEmbed" $ 
    ppl_Polyhedron_add_space_dimensions_and_embed mArg0 mArg1

foreign import ccall unsafe "ppl_Polyhedron_add_space_dimensions_and_embed"
  ppl_Polyhedron_add_space_dimensions_and_embed :: Ptr Polyhedron -> #{type ppl_dimension_type} -> IO CInt


polyhedronAddSpaceDimensionsAndProject :: 
  Polyhedron ->
  Dimension ->
  IO ()
polyhedronAddSpaceDimensionsAndProject (Polyhedron arg0) arg1 = do
  withForeignPtr arg0 $ \mArg0 -> do
  let mArg1 = fromIntegral arg1
  checkRetVal "polyhedronAddSpaceDimensionsAndProject" $ 
    ppl_Polyhedron_add_space_dimensions_and_project mArg0 mArg1

foreign import ccall unsafe "ppl_Polyhedron_add_space_dimensions_and_project"
  ppl_Polyhedron_add_space_dimensions_and_project :: Ptr Polyhedron -> #{type ppl_dimension_type} -> IO CInt


-- TODO: write binding for Polyhedron_remove_space_dimensions
polyhedronRemoveHigherSpaceDimensions :: 
  Polyhedron ->
  Dimension ->
  IO ()
polyhedronRemoveHigherSpaceDimensions (Polyhedron arg0) arg1 = do
  withForeignPtr arg0 $ \mArg0 -> do
  let mArg1 = fromIntegral arg1
  checkRetVal "polyhedronRemoveHigherSpaceDimensions" $ 
    ppl_Polyhedron_remove_higher_space_dimensions mArg0 mArg1

foreign import ccall unsafe "ppl_Polyhedron_remove_higher_space_dimensions"
  ppl_Polyhedron_remove_higher_space_dimensions :: Ptr Polyhedron -> #{type ppl_dimension_type} -> IO CInt


polyhedronExpandSpaceDimension :: 
  Polyhedron ->
  Dimension ->
  Dimension ->
  IO Dimension
polyhedronExpandSpaceDimension (Polyhedron arg0) arg1 arg2 = do
  withForeignPtr arg0 $ \mArg0 -> do
  let mArg1 = fromIntegral arg1
  let mArg2 = fromIntegral arg2
  liftM fromIntegral $ 
    ppl_Polyhedron_expand_space_dimension mArg0 mArg1 mArg2

foreign import ccall unsafe "ppl_Polyhedron_expand_space_dimension"
  ppl_Polyhedron_expand_space_dimension :: Ptr Polyhedron -> #{type ppl_dimension_type} -> #{type ppl_dimension_type} -> IO CInt


-- TODO: write binding for Polyhedron_fold_space_dimensions
-- TODO: write binding for Polyhedron_map_space_dimensions
-- TODO: write binding for Polyhedron_drop_some_non_integer_points
-- TODO: write binding for Polyhedron_drop_some_non_integer_points_2
-- TODO: write binding for Polyhedron_external_memory_in_bytes
-- TODO: write binding for Polyhedron_total_memory_in_bytes
polyhedronBHRZ03WideningAssignWithTokens :: 
  Polyhedron ->
  Polyhedron ->
  IO Int
polyhedronBHRZ03WideningAssignWithTokens (Polyhedron arg0) (Polyhedron arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  alloca $ \mArg2 -> do
  checkRetVal "polyhedronBHRZ03WideningAssignWithTokens" $ 
    ppl_Polyhedron_BHRZ03_widening_assign_with_tokens mArg0 mArg1 mArg2
  liftM fromIntegral $ peek mArg2

foreign import ccall unsafe "ppl_Polyhedron_BHRZ03_widening_assign_with_tokens"
  ppl_Polyhedron_BHRZ03_widening_assign_with_tokens :: Ptr Polyhedron -> Ptr Polyhedron -> Ptr (#{type unsigned}) -> IO CInt


polyhedronH79WideningAssignWithTokens :: 
  Polyhedron ->
  Polyhedron ->
  IO Int
polyhedronH79WideningAssignWithTokens (Polyhedron arg0) (Polyhedron arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  alloca $ \mArg2 -> do
  checkRetVal "polyhedronH79WideningAssignWithTokens" $ 
    ppl_Polyhedron_H79_widening_assign_with_tokens mArg0 mArg1 mArg2
  liftM fromIntegral $ peek mArg2

foreign import ccall unsafe "ppl_Polyhedron_H79_widening_assign_with_tokens"
  ppl_Polyhedron_H79_widening_assign_with_tokens :: Ptr Polyhedron -> Ptr Polyhedron -> Ptr (#{type unsigned}) -> IO CInt


polyhedronBHRZ03WideningAssign :: 
  Polyhedron ->
  Polyhedron ->
  IO ()
polyhedronBHRZ03WideningAssign (Polyhedron arg0) (Polyhedron arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "polyhedronBHRZ03WideningAssign" $ 
    ppl_Polyhedron_BHRZ03_widening_assign mArg0 mArg1

foreign import ccall unsafe "ppl_Polyhedron_BHRZ03_widening_assign"
  ppl_Polyhedron_BHRZ03_widening_assign :: Ptr Polyhedron -> Ptr Polyhedron -> IO CInt


polyhedronH79WideningAssign :: 
  Polyhedron ->
  Polyhedron ->
  IO ()
polyhedronH79WideningAssign (Polyhedron arg0) (Polyhedron arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "polyhedronH79WideningAssign" $ 
    ppl_Polyhedron_H79_widening_assign mArg0 mArg1

foreign import ccall unsafe "ppl_Polyhedron_H79_widening_assign"
  ppl_Polyhedron_H79_widening_assign :: Ptr Polyhedron -> Ptr Polyhedron -> IO CInt


polyhedronWideningAssignWithTokens :: 
  Polyhedron ->
  Polyhedron ->
  IO Int
polyhedronWideningAssignWithTokens (Polyhedron arg0) (Polyhedron arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  alloca $ \mArg2 -> do
  checkRetVal "polyhedronWideningAssignWithTokens" $ 
    ppl_Polyhedron_widening_assign_with_tokens mArg0 mArg1 mArg2
  liftM fromIntegral $ peek mArg2

foreign import ccall unsafe "ppl_Polyhedron_widening_assign_with_tokens"
  ppl_Polyhedron_widening_assign_with_tokens :: Ptr Polyhedron -> Ptr Polyhedron -> Ptr (#{type unsigned}) -> IO CInt


polyhedronWideningAssign :: 
  Polyhedron ->
  Polyhedron ->
  IO ()
polyhedronWideningAssign (Polyhedron arg0) (Polyhedron arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "polyhedronWideningAssign" $ 
    ppl_Polyhedron_widening_assign mArg0 mArg1

foreign import ccall unsafe "ppl_Polyhedron_widening_assign"
  ppl_Polyhedron_widening_assign :: Ptr Polyhedron -> Ptr Polyhedron -> IO CInt


polyhedronLimitedBHRZ03ExtrapolationAssignWithTokens :: 
  Polyhedron ->
  Polyhedron ->
  ConstraintSystem ->
  IO Int
polyhedronLimitedBHRZ03ExtrapolationAssignWithTokens (Polyhedron arg0) (Polyhedron arg1) (ConstraintSystem arg2) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  withForeignPtr arg2 $ \mArg2 -> do
  alloca $ \mArg3 -> do
  checkRetVal "polyhedronLimitedBHRZ03ExtrapolationAssignWithTokens" $ 
    ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_tokens mArg0 mArg1 mArg2 mArg3
  liftM fromIntegral $ peek mArg3

foreign import ccall unsafe "ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_tokens"
  ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_tokens :: Ptr Polyhedron -> Ptr Polyhedron -> Ptr ConstraintSystem -> Ptr (#{type unsigned}) -> IO CInt


polyhedronBoundedBHRZ03ExtrapolationAssignWithTokens :: 
  Polyhedron ->
  Polyhedron ->
  ConstraintSystem ->
  IO Int
polyhedronBoundedBHRZ03ExtrapolationAssignWithTokens (Polyhedron arg0) (Polyhedron arg1) (ConstraintSystem arg2) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  withForeignPtr arg2 $ \mArg2 -> do
  alloca $ \mArg3 -> do
  checkRetVal "polyhedronBoundedBHRZ03ExtrapolationAssignWithTokens" $ 
    ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_tokens mArg0 mArg1 mArg2 mArg3
  liftM fromIntegral $ peek mArg3

foreign import ccall unsafe "ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_tokens"
  ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_tokens :: Ptr Polyhedron -> Ptr Polyhedron -> Ptr ConstraintSystem -> Ptr (#{type unsigned}) -> IO CInt


polyhedronLimitedH79ExtrapolationAssignWithTokens :: 
  Polyhedron ->
  Polyhedron ->
  ConstraintSystem ->
  IO Int
polyhedronLimitedH79ExtrapolationAssignWithTokens (Polyhedron arg0) (Polyhedron arg1) (ConstraintSystem arg2) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  withForeignPtr arg2 $ \mArg2 -> do
  alloca $ \mArg3 -> do
  checkRetVal "polyhedronLimitedH79ExtrapolationAssignWithTokens" $ 
    ppl_Polyhedron_limited_H79_extrapolation_assign_with_tokens mArg0 mArg1 mArg2 mArg3
  liftM fromIntegral $ peek mArg3

foreign import ccall unsafe "ppl_Polyhedron_limited_H79_extrapolation_assign_with_tokens"
  ppl_Polyhedron_limited_H79_extrapolation_assign_with_tokens :: Ptr Polyhedron -> Ptr Polyhedron -> Ptr ConstraintSystem -> Ptr (#{type unsigned}) -> IO CInt


polyhedronBoundedH79ExtrapolationAssignWithTokens :: 
  Polyhedron ->
  Polyhedron ->
  ConstraintSystem ->
  IO Int
polyhedronBoundedH79ExtrapolationAssignWithTokens (Polyhedron arg0) (Polyhedron arg1) (ConstraintSystem arg2) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  withForeignPtr arg2 $ \mArg2 -> do
  alloca $ \mArg3 -> do
  checkRetVal "polyhedronBoundedH79ExtrapolationAssignWithTokens" $ 
    ppl_Polyhedron_bounded_H79_extrapolation_assign_with_tokens mArg0 mArg1 mArg2 mArg3
  liftM fromIntegral $ peek mArg3

foreign import ccall unsafe "ppl_Polyhedron_bounded_H79_extrapolation_assign_with_tokens"
  ppl_Polyhedron_bounded_H79_extrapolation_assign_with_tokens :: Ptr Polyhedron -> Ptr Polyhedron -> Ptr ConstraintSystem -> Ptr (#{type unsigned}) -> IO CInt


polyhedronLimitedBHRZ03ExtrapolationAssign :: 
  Polyhedron ->
  Polyhedron ->
  ConstraintSystem ->
  IO ()
polyhedronLimitedBHRZ03ExtrapolationAssign (Polyhedron arg0) (Polyhedron arg1) (ConstraintSystem arg2) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  withForeignPtr arg2 $ \mArg2 -> do
  checkRetVal "polyhedronLimitedBHRZ03ExtrapolationAssign" $ 
    ppl_Polyhedron_limited_BHRZ03_extrapolation_assign mArg0 mArg1 mArg2

foreign import ccall unsafe "ppl_Polyhedron_limited_BHRZ03_extrapolation_assign"
  ppl_Polyhedron_limited_BHRZ03_extrapolation_assign :: Ptr Polyhedron -> Ptr Polyhedron -> Ptr ConstraintSystem -> IO CInt


polyhedronBoundedBHRZ03ExtrapolationAssign :: 
  Polyhedron ->
  Polyhedron ->
  ConstraintSystem ->
  IO ()
polyhedronBoundedBHRZ03ExtrapolationAssign (Polyhedron arg0) (Polyhedron arg1) (ConstraintSystem arg2) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  withForeignPtr arg2 $ \mArg2 -> do
  checkRetVal "polyhedronBoundedBHRZ03ExtrapolationAssign" $ 
    ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign mArg0 mArg1 mArg2

foreign import ccall unsafe "ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign"
  ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign :: Ptr Polyhedron -> Ptr Polyhedron -> Ptr ConstraintSystem -> IO CInt


polyhedronLimitedH79ExtrapolationAssign :: 
  Polyhedron ->
  Polyhedron ->
  ConstraintSystem ->
  IO ()
polyhedronLimitedH79ExtrapolationAssign (Polyhedron arg0) (Polyhedron arg1) (ConstraintSystem arg2) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  withForeignPtr arg2 $ \mArg2 -> do
  checkRetVal "polyhedronLimitedH79ExtrapolationAssign" $ 
    ppl_Polyhedron_limited_H79_extrapolation_assign mArg0 mArg1 mArg2

foreign import ccall unsafe "ppl_Polyhedron_limited_H79_extrapolation_assign"
  ppl_Polyhedron_limited_H79_extrapolation_assign :: Ptr Polyhedron -> Ptr Polyhedron -> Ptr ConstraintSystem -> IO CInt


polyhedronBoundedH79ExtrapolationAssign :: 
  Polyhedron ->
  Polyhedron ->
  ConstraintSystem ->
  IO ()
polyhedronBoundedH79ExtrapolationAssign (Polyhedron arg0) (Polyhedron arg1) (ConstraintSystem arg2) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  withForeignPtr arg2 $ \mArg2 -> do
  checkRetVal "polyhedronBoundedH79ExtrapolationAssign" $ 
    ppl_Polyhedron_bounded_H79_extrapolation_assign mArg0 mArg1 mArg2

foreign import ccall unsafe "ppl_Polyhedron_bounded_H79_extrapolation_assign"
  ppl_Polyhedron_bounded_H79_extrapolation_assign :: Ptr Polyhedron -> Ptr Polyhedron -> Ptr ConstraintSystem -> IO CInt


-- TODO: write binding for Polyhedron_linear_partition
-- TODO: write binding for Polyhedron_wrap_assign
newCPolyhedronRecycleConstraintSystem :: 
  ConstraintSystem ->
  IO Polyhedron
newCPolyhedronRecycleConstraintSystem (ConstraintSystem arg1) = do
  alloca $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "newCPolyhedronRecycleConstraintSystem" $ 
    ppl_new_C_Polyhedron_recycle_Constraint_System mArg0 mArg1
  res <- peek mArg0
  liftM Polyhedron $ newForeignPtr deletePolyhedron res

foreign import ccall unsafe "ppl_new_C_Polyhedron_recycle_Constraint_System"
  ppl_new_C_Polyhedron_recycle_Constraint_System :: Ptr (Ptr Polyhedron) -> Ptr ConstraintSystem -> IO CInt


newNNCPolyhedronRecycleConstraintSystem :: 
  ConstraintSystem ->
  IO Polyhedron
newNNCPolyhedronRecycleConstraintSystem (ConstraintSystem arg1) = do
  alloca $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "newNNCPolyhedronRecycleConstraintSystem" $ 
    ppl_new_NNC_Polyhedron_recycle_Constraint_System mArg0 mArg1
  res <- peek mArg0
  liftM Polyhedron $ newForeignPtr deletePolyhedron res

foreign import ccall unsafe "ppl_new_NNC_Polyhedron_recycle_Constraint_System"
  ppl_new_NNC_Polyhedron_recycle_Constraint_System :: Ptr (Ptr Polyhedron) -> Ptr ConstraintSystem -> IO CInt


-- TODO: write binding for new_C_Polyhedron_recycle_Congruence_System
-- TODO: write binding for new_NNC_Polyhedron_recycle_Congruence_System
newCPolyhedronRecycleGeneratorSystem :: 
  GeneratorSystem ->
  IO Polyhedron
newCPolyhedronRecycleGeneratorSystem (GeneratorSystem arg1) = do
  alloca $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "newCPolyhedronRecycleGeneratorSystem" $ 
    ppl_new_C_Polyhedron_recycle_Generator_System mArg0 mArg1
  res <- peek mArg0
  liftM Polyhedron $ newForeignPtr deletePolyhedron res

foreign import ccall unsafe "ppl_new_C_Polyhedron_recycle_Generator_System"
  ppl_new_C_Polyhedron_recycle_Generator_System :: Ptr (Ptr Polyhedron) -> Ptr GeneratorSystem -> IO CInt


newNNCPolyhedronRecycleGeneratorSystem :: 
  GeneratorSystem ->
  IO Polyhedron
newNNCPolyhedronRecycleGeneratorSystem (GeneratorSystem arg1) = do
  alloca $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "newNNCPolyhedronRecycleGeneratorSystem" $ 
    ppl_new_NNC_Polyhedron_recycle_Generator_System mArg0 mArg1
  res <- peek mArg0
  liftM Polyhedron $ newForeignPtr deletePolyhedron res

foreign import ccall unsafe "ppl_new_NNC_Polyhedron_recycle_Generator_System"
  ppl_new_NNC_Polyhedron_recycle_Generator_System :: Ptr (Ptr Polyhedron) -> Ptr GeneratorSystem -> IO CInt


assignCPolyhedronFromCPolyhedron :: 
  Polyhedron ->
  Polyhedron ->
  IO ()
assignCPolyhedronFromCPolyhedron (Polyhedron arg0) (Polyhedron arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "assignCPolyhedronFromCPolyhedron" $ 
    ppl_assign_C_Polyhedron_from_C_Polyhedron mArg0 mArg1

foreign import ccall unsafe "ppl_assign_C_Polyhedron_from_C_Polyhedron"
  ppl_assign_C_Polyhedron_from_C_Polyhedron :: Ptr Polyhedron -> Ptr Polyhedron -> IO CInt


assignNNCPolyhedronFromNNCPolyhedron :: 
  Polyhedron ->
  Polyhedron ->
  IO ()
assignNNCPolyhedronFromNNCPolyhedron (Polyhedron arg0) (Polyhedron arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "assignNNCPolyhedronFromNNCPolyhedron" $ 
    ppl_assign_NNC_Polyhedron_from_NNC_Polyhedron mArg0 mArg1

foreign import ccall unsafe "ppl_assign_NNC_Polyhedron_from_NNC_Polyhedron"
  ppl_assign_NNC_Polyhedron_from_NNC_Polyhedron :: Ptr Polyhedron -> Ptr Polyhedron -> IO CInt


polyhedronAddRecycledConstraints :: 
  Polyhedron ->
  ConstraintSystem ->
  IO ()
polyhedronAddRecycledConstraints (Polyhedron arg0) (ConstraintSystem arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "polyhedronAddRecycledConstraints" $ 
    ppl_Polyhedron_add_recycled_constraints mArg0 mArg1

foreign import ccall unsafe "ppl_Polyhedron_add_recycled_constraints"
  ppl_Polyhedron_add_recycled_constraints :: Ptr Polyhedron -> Ptr ConstraintSystem -> IO CInt


-- TODO: write binding for Polyhedron_add_recycled_congruences
polyhedronAddRecycledGenerators :: 
  Polyhedron ->
  GeneratorSystem ->
  IO ()
polyhedronAddRecycledGenerators (Polyhedron arg0) (GeneratorSystem arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "polyhedronAddRecycledGenerators" $ 
    ppl_Polyhedron_add_recycled_generators mArg0 mArg1

foreign import ccall unsafe "ppl_Polyhedron_add_recycled_generators"
  ppl_Polyhedron_add_recycled_generators :: Ptr Polyhedron -> Ptr GeneratorSystem -> IO CInt


terminationTestMSCPolyhedron :: 
  Polyhedron ->
  IO ()
terminationTestMSCPolyhedron (Polyhedron arg0) = do
  withForeignPtr arg0 $ \mArg0 -> do
  checkRetVal "terminationTestMSCPolyhedron" $ 
    ppl_termination_test_MS_C_Polyhedron mArg0

foreign import ccall unsafe "ppl_termination_test_MS_C_Polyhedron"
  ppl_termination_test_MS_C_Polyhedron :: Ptr Polyhedron -> IO CInt


terminationTestPRCPolyhedron :: 
  Polyhedron ->
  IO ()
terminationTestPRCPolyhedron (Polyhedron arg0) = do
  withForeignPtr arg0 $ \mArg0 -> do
  checkRetVal "terminationTestPRCPolyhedron" $ 
    ppl_termination_test_PR_C_Polyhedron mArg0

foreign import ccall unsafe "ppl_termination_test_PR_C_Polyhedron"
  ppl_termination_test_PR_C_Polyhedron :: Ptr Polyhedron -> IO CInt


terminationTestMSNNCPolyhedron :: 
  Polyhedron ->
  IO ()
terminationTestMSNNCPolyhedron (Polyhedron arg0) = do
  withForeignPtr arg0 $ \mArg0 -> do
  checkRetVal "terminationTestMSNNCPolyhedron" $ 
    ppl_termination_test_MS_NNC_Polyhedron mArg0

foreign import ccall unsafe "ppl_termination_test_MS_NNC_Polyhedron"
  ppl_termination_test_MS_NNC_Polyhedron :: Ptr Polyhedron -> IO CInt


terminationTestPRNNCPolyhedron :: 
  Polyhedron ->
  IO ()
terminationTestPRNNCPolyhedron (Polyhedron arg0) = do
  withForeignPtr arg0 $ \mArg0 -> do
  checkRetVal "terminationTestPRNNCPolyhedron" $ 
    ppl_termination_test_PR_NNC_Polyhedron mArg0

foreign import ccall unsafe "ppl_termination_test_PR_NNC_Polyhedron"
  ppl_termination_test_PR_NNC_Polyhedron :: Ptr Polyhedron -> IO CInt


oneAffineRankingFunctionMSCPolyhedron :: 
  Polyhedron ->
  Generator ->
  IO ()
oneAffineRankingFunctionMSCPolyhedron (Polyhedron arg0) (Generator arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "oneAffineRankingFunctionMSCPolyhedron" $ 
    ppl_one_affine_ranking_function_MS_C_Polyhedron mArg0 mArg1

foreign import ccall unsafe "ppl_one_affine_ranking_function_MS_C_Polyhedron"
  ppl_one_affine_ranking_function_MS_C_Polyhedron :: Ptr Polyhedron -> Ptr Generator -> IO CInt


oneAffineRankingFunctionPRCPolyhedron :: 
  Polyhedron ->
  Generator ->
  IO ()
oneAffineRankingFunctionPRCPolyhedron (Polyhedron arg0) (Generator arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "oneAffineRankingFunctionPRCPolyhedron" $ 
    ppl_one_affine_ranking_function_PR_C_Polyhedron mArg0 mArg1

foreign import ccall unsafe "ppl_one_affine_ranking_function_PR_C_Polyhedron"
  ppl_one_affine_ranking_function_PR_C_Polyhedron :: Ptr Polyhedron -> Ptr Generator -> IO CInt


oneAffineRankingFunctionMSNNCPolyhedron :: 
  Polyhedron ->
  Generator ->
  IO ()
oneAffineRankingFunctionMSNNCPolyhedron (Polyhedron arg0) (Generator arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "oneAffineRankingFunctionMSNNCPolyhedron" $ 
    ppl_one_affine_ranking_function_MS_NNC_Polyhedron mArg0 mArg1

foreign import ccall unsafe "ppl_one_affine_ranking_function_MS_NNC_Polyhedron"
  ppl_one_affine_ranking_function_MS_NNC_Polyhedron :: Ptr Polyhedron -> Ptr Generator -> IO CInt


oneAffineRankingFunctionPRNNCPolyhedron :: 
  Polyhedron ->
  Generator ->
  IO ()
oneAffineRankingFunctionPRNNCPolyhedron (Polyhedron arg0) (Generator arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "oneAffineRankingFunctionPRNNCPolyhedron" $ 
    ppl_one_affine_ranking_function_PR_NNC_Polyhedron mArg0 mArg1

foreign import ccall unsafe "ppl_one_affine_ranking_function_PR_NNC_Polyhedron"
  ppl_one_affine_ranking_function_PR_NNC_Polyhedron :: Ptr Polyhedron -> Ptr Generator -> IO CInt


allAffineRankingFunctionsMSCPolyhedron :: 
  Polyhedron ->
  Polyhedron ->
  IO ()
allAffineRankingFunctionsMSCPolyhedron (Polyhedron arg0) (Polyhedron arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "allAffineRankingFunctionsMSCPolyhedron" $ 
    ppl_all_affine_ranking_functions_MS_C_Polyhedron mArg0 mArg1

foreign import ccall unsafe "ppl_all_affine_ranking_functions_MS_C_Polyhedron"
  ppl_all_affine_ranking_functions_MS_C_Polyhedron :: Ptr Polyhedron -> Ptr Polyhedron -> IO CInt


allAffineRankingFunctionsPRCPolyhedron :: 
  Polyhedron ->
  Polyhedron ->
  IO ()
allAffineRankingFunctionsPRCPolyhedron (Polyhedron arg0) (Polyhedron arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "allAffineRankingFunctionsPRCPolyhedron" $ 
    ppl_all_affine_ranking_functions_PR_C_Polyhedron mArg0 mArg1

foreign import ccall unsafe "ppl_all_affine_ranking_functions_PR_C_Polyhedron"
  ppl_all_affine_ranking_functions_PR_C_Polyhedron :: Ptr Polyhedron -> Ptr Polyhedron -> IO CInt


allAffineRankingFunctionsMSNNCPolyhedron :: 
  Polyhedron ->
  Polyhedron ->
  IO ()
allAffineRankingFunctionsMSNNCPolyhedron (Polyhedron arg0) (Polyhedron arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "allAffineRankingFunctionsMSNNCPolyhedron" $ 
    ppl_all_affine_ranking_functions_MS_NNC_Polyhedron mArg0 mArg1

foreign import ccall unsafe "ppl_all_affine_ranking_functions_MS_NNC_Polyhedron"
  ppl_all_affine_ranking_functions_MS_NNC_Polyhedron :: Ptr Polyhedron -> Ptr Polyhedron -> IO CInt


allAffineRankingFunctionsPRNNCPolyhedron :: 
  Polyhedron ->
  Polyhedron ->
  IO ()
allAffineRankingFunctionsPRNNCPolyhedron (Polyhedron arg0) (Polyhedron arg1) = do
  withForeignPtr arg0 $ \mArg0 -> do
  withForeignPtr arg1 $ \mArg1 -> do
  checkRetVal "allAffineRankingFunctionsPRNNCPolyhedron" $ 
    ppl_all_affine_ranking_functions_PR_NNC_Polyhedron mArg0 mArg1

foreign import ccall unsafe "ppl_all_affine_ranking_functions_PR_NNC_Polyhedron"
  ppl_all_affine_ranking_functions_PR_NNC_Polyhedron :: Ptr Polyhedron -> Ptr Polyhedron -> IO CInt


-- TODO: write binding for termination_test_MS_C_Polyhedron_2
-- TODO: write binding for termination_test_PR_C_Polyhedron_2
-- TODO: write binding for termination_test_MS_NNC_Polyhedron_2
-- TODO: write binding for termination_test_PR_NNC_Polyhedron_2
-- TODO: write binding for one_affine_ranking_function_MS_C_Polyhedron_2
-- TODO: write binding for one_affine_ranking_function_PR_C_Polyhedron_2
-- TODO: write binding for one_affine_ranking_function_MS_NNC_Polyhedron_2
-- TODO: write binding for one_affine_ranking_function_PR_NNC_Polyhedron_2
-- TODO: write binding for all_affine_ranking_functions_MS_C_Polyhedron_2
-- TODO: write binding for all_affine_ranking_functions_PR_C_Polyhedron_2
-- TODO: write binding for all_affine_ranking_functions_MS_NNC_Polyhedron_2
-- TODO: write binding for all_affine_ranking_functions_PR_NNC_Polyhedron_2
-- TODO: write binding for delete_Grid
-- TODO: write binding for new_Grid_from_space_dimension
-- TODO: write binding for new_Grid_from_C_Polyhedron
-- TODO: write binding for new_Grid_from_NNC_Polyhedron
-- TODO: write binding for new_Grid_from_Grid
-- TODO: write binding for new_Grid_from_Rational_Box
-- TODO: write binding for new_Grid_from_BD_Shape_mpz_class
-- TODO: write binding for new_Grid_from_BD_Shape_mpq_class
-- TODO: write binding for new_Grid_from_Octagonal_Shape_mpz_class
-- TODO: write binding for new_Grid_from_Octagonal_Shape_mpq_class
-- TODO: write binding for new_Grid_from_Double_Box
-- TODO: write binding for new_Grid_from_BD_Shape_double
-- TODO: write binding for new_Grid_from_Octagonal_Shape_double
-- TODO: write binding for new_Grid_from_C_Polyhedron_with_complexity
-- TODO: write binding for new_Grid_from_NNC_Polyhedron_with_complexity
-- TODO: write binding for new_Grid_from_Grid_with_complexity
-- TODO: write binding for new_Grid_from_Rational_Box_with_complexity
-- TODO: write binding for new_Grid_from_BD_Shape_mpz_class_with_complexity
-- TODO: write binding for new_Grid_from_BD_Shape_mpq_class_with_complexity
-- TODO: write binding for new_Grid_from_Octagonal_Shape_mpz_class_with_complexity
-- TODO: write binding for new_Grid_from_Octagonal_Shape_mpq_class_with_complexity
-- TODO: write binding for new_Grid_from_Double_Box_with_complexity
-- TODO: write binding for new_Grid_from_BD_Shape_double_with_complexity
-- TODO: write binding for new_Grid_from_Octagonal_Shape_double_with_complexity
-- TODO: write binding for new_Grid_from_Constraint_System
-- TODO: write binding for new_Grid_from_Congruence_System
-- TODO: write binding for new_Grid_from_Grid_Generator_System
-- TODO: write binding for Grid_space_dimension
-- TODO: write binding for Grid_affine_dimension
-- TODO: write binding for Grid_relation_with_Constraint
-- TODO: write binding for Grid_relation_with_Generator
-- TODO: write binding for Grid_relation_with_Congruence
-- TODO: write binding for Grid_relation_with_Grid_Generator
-- TODO: write binding for Grid_get_constraints
-- TODO: write binding for Grid_get_congruences
-- TODO: write binding for Grid_get_grid_generators
-- TODO: write binding for Grid_get_minimized_constraints
-- TODO: write binding for Grid_get_minimized_congruences
-- TODO: write binding for Grid_get_minimized_grid_generators
-- TODO: write binding for Grid_is_empty
-- TODO: write binding for Grid_is_universe
-- TODO: write binding for Grid_is_bounded
-- TODO: write binding for Grid_contains_integer_point
-- TODO: write binding for Grid_is_topologically_closed
-- TODO: write binding for Grid_is_discrete
-- TODO: write binding for Grid_topological_closure_assign
-- TODO: write binding for Grid_bounds_from_above
-- TODO: write binding for Grid_bounds_from_below
-- TODO: write binding for Grid_maximize
-- TODO: write binding for Grid_minimize
-- TODO: write binding for Grid_maximize_with_point
-- TODO: write binding for Grid_minimize_with_point
-- TODO: write binding for Grid_frequency
-- TODO: write binding for Grid_contains_Grid
-- TODO: write binding for Grid_strictly_contains_Grid
-- TODO: write binding for Grid_is_disjoint_from_Grid
-- TODO: write binding for Grid_equals_Grid
-- TODO: write binding for Grid_OK
-- TODO: write binding for Grid_add_constraint
-- TODO: write binding for Grid_add_congruence
-- TODO: write binding for Grid_add_grid_generator
-- TODO: write binding for Grid_add_constraints
-- TODO: write binding for Grid_add_congruences
-- TODO: write binding for Grid_add_grid_generators
-- TODO: write binding for Grid_refine_with_constraint
-- TODO: write binding for Grid_refine_with_congruence
-- TODO: write binding for Grid_refine_with_constraints
-- TODO: write binding for Grid_refine_with_congruences
-- TODO: write binding for Grid_intersection_assign
-- TODO: write binding for Grid_upper_bound_assign
-- TODO: write binding for Grid_difference_assign
-- TODO: write binding for Grid_concatenate_assign
-- TODO: write binding for Grid_time_elapse_assign
-- TODO: write binding for Grid_upper_bound_assign_if_exact
-- TODO: write binding for Grid_simplify_using_context_assign
-- TODO: write binding for Grid_constrains
-- TODO: write binding for Grid_unconstrain_space_dimension
-- TODO: write binding for Grid_unconstrain_space_dimensions
-- TODO: write binding for Grid_affine_image
-- TODO: write binding for Grid_affine_preimage
-- TODO: write binding for Grid_bounded_affine_image
-- TODO: write binding for Grid_bounded_affine_preimage
-- TODO: write binding for Grid_generalized_affine_image
-- TODO: write binding for Grid_generalized_affine_preimage
-- TODO: write binding for Grid_generalized_affine_image_lhs_rhs
-- TODO: write binding for Grid_generalized_affine_preimage_lhs_rhs
-- TODO: write binding for Grid_generalized_affine_image_with_congruence
-- TODO: write binding for Grid_generalized_affine_preimage_with_congruence
-- TODO: write binding for Grid_generalized_affine_image_lhs_rhs_with_congruence
-- TODO: write binding for Grid_generalized_affine_preimage_lhs_rhs_with_congruence
-- TODO: write binding for Grid_add_space_dimensions_and_embed
-- TODO: write binding for Grid_add_space_dimensions_and_project
-- TODO: write binding for Grid_remove_space_dimensions
-- TODO: write binding for Grid_remove_higher_space_dimensions
-- TODO: write binding for Grid_expand_space_dimension
-- TODO: write binding for Grid_fold_space_dimensions
-- TODO: write binding for Grid_map_space_dimensions
-- TODO: write binding for Grid_drop_some_non_integer_points
-- TODO: write binding for Grid_drop_some_non_integer_points_2
-- TODO: write binding for Grid_external_memory_in_bytes
-- TODO: write binding for Grid_total_memory_in_bytes
-- TODO: write binding for Grid_congruence_widening_assign_with_tokens
-- TODO: write binding for Grid_generator_widening_assign_with_tokens
-- TODO: write binding for Grid_congruence_widening_assign
-- TODO: write binding for Grid_generator_widening_assign
-- TODO: write binding for Grid_widening_assign_with_tokens
-- TODO: write binding for Grid_widening_assign
-- TODO: write binding for Grid_limited_congruence_extrapolation_assign_with_tokens
-- TODO: write binding for Grid_limited_generator_extrapolation_assign_with_tokens
-- TODO: write binding for Grid_limited_congruence_extrapolation_assign
-- TODO: write binding for Grid_limited_generator_extrapolation_assign
-- TODO: write binding for Grid_wrap_assign
-- TODO: write binding for new_Grid_recycle_Constraint_System
-- TODO: write binding for new_Grid_recycle_Congruence_System
-- TODO: write binding for new_Grid_recycle_Grid_Generator_System
-- TODO: write binding for assign_Grid_from_Grid
-- TODO: write binding for Grid_add_recycled_constraints
-- TODO: write binding for Grid_add_recycled_congruences
-- TODO: write binding for Grid_add_recycled_grid_generators
-- TODO: write binding for termination_test_MS_Grid
-- TODO: write binding for termination_test_PR_Grid
-- TODO: write binding for one_affine_ranking_function_MS_Grid
-- TODO: write binding for one_affine_ranking_function_PR_Grid
-- TODO: write binding for all_affine_ranking_functions_MS_Grid
-- TODO: write binding for all_affine_ranking_functions_PR_Grid
-- TODO: write binding for termination_test_MS_Grid_2
-- TODO: write binding for termination_test_PR_Grid_2
-- TODO: write binding for one_affine_ranking_function_MS_Grid_2
-- TODO: write binding for one_affine_ranking_function_PR_Grid_2
-- TODO: write binding for all_affine_ranking_functions_MS_Grid_2
-- TODO: write binding for all_affine_ranking_functions_PR_Grid_2
-- TODO: write binding for delete_Rational_Box
-- TODO: write binding for new_Rational_Box_from_space_dimension
-- TODO: write binding for new_Rational_Box_from_C_Polyhedron
-- TODO: write binding for new_Rational_Box_from_NNC_Polyhedron
-- TODO: write binding for new_Rational_Box_from_Grid
-- TODO: write binding for new_Rational_Box_from_Rational_Box
-- TODO: write binding for new_Rational_Box_from_BD_Shape_mpz_class
-- TODO: write binding for new_Rational_Box_from_BD_Shape_mpq_class
-- TODO: write binding for new_Rational_Box_from_Octagonal_Shape_mpz_class
-- TODO: write binding for new_Rational_Box_from_Octagonal_Shape_mpq_class
-- TODO: write binding for new_Rational_Box_from_Double_Box
-- TODO: write binding for new_Rational_Box_from_BD_Shape_double
-- TODO: write binding for new_Rational_Box_from_Octagonal_Shape_double
-- TODO: write binding for new_Rational_Box_from_C_Polyhedron_with_complexity
-- TODO: write binding for new_Rational_Box_from_NNC_Polyhedron_with_complexity
-- TODO: write binding for new_Rational_Box_from_Grid_with_complexity
-- TODO: write binding for new_Rational_Box_from_Rational_Box_with_complexity
-- TODO: write binding for new_Rational_Box_from_BD_Shape_mpz_class_with_complexity
-- TODO: write binding for new_Rational_Box_from_BD_Shape_mpq_class_with_complexity
-- TODO: write binding for new_Rational_Box_from_Octagonal_Shape_mpz_class_with_complexity
-- TODO: write binding for new_Rational_Box_from_Octagonal_Shape_mpq_class_with_complexity
-- TODO: write binding for new_Rational_Box_from_Double_Box_with_complexity
-- TODO: write binding for new_Rational_Box_from_BD_Shape_double_with_complexity
-- TODO: write binding for new_Rational_Box_from_Octagonal_Shape_double_with_complexity
-- TODO: write binding for new_Rational_Box_from_Constraint_System
-- TODO: write binding for new_Rational_Box_from_Congruence_System
-- TODO: write binding for new_Rational_Box_from_Generator_System
-- TODO: write binding for Rational_Box_space_dimension
-- TODO: write binding for Rational_Box_affine_dimension
-- TODO: write binding for Rational_Box_relation_with_Constraint
-- TODO: write binding for Rational_Box_relation_with_Generator
-- TODO: write binding for Rational_Box_relation_with_Congruence
-- TODO: write binding for Rational_Box_get_constraints
-- TODO: write binding for Rational_Box_get_congruences
-- TODO: write binding for Rational_Box_get_minimized_constraints
-- TODO: write binding for Rational_Box_get_minimized_congruences
-- TODO: write binding for Rational_Box_is_empty
-- TODO: write binding for Rational_Box_is_universe
-- TODO: write binding for Rational_Box_is_bounded
-- TODO: write binding for Rational_Box_contains_integer_point
-- TODO: write binding for Rational_Box_is_topologically_closed
-- TODO: write binding for Rational_Box_is_discrete
-- TODO: write binding for Rational_Box_topological_closure_assign
-- TODO: write binding for Rational_Box_bounds_from_above
-- TODO: write binding for Rational_Box_bounds_from_below
-- TODO: write binding for Rational_Box_maximize
-- TODO: write binding for Rational_Box_minimize
-- TODO: write binding for Rational_Box_maximize_with_point
-- TODO: write binding for Rational_Box_minimize_with_point
-- TODO: write binding for Rational_Box_frequency
-- TODO: write binding for Rational_Box_contains_Rational_Box
-- TODO: write binding for Rational_Box_strictly_contains_Rational_Box
-- TODO: write binding for Rational_Box_is_disjoint_from_Rational_Box
-- TODO: write binding for Rational_Box_equals_Rational_Box
-- TODO: write binding for Rational_Box_OK
-- TODO: write binding for Rational_Box_add_constraint
-- TODO: write binding for Rational_Box_add_congruence
-- TODO: write binding for Rational_Box_add_constraints
-- TODO: write binding for Rational_Box_add_congruences
-- TODO: write binding for Rational_Box_refine_with_constraint
-- TODO: write binding for Rational_Box_refine_with_congruence
-- TODO: write binding for Rational_Box_refine_with_constraints
-- TODO: write binding for Rational_Box_refine_with_congruences
-- TODO: write binding for Rational_Box_intersection_assign
-- TODO: write binding for Rational_Box_upper_bound_assign
-- TODO: write binding for Rational_Box_difference_assign
-- TODO: write binding for Rational_Box_concatenate_assign
-- TODO: write binding for Rational_Box_time_elapse_assign
-- TODO: write binding for Rational_Box_upper_bound_assign_if_exact
-- TODO: write binding for Rational_Box_simplify_using_context_assign
-- TODO: write binding for Rational_Box_constrains
-- TODO: write binding for Rational_Box_unconstrain_space_dimension
-- TODO: write binding for Rational_Box_unconstrain_space_dimensions
-- TODO: write binding for Rational_Box_affine_image
-- TODO: write binding for Rational_Box_affine_preimage
-- TODO: write binding for Rational_Box_bounded_affine_image
-- TODO: write binding for Rational_Box_bounded_affine_preimage
-- TODO: write binding for Rational_Box_generalized_affine_image
-- TODO: write binding for Rational_Box_generalized_affine_preimage
-- TODO: write binding for Rational_Box_generalized_affine_image_lhs_rhs
-- TODO: write binding for Rational_Box_generalized_affine_preimage_lhs_rhs
-- TODO: write binding for Rational_Box_add_space_dimensions_and_embed
-- TODO: write binding for Rational_Box_add_space_dimensions_and_project
-- TODO: write binding for Rational_Box_remove_space_dimensions
-- TODO: write binding for Rational_Box_remove_higher_space_dimensions
-- TODO: write binding for Rational_Box_expand_space_dimension
-- TODO: write binding for Rational_Box_fold_space_dimensions
-- TODO: write binding for Rational_Box_map_space_dimensions
-- TODO: write binding for Rational_Box_drop_some_non_integer_points
-- TODO: write binding for Rational_Box_drop_some_non_integer_points_2
-- TODO: write binding for Rational_Box_external_memory_in_bytes
-- TODO: write binding for Rational_Box_total_memory_in_bytes
-- TODO: write binding for Rational_Box_CC76_widening_assign_with_tokens
-- TODO: write binding for Rational_Box_CC76_widening_assign
-- TODO: write binding for Rational_Box_widening_assign_with_tokens
-- TODO: write binding for Rational_Box_widening_assign
-- TODO: write binding for Rational_Box_limited_CC76_extrapolation_assign_with_tokens
-- TODO: write binding for Rational_Box_limited_CC76_extrapolation_assign
-- TODO: write binding for Rational_Box_linear_partition
-- TODO: write binding for Rational_Box_wrap_assign
-- TODO: write binding for new_Rational_Box_recycle_Constraint_System
-- TODO: write binding for new_Rational_Box_recycle_Congruence_System
-- TODO: write binding for new_Rational_Box_recycle_Generator_System
-- TODO: write binding for assign_Rational_Box_from_Rational_Box
-- TODO: write binding for Rational_Box_add_recycled_constraints
-- TODO: write binding for Rational_Box_add_recycled_congruences
-- TODO: write binding for termination_test_MS_Rational_Box
-- TODO: write binding for termination_test_PR_Rational_Box
-- TODO: write binding for one_affine_ranking_function_MS_Rational_Box
-- TODO: write binding for one_affine_ranking_function_PR_Rational_Box
-- TODO: write binding for all_affine_ranking_functions_MS_Rational_Box
-- TODO: write binding for all_affine_ranking_functions_PR_Rational_Box
-- TODO: write binding for termination_test_MS_Rational_Box_2
-- TODO: write binding for termination_test_PR_Rational_Box_2
-- TODO: write binding for one_affine_ranking_function_MS_Rational_Box_2
-- TODO: write binding for one_affine_ranking_function_PR_Rational_Box_2
-- TODO: write binding for all_affine_ranking_functions_MS_Rational_Box_2
-- TODO: write binding for all_affine_ranking_functions_PR_Rational_Box_2
-- TODO: write binding for delete_BD_Shape_mpz_class
-- TODO: write binding for new_BD_Shape_mpz_class_from_space_dimension
-- TODO: write binding for new_BD_Shape_mpz_class_from_C_Polyhedron
-- TODO: write binding for new_BD_Shape_mpz_class_from_NNC_Polyhedron
-- TODO: write binding for new_BD_Shape_mpz_class_from_Grid
-- TODO: write binding for new_BD_Shape_mpz_class_from_Rational_Box
-- TODO: write binding for new_BD_Shape_mpz_class_from_BD_Shape_mpz_class
-- TODO: write binding for new_BD_Shape_mpz_class_from_BD_Shape_mpq_class
-- TODO: write binding for new_BD_Shape_mpz_class_from_Octagonal_Shape_mpz_class
-- TODO: write binding for new_BD_Shape_mpz_class_from_Octagonal_Shape_mpq_class
-- TODO: write binding for new_BD_Shape_mpz_class_from_Double_Box
-- TODO: write binding for new_BD_Shape_mpz_class_from_BD_Shape_double
-- TODO: write binding for new_BD_Shape_mpz_class_from_Octagonal_Shape_double
-- TODO: write binding for new_BD_Shape_mpz_class_from_C_Polyhedron_with_complexity
-- TODO: write binding for new_BD_Shape_mpz_class_from_NNC_Polyhedron_with_complexity
-- TODO: write binding for new_BD_Shape_mpz_class_from_Grid_with_complexity
-- TODO: write binding for new_BD_Shape_mpz_class_from_Rational_Box_with_complexity
-- TODO: write binding for new_BD_Shape_mpz_class_from_BD_Shape_mpz_class_with_complexity
-- TODO: write binding for new_BD_Shape_mpz_class_from_BD_Shape_mpq_class_with_complexity
-- TODO: write binding for new_BD_Shape_mpz_class_from_Octagonal_Shape_mpz_class_with_complexity
-- TODO: write binding for new_BD_Shape_mpz_class_from_Octagonal_Shape_mpq_class_with_complexity
-- TODO: write binding for new_BD_Shape_mpz_class_from_Double_Box_with_complexity
-- TODO: write binding for new_BD_Shape_mpz_class_from_BD_Shape_double_with_complexity
-- TODO: write binding for new_BD_Shape_mpz_class_from_Octagonal_Shape_double_with_complexity
-- TODO: write binding for new_BD_Shape_mpz_class_from_Constraint_System
-- TODO: write binding for new_BD_Shape_mpz_class_from_Congruence_System
-- TODO: write binding for new_BD_Shape_mpz_class_from_Generator_System
-- TODO: write binding for BD_Shape_mpz_class_space_dimension
-- TODO: write binding for BD_Shape_mpz_class_affine_dimension
-- TODO: write binding for BD_Shape_mpz_class_relation_with_Constraint
-- TODO: write binding for BD_Shape_mpz_class_relation_with_Generator
-- TODO: write binding for BD_Shape_mpz_class_relation_with_Congruence
-- TODO: write binding for BD_Shape_mpz_class_get_constraints
-- TODO: write binding for BD_Shape_mpz_class_get_congruences
-- TODO: write binding for BD_Shape_mpz_class_get_minimized_constraints
-- TODO: write binding for BD_Shape_mpz_class_get_minimized_congruences
-- TODO: write binding for BD_Shape_mpz_class_is_empty
-- TODO: write binding for BD_Shape_mpz_class_is_universe
-- TODO: write binding for BD_Shape_mpz_class_is_bounded
-- TODO: write binding for BD_Shape_mpz_class_contains_integer_point
-- TODO: write binding for BD_Shape_mpz_class_is_topologically_closed
-- TODO: write binding for BD_Shape_mpz_class_is_discrete
-- TODO: write binding for BD_Shape_mpz_class_topological_closure_assign
-- TODO: write binding for BD_Shape_mpz_class_bounds_from_above
-- TODO: write binding for BD_Shape_mpz_class_bounds_from_below
-- TODO: write binding for BD_Shape_mpz_class_maximize
-- TODO: write binding for BD_Shape_mpz_class_minimize
-- TODO: write binding for BD_Shape_mpz_class_maximize_with_point
-- TODO: write binding for BD_Shape_mpz_class_minimize_with_point
-- TODO: write binding for BD_Shape_mpz_class_frequency
-- TODO: write binding for BD_Shape_mpz_class_contains_BD_Shape_mpz_class
-- TODO: write binding for BD_Shape_mpz_class_strictly_contains_BD_Shape_mpz_class
-- TODO: write binding for BD_Shape_mpz_class_is_disjoint_from_BD_Shape_mpz_class
-- TODO: write binding for BD_Shape_mpz_class_equals_BD_Shape_mpz_class
-- TODO: write binding for BD_Shape_mpz_class_OK
-- TODO: write binding for BD_Shape_mpz_class_add_constraint
-- TODO: write binding for BD_Shape_mpz_class_add_congruence
-- TODO: write binding for BD_Shape_mpz_class_add_constraints
-- TODO: write binding for BD_Shape_mpz_class_add_congruences
-- TODO: write binding for BD_Shape_mpz_class_refine_with_constraint
-- TODO: write binding for BD_Shape_mpz_class_refine_with_congruence
-- TODO: write binding for BD_Shape_mpz_class_refine_with_constraints
-- TODO: write binding for BD_Shape_mpz_class_refine_with_congruences
-- TODO: write binding for BD_Shape_mpz_class_intersection_assign
-- TODO: write binding for BD_Shape_mpz_class_upper_bound_assign
-- TODO: write binding for BD_Shape_mpz_class_difference_assign
-- TODO: write binding for BD_Shape_mpz_class_concatenate_assign
-- TODO: write binding for BD_Shape_mpz_class_time_elapse_assign
-- TODO: write binding for BD_Shape_mpz_class_upper_bound_assign_if_exact
-- TODO: write binding for BD_Shape_mpz_class_simplify_using_context_assign
-- TODO: write binding for BD_Shape_mpz_class_constrains
-- TODO: write binding for BD_Shape_mpz_class_unconstrain_space_dimension
-- TODO: write binding for BD_Shape_mpz_class_unconstrain_space_dimensions
-- TODO: write binding for BD_Shape_mpz_class_affine_image
-- TODO: write binding for BD_Shape_mpz_class_affine_preimage
-- TODO: write binding for BD_Shape_mpz_class_bounded_affine_image
-- TODO: write binding for BD_Shape_mpz_class_bounded_affine_preimage
-- TODO: write binding for BD_Shape_mpz_class_generalized_affine_image
-- TODO: write binding for BD_Shape_mpz_class_generalized_affine_preimage
-- TODO: write binding for BD_Shape_mpz_class_generalized_affine_image_lhs_rhs
-- TODO: write binding for BD_Shape_mpz_class_generalized_affine_preimage_lhs_rhs
-- TODO: write binding for BD_Shape_mpz_class_add_space_dimensions_and_embed
-- TODO: write binding for BD_Shape_mpz_class_add_space_dimensions_and_project
-- TODO: write binding for BD_Shape_mpz_class_remove_space_dimensions
-- TODO: write binding for BD_Shape_mpz_class_remove_higher_space_dimensions
-- TODO: write binding for BD_Shape_mpz_class_expand_space_dimension
-- TODO: write binding for BD_Shape_mpz_class_fold_space_dimensions
-- TODO: write binding for BD_Shape_mpz_class_map_space_dimensions
-- TODO: write binding for BD_Shape_mpz_class_drop_some_non_integer_points
-- TODO: write binding for BD_Shape_mpz_class_drop_some_non_integer_points_2
-- TODO: write binding for BD_Shape_mpz_class_external_memory_in_bytes
-- TODO: write binding for BD_Shape_mpz_class_total_memory_in_bytes
-- TODO: write binding for BD_Shape_mpz_class_BHMZ05_widening_assign_with_tokens
-- TODO: write binding for BD_Shape_mpz_class_H79_widening_assign_with_tokens
-- TODO: write binding for BD_Shape_mpz_class_BHMZ05_widening_assign
-- TODO: write binding for BD_Shape_mpz_class_H79_widening_assign
-- TODO: write binding for BD_Shape_mpz_class_widening_assign_with_tokens
-- TODO: write binding for BD_Shape_mpz_class_widening_assign
-- TODO: write binding for BD_Shape_mpz_class_limited_BHMZ05_extrapolation_assign_with_tokens
-- TODO: write binding for BD_Shape_mpz_class_limited_H79_extrapolation_assign_with_tokens
-- TODO: write binding for BD_Shape_mpz_class_limited_CC76_extrapolation_assign_with_tokens
-- TODO: write binding for BD_Shape_mpz_class_limited_BHMZ05_extrapolation_assign
-- TODO: write binding for BD_Shape_mpz_class_limited_H79_extrapolation_assign
-- TODO: write binding for BD_Shape_mpz_class_limited_CC76_extrapolation_assign
-- TODO: write binding for BD_Shape_mpz_class_CC76_extrapolation_assign_with_tokens
-- TODO: write binding for BD_Shape_mpz_class_CC76_extrapolation_assign
-- TODO: write binding for BD_Shape_mpz_class_CC76_narrowing_assign
-- TODO: write binding for BD_Shape_mpz_class_linear_partition
-- TODO: write binding for BD_Shape_mpz_class_wrap_assign
-- TODO: write binding for new_BD_Shape_mpz_class_recycle_Constraint_System
-- TODO: write binding for new_BD_Shape_mpz_class_recycle_Congruence_System
-- TODO: write binding for new_BD_Shape_mpz_class_recycle_Generator_System
-- TODO: write binding for assign_BD_Shape_mpz_class_from_BD_Shape_mpz_class
-- TODO: write binding for BD_Shape_mpz_class_add_recycled_constraints
-- TODO: write binding for BD_Shape_mpz_class_add_recycled_congruences
-- TODO: write binding for termination_test_MS_BD_Shape_mpz_class
-- TODO: write binding for termination_test_PR_BD_Shape_mpz_class
-- TODO: write binding for one_affine_ranking_function_MS_BD_Shape_mpz_class
-- TODO: write binding for one_affine_ranking_function_PR_BD_Shape_mpz_class
-- TODO: write binding for all_affine_ranking_functions_MS_BD_Shape_mpz_class
-- TODO: write binding for all_affine_ranking_functions_PR_BD_Shape_mpz_class
-- TODO: write binding for termination_test_MS_BD_Shape_mpz_class_2
-- TODO: write binding for termination_test_PR_BD_Shape_mpz_class_2
-- TODO: write binding for one_affine_ranking_function_MS_BD_Shape_mpz_class_2
-- TODO: write binding for one_affine_ranking_function_PR_BD_Shape_mpz_class_2
-- TODO: write binding for all_affine_ranking_functions_MS_BD_Shape_mpz_class_2
-- TODO: write binding for all_affine_ranking_functions_PR_BD_Shape_mpz_class_2
-- TODO: write binding for delete_BD_Shape_mpq_class
-- TODO: write binding for new_BD_Shape_mpq_class_from_space_dimension
-- TODO: write binding for new_BD_Shape_mpq_class_from_C_Polyhedron
-- TODO: write binding for new_BD_Shape_mpq_class_from_NNC_Polyhedron
-- TODO: write binding for new_BD_Shape_mpq_class_from_Grid
-- TODO: write binding for new_BD_Shape_mpq_class_from_Rational_Box
-- TODO: write binding for new_BD_Shape_mpq_class_from_BD_Shape_mpz_class
-- TODO: write binding for new_BD_Shape_mpq_class_from_BD_Shape_mpq_class
-- TODO: write binding for new_BD_Shape_mpq_class_from_Octagonal_Shape_mpz_class
-- TODO: write binding for new_BD_Shape_mpq_class_from_Octagonal_Shape_mpq_class
-- TODO: write binding for new_BD_Shape_mpq_class_from_Double_Box
-- TODO: write binding for new_BD_Shape_mpq_class_from_BD_Shape_double
-- TODO: write binding for new_BD_Shape_mpq_class_from_Octagonal_Shape_double
-- TODO: write binding for new_BD_Shape_mpq_class_from_C_Polyhedron_with_complexity
-- TODO: write binding for new_BD_Shape_mpq_class_from_NNC_Polyhedron_with_complexity
-- TODO: write binding for new_BD_Shape_mpq_class_from_Grid_with_complexity
-- TODO: write binding for new_BD_Shape_mpq_class_from_Rational_Box_with_complexity
-- TODO: write binding for new_BD_Shape_mpq_class_from_BD_Shape_mpz_class_with_complexity
-- TODO: write binding for new_BD_Shape_mpq_class_from_BD_Shape_mpq_class_with_complexity
-- TODO: write binding for new_BD_Shape_mpq_class_from_Octagonal_Shape_mpz_class_with_complexity
-- TODO: write binding for new_BD_Shape_mpq_class_from_Octagonal_Shape_mpq_class_with_complexity
-- TODO: write binding for new_BD_Shape_mpq_class_from_Double_Box_with_complexity
-- TODO: write binding for new_BD_Shape_mpq_class_from_BD_Shape_double_with_complexity
-- TODO: write binding for new_BD_Shape_mpq_class_from_Octagonal_Shape_double_with_complexity
-- TODO: write binding for new_BD_Shape_mpq_class_from_Constraint_System
-- TODO: write binding for new_BD_Shape_mpq_class_from_Congruence_System
-- TODO: write binding for new_BD_Shape_mpq_class_from_Generator_System
-- TODO: write binding for BD_Shape_mpq_class_space_dimension
-- TODO: write binding for BD_Shape_mpq_class_affine_dimension
-- TODO: write binding for BD_Shape_mpq_class_relation_with_Constraint
-- TODO: write binding for BD_Shape_mpq_class_relation_with_Generator
-- TODO: write binding for BD_Shape_mpq_class_relation_with_Congruence
-- TODO: write binding for BD_Shape_mpq_class_get_constraints
-- TODO: write binding for BD_Shape_mpq_class_get_congruences
-- TODO: write binding for BD_Shape_mpq_class_get_minimized_constraints
-- TODO: write binding for BD_Shape_mpq_class_get_minimized_congruences
-- TODO: write binding for BD_Shape_mpq_class_is_empty
-- TODO: write binding for BD_Shape_mpq_class_is_universe
-- TODO: write binding for BD_Shape_mpq_class_is_bounded
-- TODO: write binding for BD_Shape_mpq_class_contains_integer_point
-- TODO: write binding for BD_Shape_mpq_class_is_topologically_closed
-- TODO: write binding for BD_Shape_mpq_class_is_discrete
-- TODO: write binding for BD_Shape_mpq_class_topological_closure_assign
-- TODO: write binding for BD_Shape_mpq_class_bounds_from_above
-- TODO: write binding for BD_Shape_mpq_class_bounds_from_below
-- TODO: write binding for BD_Shape_mpq_class_maximize
-- TODO: write binding for BD_Shape_mpq_class_minimize
-- TODO: write binding for BD_Shape_mpq_class_maximize_with_point
-- TODO: write binding for BD_Shape_mpq_class_minimize_with_point
-- TODO: write binding for BD_Shape_mpq_class_frequency
-- TODO: write binding for BD_Shape_mpq_class_contains_BD_Shape_mpq_class
-- TODO: write binding for BD_Shape_mpq_class_strictly_contains_BD_Shape_mpq_class
-- TODO: write binding for BD_Shape_mpq_class_is_disjoint_from_BD_Shape_mpq_class
-- TODO: write binding for BD_Shape_mpq_class_equals_BD_Shape_mpq_class
-- TODO: write binding for BD_Shape_mpq_class_OK
-- TODO: write binding for BD_Shape_mpq_class_add_constraint
-- TODO: write binding for BD_Shape_mpq_class_add_congruence
-- TODO: write binding for BD_Shape_mpq_class_add_constraints
-- TODO: write binding for BD_Shape_mpq_class_add_congruences
-- TODO: write binding for BD_Shape_mpq_class_refine_with_constraint
-- TODO: write binding for BD_Shape_mpq_class_refine_with_congruence
-- TODO: write binding for BD_Shape_mpq_class_refine_with_constraints
-- TODO: write binding for BD_Shape_mpq_class_refine_with_congruences
-- TODO: write binding for BD_Shape_mpq_class_intersection_assign
-- TODO: write binding for BD_Shape_mpq_class_upper_bound_assign
-- TODO: write binding for BD_Shape_mpq_class_difference_assign
-- TODO: write binding for BD_Shape_mpq_class_concatenate_assign
-- TODO: write binding for BD_Shape_mpq_class_time_elapse_assign
-- TODO: write binding for BD_Shape_mpq_class_upper_bound_assign_if_exact
-- TODO: write binding for BD_Shape_mpq_class_simplify_using_context_assign
-- TODO: write binding for BD_Shape_mpq_class_constrains
-- TODO: write binding for BD_Shape_mpq_class_unconstrain_space_dimension
-- TODO: write binding for BD_Shape_mpq_class_unconstrain_space_dimensions
-- TODO: write binding for BD_Shape_mpq_class_affine_image
-- TODO: write binding for BD_Shape_mpq_class_affine_preimage
-- TODO: write binding for BD_Shape_mpq_class_bounded_affine_image
-- TODO: write binding for BD_Shape_mpq_class_bounded_affine_preimage
-- TODO: write binding for BD_Shape_mpq_class_generalized_affine_image
-- TODO: write binding for BD_Shape_mpq_class_generalized_affine_preimage
-- TODO: write binding for BD_Shape_mpq_class_generalized_affine_image_lhs_rhs
-- TODO: write binding for BD_Shape_mpq_class_generalized_affine_preimage_lhs_rhs
-- TODO: write binding for BD_Shape_mpq_class_add_space_dimensions_and_embed
-- TODO: write binding for BD_Shape_mpq_class_add_space_dimensions_and_project
-- TODO: write binding for BD_Shape_mpq_class_remove_space_dimensions
-- TODO: write binding for BD_Shape_mpq_class_remove_higher_space_dimensions
-- TODO: write binding for BD_Shape_mpq_class_expand_space_dimension
-- TODO: write binding for BD_Shape_mpq_class_fold_space_dimensions
-- TODO: write binding for BD_Shape_mpq_class_map_space_dimensions
-- TODO: write binding for BD_Shape_mpq_class_drop_some_non_integer_points
-- TODO: write binding for BD_Shape_mpq_class_drop_some_non_integer_points_2
-- TODO: write binding for BD_Shape_mpq_class_external_memory_in_bytes
-- TODO: write binding for BD_Shape_mpq_class_total_memory_in_bytes
-- TODO: write binding for BD_Shape_mpq_class_BHMZ05_widening_assign_with_tokens
-- TODO: write binding for BD_Shape_mpq_class_H79_widening_assign_with_tokens
-- TODO: write binding for BD_Shape_mpq_class_BHMZ05_widening_assign
-- TODO: write binding for BD_Shape_mpq_class_H79_widening_assign
-- TODO: write binding for BD_Shape_mpq_class_widening_assign_with_tokens
-- TODO: write binding for BD_Shape_mpq_class_widening_assign
-- TODO: write binding for BD_Shape_mpq_class_limited_BHMZ05_extrapolation_assign_with_tokens
-- TODO: write binding for BD_Shape_mpq_class_limited_H79_extrapolation_assign_with_tokens
-- TODO: write binding for BD_Shape_mpq_class_limited_CC76_extrapolation_assign_with_tokens
-- TODO: write binding for BD_Shape_mpq_class_limited_BHMZ05_extrapolation_assign
-- TODO: write binding for BD_Shape_mpq_class_limited_H79_extrapolation_assign
-- TODO: write binding for BD_Shape_mpq_class_limited_CC76_extrapolation_assign
-- TODO: write binding for BD_Shape_mpq_class_CC76_extrapolation_assign_with_tokens
-- TODO: write binding for BD_Shape_mpq_class_CC76_extrapolation_assign
-- TODO: write binding for BD_Shape_mpq_class_CC76_narrowing_assign
-- TODO: write binding for BD_Shape_mpq_class_linear_partition
-- TODO: write binding for BD_Shape_mpq_class_wrap_assign
-- TODO: write binding for new_BD_Shape_mpq_class_recycle_Constraint_System
-- TODO: write binding for new_BD_Shape_mpq_class_recycle_Congruence_System
-- TODO: write binding for new_BD_Shape_mpq_class_recycle_Generator_System
-- TODO: write binding for assign_BD_Shape_mpq_class_from_BD_Shape_mpq_class
-- TODO: write binding for BD_Shape_mpq_class_add_recycled_constraints
-- TODO: write binding for BD_Shape_mpq_class_add_recycled_congruences
-- TODO: write binding for termination_test_MS_BD_Shape_mpq_class
-- TODO: write binding for termination_test_PR_BD_Shape_mpq_class
-- TODO: write binding for one_affine_ranking_function_MS_BD_Shape_mpq_class
-- TODO: write binding for one_affine_ranking_function_PR_BD_Shape_mpq_class
-- TODO: write binding for all_affine_ranking_functions_MS_BD_Shape_mpq_class
-- TODO: write binding for all_affine_ranking_functions_PR_BD_Shape_mpq_class
-- TODO: write binding for termination_test_MS_BD_Shape_mpq_class_2
-- TODO: write binding for termination_test_PR_BD_Shape_mpq_class_2
-- TODO: write binding for one_affine_ranking_function_MS_BD_Shape_mpq_class_2
-- TODO: write binding for one_affine_ranking_function_PR_BD_Shape_mpq_class_2
-- TODO: write binding for all_affine_ranking_functions_MS_BD_Shape_mpq_class_2
-- TODO: write binding for all_affine_ranking_functions_PR_BD_Shape_mpq_class_2
-- TODO: write binding for delete_Octagonal_Shape_mpz_class
-- TODO: write binding for new_Octagonal_Shape_mpz_class_from_space_dimension
-- TODO: write binding for new_Octagonal_Shape_mpz_class_from_C_Polyhedron
-- TODO: write binding for new_Octagonal_Shape_mpz_class_from_NNC_Polyhedron
-- TODO: write binding for new_Octagonal_Shape_mpz_class_from_Grid
-- TODO: write binding for new_Octagonal_Shape_mpz_class_from_Rational_Box
-- TODO: write binding for new_Octagonal_Shape_mpz_class_from_BD_Shape_mpz_class
-- TODO: write binding for new_Octagonal_Shape_mpz_class_from_BD_Shape_mpq_class
-- TODO: write binding for new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_mpz_class
-- TODO: write binding for new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_mpq_class
-- TODO: write binding for new_Octagonal_Shape_mpz_class_from_Double_Box
-- TODO: write binding for new_Octagonal_Shape_mpz_class_from_BD_Shape_double
-- TODO: write binding for new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_double
-- TODO: write binding for new_Octagonal_Shape_mpz_class_from_C_Polyhedron_with_complexity
-- TODO: write binding for new_Octagonal_Shape_mpz_class_from_NNC_Polyhedron_with_complexity
-- TODO: write binding for new_Octagonal_Shape_mpz_class_from_Grid_with_complexity
-- TODO: write binding for new_Octagonal_Shape_mpz_class_from_Rational_Box_with_complexity
-- TODO: write binding for new_Octagonal_Shape_mpz_class_from_BD_Shape_mpz_class_with_complexity
-- TODO: write binding for new_Octagonal_Shape_mpz_class_from_BD_Shape_mpq_class_with_complexity
-- TODO: write binding for new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_mpz_class_with_complexity
-- TODO: write binding for new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_mpq_class_with_complexity
-- TODO: write binding for new_Octagonal_Shape_mpz_class_from_Double_Box_with_complexity
-- TODO: write binding for new_Octagonal_Shape_mpz_class_from_BD_Shape_double_with_complexity
-- TODO: write binding for new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_double_with_complexity
-- TODO: write binding for new_Octagonal_Shape_mpz_class_from_Constraint_System
-- TODO: write binding for new_Octagonal_Shape_mpz_class_from_Congruence_System
-- TODO: write binding for new_Octagonal_Shape_mpz_class_from_Generator_System
-- TODO: write binding for Octagonal_Shape_mpz_class_space_dimension
-- TODO: write binding for Octagonal_Shape_mpz_class_affine_dimension
-- TODO: write binding for Octagonal_Shape_mpz_class_relation_with_Constraint
-- TODO: write binding for Octagonal_Shape_mpz_class_relation_with_Generator
-- TODO: write binding for Octagonal_Shape_mpz_class_relation_with_Congruence
-- TODO: write binding for Octagonal_Shape_mpz_class_get_constraints
-- TODO: write binding for Octagonal_Shape_mpz_class_get_congruences
-- TODO: write binding for Octagonal_Shape_mpz_class_get_minimized_constraints
-- TODO: write binding for Octagonal_Shape_mpz_class_get_minimized_congruences
-- TODO: write binding for Octagonal_Shape_mpz_class_is_empty
-- TODO: write binding for Octagonal_Shape_mpz_class_is_universe
-- TODO: write binding for Octagonal_Shape_mpz_class_is_bounded
-- TODO: write binding for Octagonal_Shape_mpz_class_contains_integer_point
-- TODO: write binding for Octagonal_Shape_mpz_class_is_topologically_closed
-- TODO: write binding for Octagonal_Shape_mpz_class_is_discrete
-- TODO: write binding for Octagonal_Shape_mpz_class_topological_closure_assign
-- TODO: write binding for Octagonal_Shape_mpz_class_bounds_from_above
-- TODO: write binding for Octagonal_Shape_mpz_class_bounds_from_below
-- TODO: write binding for Octagonal_Shape_mpz_class_maximize
-- TODO: write binding for Octagonal_Shape_mpz_class_minimize
-- TODO: write binding for Octagonal_Shape_mpz_class_maximize_with_point
-- TODO: write binding for Octagonal_Shape_mpz_class_minimize_with_point
-- TODO: write binding for Octagonal_Shape_mpz_class_frequency
-- TODO: write binding for Octagonal_Shape_mpz_class_contains_Octagonal_Shape_mpz_class
-- TODO: write binding for Octagonal_Shape_mpz_class_strictly_contains_Octagonal_Shape_mpz_class
-- TODO: write binding for Octagonal_Shape_mpz_class_is_disjoint_from_Octagonal_Shape_mpz_class
-- TODO: write binding for Octagonal_Shape_mpz_class_equals_Octagonal_Shape_mpz_class
-- TODO: write binding for Octagonal_Shape_mpz_class_OK
-- TODO: write binding for Octagonal_Shape_mpz_class_add_constraint
-- TODO: write binding for Octagonal_Shape_mpz_class_add_congruence
-- TODO: write binding for Octagonal_Shape_mpz_class_add_constraints
-- TODO: write binding for Octagonal_Shape_mpz_class_add_congruences
-- TODO: write binding for Octagonal_Shape_mpz_class_refine_with_constraint
-- TODO: write binding for Octagonal_Shape_mpz_class_refine_with_congruence
-- TODO: write binding for Octagonal_Shape_mpz_class_refine_with_constraints
-- TODO: write binding for Octagonal_Shape_mpz_class_refine_with_congruences
-- TODO: write binding for Octagonal_Shape_mpz_class_intersection_assign
-- TODO: write binding for Octagonal_Shape_mpz_class_upper_bound_assign
-- TODO: write binding for Octagonal_Shape_mpz_class_difference_assign
-- TODO: write binding for Octagonal_Shape_mpz_class_concatenate_assign
-- TODO: write binding for Octagonal_Shape_mpz_class_time_elapse_assign
-- TODO: write binding for Octagonal_Shape_mpz_class_upper_bound_assign_if_exact
-- TODO: write binding for Octagonal_Shape_mpz_class_simplify_using_context_assign
-- TODO: write binding for Octagonal_Shape_mpz_class_constrains
-- TODO: write binding for Octagonal_Shape_mpz_class_unconstrain_space_dimension
-- TODO: write binding for Octagonal_Shape_mpz_class_unconstrain_space_dimensions
-- TODO: write binding for Octagonal_Shape_mpz_class_affine_image
-- TODO: write binding for Octagonal_Shape_mpz_class_affine_preimage
-- TODO: write binding for Octagonal_Shape_mpz_class_bounded_affine_image
-- TODO: write binding for Octagonal_Shape_mpz_class_bounded_affine_preimage
-- TODO: write binding for Octagonal_Shape_mpz_class_generalized_affine_image
-- TODO: write binding for Octagonal_Shape_mpz_class_generalized_affine_preimage
-- TODO: write binding for Octagonal_Shape_mpz_class_generalized_affine_image_lhs_rhs
-- TODO: write binding for Octagonal_Shape_mpz_class_generalized_affine_preimage_lhs_rhs
-- TODO: write binding for Octagonal_Shape_mpz_class_add_space_dimensions_and_embed
-- TODO: write binding for Octagonal_Shape_mpz_class_add_space_dimensions_and_project
-- TODO: write binding for Octagonal_Shape_mpz_class_remove_space_dimensions
-- TODO: write binding for Octagonal_Shape_mpz_class_remove_higher_space_dimensions
-- TODO: write binding for Octagonal_Shape_mpz_class_expand_space_dimension
-- TODO: write binding for Octagonal_Shape_mpz_class_fold_space_dimensions
-- TODO: write binding for Octagonal_Shape_mpz_class_map_space_dimensions
-- TODO: write binding for Octagonal_Shape_mpz_class_drop_some_non_integer_points
-- TODO: write binding for Octagonal_Shape_mpz_class_drop_some_non_integer_points_2
-- TODO: write binding for Octagonal_Shape_mpz_class_external_memory_in_bytes
-- TODO: write binding for Octagonal_Shape_mpz_class_total_memory_in_bytes
-- TODO: write binding for Octagonal_Shape_mpz_class_BHMZ05_widening_assign_with_tokens
-- TODO: write binding for Octagonal_Shape_mpz_class_BHMZ05_widening_assign
-- TODO: write binding for Octagonal_Shape_mpz_class_widening_assign_with_tokens
-- TODO: write binding for Octagonal_Shape_mpz_class_widening_assign
-- TODO: write binding for Octagonal_Shape_mpz_class_limited_BHMZ05_extrapolation_assign_with_tokens
-- TODO: write binding for Octagonal_Shape_mpz_class_limited_CC76_extrapolation_assign_with_tokens
-- TODO: write binding for Octagonal_Shape_mpz_class_limited_BHMZ05_extrapolation_assign
-- TODO: write binding for Octagonal_Shape_mpz_class_limited_CC76_extrapolation_assign
-- TODO: write binding for Octagonal_Shape_mpz_class_CC76_extrapolation_assign_with_tokens
-- TODO: write binding for Octagonal_Shape_mpz_class_CC76_extrapolation_assign
-- TODO: write binding for Octagonal_Shape_mpz_class_CC76_narrowing_assign
-- TODO: write binding for Octagonal_Shape_mpz_class_linear_partition
-- TODO: write binding for Octagonal_Shape_mpz_class_wrap_assign
-- TODO: write binding for new_Octagonal_Shape_mpz_class_recycle_Constraint_System
-- TODO: write binding for new_Octagonal_Shape_mpz_class_recycle_Congruence_System
-- TODO: write binding for new_Octagonal_Shape_mpz_class_recycle_Generator_System
-- TODO: write binding for assign_Octagonal_Shape_mpz_class_from_Octagonal_Shape_mpz_class
-- TODO: write binding for Octagonal_Shape_mpz_class_add_recycled_constraints
-- TODO: write binding for Octagonal_Shape_mpz_class_add_recycled_congruences
-- TODO: write binding for termination_test_MS_Octagonal_Shape_mpz_class
-- TODO: write binding for termination_test_PR_Octagonal_Shape_mpz_class
-- TODO: write binding for one_affine_ranking_function_MS_Octagonal_Shape_mpz_class
-- TODO: write binding for one_affine_ranking_function_PR_Octagonal_Shape_mpz_class
-- TODO: write binding for all_affine_ranking_functions_MS_Octagonal_Shape_mpz_class
-- TODO: write binding for all_affine_ranking_functions_PR_Octagonal_Shape_mpz_class
-- TODO: write binding for termination_test_MS_Octagonal_Shape_mpz_class_2
-- TODO: write binding for termination_test_PR_Octagonal_Shape_mpz_class_2
-- TODO: write binding for one_affine_ranking_function_MS_Octagonal_Shape_mpz_class_2
-- TODO: write binding for one_affine_ranking_function_PR_Octagonal_Shape_mpz_class_2
-- TODO: write binding for all_affine_ranking_functions_MS_Octagonal_Shape_mpz_class_2
-- TODO: write binding for all_affine_ranking_functions_PR_Octagonal_Shape_mpz_class_2
-- TODO: write binding for delete_Octagonal_Shape_mpq_class
-- TODO: write binding for new_Octagonal_Shape_mpq_class_from_space_dimension
-- TODO: write binding for new_Octagonal_Shape_mpq_class_from_C_Polyhedron
-- TODO: write binding for new_Octagonal_Shape_mpq_class_from_NNC_Polyhedron
-- TODO: write binding for new_Octagonal_Shape_mpq_class_from_Grid
-- TODO: write binding for new_Octagonal_Shape_mpq_class_from_Rational_Box
-- TODO: write binding for new_Octagonal_Shape_mpq_class_from_BD_Shape_mpz_class
-- TODO: write binding for new_Octagonal_Shape_mpq_class_from_BD_Shape_mpq_class
-- TODO: write binding for new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_mpz_class
-- TODO: write binding for new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_mpq_class
-- TODO: write binding for new_Octagonal_Shape_mpq_class_from_Double_Box
-- TODO: write binding for new_Octagonal_Shape_mpq_class_from_BD_Shape_double
-- TODO: write binding for new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_double
-- TODO: write binding for new_Octagonal_Shape_mpq_class_from_C_Polyhedron_with_complexity
-- TODO: write binding for new_Octagonal_Shape_mpq_class_from_NNC_Polyhedron_with_complexity
-- TODO: write binding for new_Octagonal_Shape_mpq_class_from_Grid_with_complexity
-- TODO: write binding for new_Octagonal_Shape_mpq_class_from_Rational_Box_with_complexity
-- TODO: write binding for new_Octagonal_Shape_mpq_class_from_BD_Shape_mpz_class_with_complexity
-- TODO: write binding for new_Octagonal_Shape_mpq_class_from_BD_Shape_mpq_class_with_complexity
-- TODO: write binding for new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_mpz_class_with_complexity
-- TODO: write binding for new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_mpq_class_with_complexity
-- TODO: write binding for new_Octagonal_Shape_mpq_class_from_Double_Box_with_complexity
-- TODO: write binding for new_Octagonal_Shape_mpq_class_from_BD_Shape_double_with_complexity
-- TODO: write binding for new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_double_with_complexity
-- TODO: write binding for new_Octagonal_Shape_mpq_class_from_Constraint_System
-- TODO: write binding for new_Octagonal_Shape_mpq_class_from_Congruence_System
-- TODO: write binding for new_Octagonal_Shape_mpq_class_from_Generator_System
-- TODO: write binding for Octagonal_Shape_mpq_class_space_dimension
-- TODO: write binding for Octagonal_Shape_mpq_class_affine_dimension
-- TODO: write binding for Octagonal_Shape_mpq_class_relation_with_Constraint
-- TODO: write binding for Octagonal_Shape_mpq_class_relation_with_Generator
-- TODO: write binding for Octagonal_Shape_mpq_class_relation_with_Congruence
-- TODO: write binding for Octagonal_Shape_mpq_class_get_constraints
-- TODO: write binding for Octagonal_Shape_mpq_class_get_congruences
-- TODO: write binding for Octagonal_Shape_mpq_class_get_minimized_constraints
-- TODO: write binding for Octagonal_Shape_mpq_class_get_minimized_congruences
-- TODO: write binding for Octagonal_Shape_mpq_class_is_empty
-- TODO: write binding for Octagonal_Shape_mpq_class_is_universe
-- TODO: write binding for Octagonal_Shape_mpq_class_is_bounded
-- TODO: write binding for Octagonal_Shape_mpq_class_contains_integer_point
-- TODO: write binding for Octagonal_Shape_mpq_class_is_topologically_closed
-- TODO: write binding for Octagonal_Shape_mpq_class_is_discrete
-- TODO: write binding for Octagonal_Shape_mpq_class_topological_closure_assign
-- TODO: write binding for Octagonal_Shape_mpq_class_bounds_from_above
-- TODO: write binding for Octagonal_Shape_mpq_class_bounds_from_below
-- TODO: write binding for Octagonal_Shape_mpq_class_maximize
-- TODO: write binding for Octagonal_Shape_mpq_class_minimize
-- TODO: write binding for Octagonal_Shape_mpq_class_maximize_with_point
-- TODO: write binding for Octagonal_Shape_mpq_class_minimize_with_point
-- TODO: write binding for Octagonal_Shape_mpq_class_frequency
-- TODO: write binding for Octagonal_Shape_mpq_class_contains_Octagonal_Shape_mpq_class
-- TODO: write binding for Octagonal_Shape_mpq_class_strictly_contains_Octagonal_Shape_mpq_class
-- TODO: write binding for Octagonal_Shape_mpq_class_is_disjoint_from_Octagonal_Shape_mpq_class
-- TODO: write binding for Octagonal_Shape_mpq_class_equals_Octagonal_Shape_mpq_class
-- TODO: write binding for Octagonal_Shape_mpq_class_OK
-- TODO: write binding for Octagonal_Shape_mpq_class_add_constraint
-- TODO: write binding for Octagonal_Shape_mpq_class_add_congruence
-- TODO: write binding for Octagonal_Shape_mpq_class_add_constraints
-- TODO: write binding for Octagonal_Shape_mpq_class_add_congruences
-- TODO: write binding for Octagonal_Shape_mpq_class_refine_with_constraint
-- TODO: write binding for Octagonal_Shape_mpq_class_refine_with_congruence
-- TODO: write binding for Octagonal_Shape_mpq_class_refine_with_constraints
-- TODO: write binding for Octagonal_Shape_mpq_class_refine_with_congruences
-- TODO: write binding for Octagonal_Shape_mpq_class_intersection_assign
-- TODO: write binding for Octagonal_Shape_mpq_class_upper_bound_assign
-- TODO: write binding for Octagonal_Shape_mpq_class_difference_assign
-- TODO: write binding for Octagonal_Shape_mpq_class_concatenate_assign
-- TODO: write binding for Octagonal_Shape_mpq_class_time_elapse_assign
-- TODO: write binding for Octagonal_Shape_mpq_class_upper_bound_assign_if_exact
-- TODO: write binding for Octagonal_Shape_mpq_class_simplify_using_context_assign
-- TODO: write binding for Octagonal_Shape_mpq_class_constrains
-- TODO: write binding for Octagonal_Shape_mpq_class_unconstrain_space_dimension
-- TODO: write binding for Octagonal_Shape_mpq_class_unconstrain_space_dimensions
-- TODO: write binding for Octagonal_Shape_mpq_class_affine_image
-- TODO: write binding for Octagonal_Shape_mpq_class_affine_preimage
-- TODO: write binding for Octagonal_Shape_mpq_class_bounded_affine_image
-- TODO: write binding for Octagonal_Shape_mpq_class_bounded_affine_preimage
-- TODO: write binding for Octagonal_Shape_mpq_class_generalized_affine_image
-- TODO: write binding for Octagonal_Shape_mpq_class_generalized_affine_preimage
-- TODO: write binding for Octagonal_Shape_mpq_class_generalized_affine_image_lhs_rhs
-- TODO: write binding for Octagonal_Shape_mpq_class_generalized_affine_preimage_lhs_rhs
-- TODO: write binding for Octagonal_Shape_mpq_class_add_space_dimensions_and_embed
-- TODO: write binding for Octagonal_Shape_mpq_class_add_space_dimensions_and_project
-- TODO: write binding for Octagonal_Shape_mpq_class_remove_space_dimensions
-- TODO: write binding for Octagonal_Shape_mpq_class_remove_higher_space_dimensions
-- TODO: write binding for Octagonal_Shape_mpq_class_expand_space_dimension
-- TODO: write binding for Octagonal_Shape_mpq_class_fold_space_dimensions
-- TODO: write binding for Octagonal_Shape_mpq_class_map_space_dimensions
-- TODO: write binding for Octagonal_Shape_mpq_class_drop_some_non_integer_points
-- TODO: write binding for Octagonal_Shape_mpq_class_drop_some_non_integer_points_2
-- TODO: write binding for Octagonal_Shape_mpq_class_external_memory_in_bytes
-- TODO: write binding for Octagonal_Shape_mpq_class_total_memory_in_bytes
-- TODO: write binding for Octagonal_Shape_mpq_class_BHMZ05_widening_assign_with_tokens
-- TODO: write binding for Octagonal_Shape_mpq_class_BHMZ05_widening_assign
-- TODO: write binding for Octagonal_Shape_mpq_class_widening_assign_with_tokens
-- TODO: write binding for Octagonal_Shape_mpq_class_widening_assign
-- TODO: write binding for Octagonal_Shape_mpq_class_limited_BHMZ05_extrapolation_assign_with_tokens
-- TODO: write binding for Octagonal_Shape_mpq_class_limited_CC76_extrapolation_assign_with_tokens
-- TODO: write binding for Octagonal_Shape_mpq_class_limited_BHMZ05_extrapolation_assign
-- TODO: write binding for Octagonal_Shape_mpq_class_limited_CC76_extrapolation_assign
-- TODO: write binding for Octagonal_Shape_mpq_class_CC76_extrapolation_assign_with_tokens
-- TODO: write binding for Octagonal_Shape_mpq_class_CC76_extrapolation_assign
-- TODO: write binding for Octagonal_Shape_mpq_class_CC76_narrowing_assign
-- TODO: write binding for Octagonal_Shape_mpq_class_linear_partition
-- TODO: write binding for Octagonal_Shape_mpq_class_wrap_assign
-- TODO: write binding for new_Octagonal_Shape_mpq_class_recycle_Constraint_System
-- TODO: write binding for new_Octagonal_Shape_mpq_class_recycle_Congruence_System
-- TODO: write binding for new_Octagonal_Shape_mpq_class_recycle_Generator_System
-- TODO: write binding for assign_Octagonal_Shape_mpq_class_from_Octagonal_Shape_mpq_class
-- TODO: write binding for Octagonal_Shape_mpq_class_add_recycled_constraints
-- TODO: write binding for Octagonal_Shape_mpq_class_add_recycled_congruences
-- TODO: write binding for termination_test_MS_Octagonal_Shape_mpq_class
-- TODO: write binding for termination_test_PR_Octagonal_Shape_mpq_class
-- TODO: write binding for one_affine_ranking_function_MS_Octagonal_Shape_mpq_class
-- TODO: write binding for one_affine_ranking_function_PR_Octagonal_Shape_mpq_class
-- TODO: write binding for all_affine_ranking_functions_MS_Octagonal_Shape_mpq_class
-- TODO: write binding for all_affine_ranking_functions_PR_Octagonal_Shape_mpq_class
-- TODO: write binding for termination_test_MS_Octagonal_Shape_mpq_class_2
-- TODO: write binding for termination_test_PR_Octagonal_Shape_mpq_class_2
-- TODO: write binding for one_affine_ranking_function_MS_Octagonal_Shape_mpq_class_2
-- TODO: write binding for one_affine_ranking_function_PR_Octagonal_Shape_mpq_class_2
-- TODO: write binding for all_affine_ranking_functions_MS_Octagonal_Shape_mpq_class_2
-- TODO: write binding for all_affine_ranking_functions_PR_Octagonal_Shape_mpq_class_2
-- TODO: write binding for delete_Constraints_Product_C_Polyhedron_Grid
-- TODO: write binding for new_Constraints_Product_C_Polyhedron_Grid_from_space_dimension
-- TODO: write binding for new_Constraints_Product_C_Polyhedron_Grid_from_C_Polyhedron
-- TODO: write binding for new_Constraints_Product_C_Polyhedron_Grid_from_NNC_Polyhedron
-- TODO: write binding for new_Constraints_Product_C_Polyhedron_Grid_from_Grid
-- TODO: write binding for new_Constraints_Product_C_Polyhedron_Grid_from_Rational_Box
-- TODO: write binding for new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_mpz_class
-- TODO: write binding for new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_mpq_class
-- TODO: write binding for new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_mpz_class
-- TODO: write binding for new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_mpq_class
-- TODO: write binding for new_Constraints_Product_C_Polyhedron_Grid_from_Double_Box
-- TODO: write binding for new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_double
-- TODO: write binding for new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_double
-- TODO: write binding for new_Constraints_Product_C_Polyhedron_Grid_from_Constraints_Product_C_Polyhedron_Grid
-- TODO: write binding for new_Constraints_Product_C_Polyhedron_Grid_from_C_Polyhedron_with_complexity
-- TODO: write binding for new_Constraints_Product_C_Polyhedron_Grid_from_NNC_Polyhedron_with_complexity
-- TODO: write binding for new_Constraints_Product_C_Polyhedron_Grid_from_Grid_with_complexity
-- TODO: write binding for new_Constraints_Product_C_Polyhedron_Grid_from_Rational_Box_with_complexity
-- TODO: write binding for new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_mpz_class_with_complexity
-- TODO: write binding for new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_mpq_class_with_complexity
-- TODO: write binding for new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_mpz_class_with_complexity
-- TODO: write binding for new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_mpq_class_with_complexity
-- TODO: write binding for new_Constraints_Product_C_Polyhedron_Grid_from_Double_Box_with_complexity
-- TODO: write binding for new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_double_with_complexity
-- TODO: write binding for new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_double_with_complexity
-- TODO: write binding for new_Constraints_Product_C_Polyhedron_Grid_from_Constraints_Product_C_Polyhedron_Grid_with_complexity
-- TODO: write binding for new_Constraints_Product_C_Polyhedron_Grid_from_Constraint_System
-- TODO: write binding for new_Constraints_Product_C_Polyhedron_Grid_from_Congruence_System
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_space_dimension
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_affine_dimension
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_relation_with_Constraint
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_relation_with_Generator
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_relation_with_Congruence
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_is_empty
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_is_universe
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_is_bounded
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_is_topologically_closed
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_is_discrete
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_topological_closure_assign
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_bounds_from_above
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_bounds_from_below
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_maximize
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_minimize
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_maximize_with_point
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_minimize_with_point
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_contains_Constraints_Product_C_Polyhedron_Grid
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_strictly_contains_Constraints_Product_C_Polyhedron_Grid
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_is_disjoint_from_Constraints_Product_C_Polyhedron_Grid
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_equals_Constraints_Product_C_Polyhedron_Grid
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_OK
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_add_constraint
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_add_congruence
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_add_constraints
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_add_congruences
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_refine_with_constraint
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_refine_with_congruence
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_refine_with_constraints
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_refine_with_congruences
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_intersection_assign
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_upper_bound_assign
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_difference_assign
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_concatenate_assign
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_time_elapse_assign
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_upper_bound_assign_if_exact
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_constrains
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_unconstrain_space_dimension
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_unconstrain_space_dimensions
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_affine_image
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_affine_preimage
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_bounded_affine_image
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_bounded_affine_preimage
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_generalized_affine_image
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_generalized_affine_preimage
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_generalized_affine_image_lhs_rhs
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_generalized_affine_preimage_lhs_rhs
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_add_space_dimensions_and_embed
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_add_space_dimensions_and_project
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_remove_space_dimensions
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_remove_higher_space_dimensions
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_expand_space_dimension
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_fold_space_dimensions
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_map_space_dimensions
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_drop_some_non_integer_points
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_drop_some_non_integer_points_2
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_external_memory_in_bytes
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_total_memory_in_bytes
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_widening_assign_with_tokens
-- TODO: write binding for Constraints_Product_C_Polyhedron_Grid_widening_assign
-- TODO: write binding for new_Pointset_Powerset_C_Polyhedron_iterator
-- TODO: write binding for new_Pointset_Powerset_C_Polyhedron_const_iterator
-- TODO: write binding for delete_Pointset_Powerset_C_Polyhedron
-- TODO: write binding for new_Pointset_Powerset_C_Polyhedron_from_space_dimension
-- TODO: write binding for new_Pointset_Powerset_C_Polyhedron_from_Pointset_Powerset_C_Polyhedron
-- TODO: write binding for new_Pointset_Powerset_C_Polyhedron_from_C_Polyhedron
-- TODO: write binding for new_Pointset_Powerset_C_Polyhedron_from_Pointset_Powerset_C_Polyhedron_with_complexity
-- TODO: write binding for new_Pointset_Powerset_C_Polyhedron_from_C_Polyhedron_with_complexity
-- TODO: write binding for new_Pointset_Powerset_C_Polyhedron_from_Constraint_System
-- TODO: write binding for new_Pointset_Powerset_C_Polyhedron_from_Congruence_System
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_space_dimension
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_affine_dimension
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_relation_with_Constraint
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_relation_with_Generator
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_relation_with_Congruence
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_is_empty
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_is_universe
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_is_bounded
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_contains_integer_point
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_is_topologically_closed
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_is_discrete
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_topological_closure_assign
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_pairwise_reduce
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_omega_reduce
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_bounds_from_above
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_bounds_from_below
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_maximize
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_minimize
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_maximize_with_point
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_minimize_with_point
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_contains_Pointset_Powerset_C_Polyhedron
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_strictly_contains_Pointset_Powerset_C_Polyhedron
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_is_disjoint_from_Pointset_Powerset_C_Polyhedron
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_geometrically_covers_Pointset_Powerset_C_Polyhedron
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_geometrically_equals_Pointset_Powerset_C_Polyhedron
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_equals_Pointset_Powerset_C_Polyhedron
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_OK
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_add_constraint
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_add_congruence
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_add_constraints
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_add_congruences
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_refine_with_constraint
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_refine_with_congruence
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_refine_with_constraints
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_refine_with_congruences
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_intersection_assign
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_upper_bound_assign
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_difference_assign
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_concatenate_assign
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_time_elapse_assign
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_upper_bound_assign_if_exact
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_simplify_using_context_assign
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_constrains
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_unconstrain_space_dimension
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_unconstrain_space_dimensions
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_affine_image
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_affine_preimage
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_bounded_affine_image
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_bounded_affine_preimage
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_generalized_affine_image
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_generalized_affine_preimage
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_generalized_affine_image_lhs_rhs
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_generalized_affine_preimage_lhs_rhs
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_add_space_dimensions_and_embed
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_add_space_dimensions_and_project
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_remove_space_dimensions
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_remove_higher_space_dimensions
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_expand_space_dimension
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_fold_space_dimensions
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_map_space_dimensions
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_drop_some_non_integer_points
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_drop_some_non_integer_points_2
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_external_memory_in_bytes
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_total_memory_in_bytes
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_size
-- TODO: write binding for new_Pointset_Powerset_C_Polyhedron_iterator_from_iterator
-- TODO: write binding for new_Pointset_Powerset_C_Polyhedron_const_iterator_from_const_iterator
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_iterator_begin
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_const_iterator_begin
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_iterator_end
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_const_iterator_end
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_iterator_equal_test
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_const_iterator_equal_test
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_iterator_increment
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_const_iterator_increment
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_iterator_decrement
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_const_iterator_decrement
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_iterator_dereference
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_const_iterator_dereference
-- TODO: write binding for delete_Pointset_Powerset_C_Polyhedron_iterator
-- TODO: write binding for delete_Pointset_Powerset_C_Polyhedron_const_iterator
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_add_disjunct
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_drop_disjunct
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_drop_disjuncts
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_BHZ03_BHRZ03_BHRZ03_widening_assign
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_BHZ03_H79_H79_widening_assign
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_BGP99_BHRZ03_extrapolation_assign
-- TODO: write binding for Pointset_Powerset_C_Polyhedron_BGP99_H79_extrapolation_assign
-- TODO: write binding for new_Pointset_Powerset_NNC_Polyhedron_iterator
-- TODO: write binding for new_Pointset_Powerset_NNC_Polyhedron_const_iterator
-- TODO: write binding for delete_Pointset_Powerset_NNC_Polyhedron
-- TODO: write binding for new_Pointset_Powerset_NNC_Polyhedron_from_space_dimension
-- TODO: write binding for new_Pointset_Powerset_NNC_Polyhedron_from_Pointset_Powerset_NNC_Polyhedron
-- TODO: write binding for new_Pointset_Powerset_NNC_Polyhedron_from_NNC_Polyhedron
-- TODO: write binding for new_Pointset_Powerset_NNC_Polyhedron_from_Pointset_Powerset_NNC_Polyhedron_with_complexity
-- TODO: write binding for new_Pointset_Powerset_NNC_Polyhedron_from_NNC_Polyhedron_with_complexity
-- TODO: write binding for new_Pointset_Powerset_NNC_Polyhedron_from_Constraint_System
-- TODO: write binding for new_Pointset_Powerset_NNC_Polyhedron_from_Congruence_System
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_space_dimension
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_affine_dimension
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_relation_with_Constraint
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_relation_with_Generator
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_relation_with_Congruence
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_is_empty
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_is_universe
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_is_bounded
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_contains_integer_point
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_is_topologically_closed
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_is_discrete
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_topological_closure_assign
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_pairwise_reduce
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_omega_reduce
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_bounds_from_above
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_bounds_from_below
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_maximize
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_minimize
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_maximize_with_point
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_minimize_with_point
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_contains_Pointset_Powerset_NNC_Polyhedron
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_strictly_contains_Pointset_Powerset_NNC_Polyhedron
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_is_disjoint_from_Pointset_Powerset_NNC_Polyhedron
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_geometrically_covers_Pointset_Powerset_NNC_Polyhedron
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_geometrically_equals_Pointset_Powerset_NNC_Polyhedron
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_equals_Pointset_Powerset_NNC_Polyhedron
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_OK
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_add_constraint
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_add_congruence
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_add_constraints
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_add_congruences
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_refine_with_constraint
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_refine_with_congruence
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_refine_with_constraints
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_refine_with_congruences
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_intersection_assign
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_upper_bound_assign
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_difference_assign
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_concatenate_assign
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_time_elapse_assign
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_upper_bound_assign_if_exact
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_simplify_using_context_assign
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_constrains
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_unconstrain_space_dimension
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_unconstrain_space_dimensions
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_affine_image
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_affine_preimage
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_bounded_affine_image
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_bounded_affine_preimage
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_generalized_affine_image
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_generalized_affine_preimage
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_generalized_affine_image_lhs_rhs
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_generalized_affine_preimage_lhs_rhs
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_add_space_dimensions_and_embed
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_add_space_dimensions_and_project
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_remove_space_dimensions
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_remove_higher_space_dimensions
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_expand_space_dimension
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_fold_space_dimensions
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_map_space_dimensions
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_drop_some_non_integer_points
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_drop_some_non_integer_points_2
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_external_memory_in_bytes
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_total_memory_in_bytes
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_size
-- TODO: write binding for new_Pointset_Powerset_NNC_Polyhedron_iterator_from_iterator
-- TODO: write binding for new_Pointset_Powerset_NNC_Polyhedron_const_iterator_from_const_iterator
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_iterator_begin
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_const_iterator_begin
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_iterator_end
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_const_iterator_end
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_iterator_equal_test
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_const_iterator_equal_test
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_iterator_increment
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_const_iterator_increment
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_iterator_decrement
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_const_iterator_decrement
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_iterator_dereference
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_const_iterator_dereference
-- TODO: write binding for delete_Pointset_Powerset_NNC_Polyhedron_iterator
-- TODO: write binding for delete_Pointset_Powerset_NNC_Polyhedron_const_iterator
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_add_disjunct
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_drop_disjunct
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_drop_disjuncts
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_BHZ03_BHRZ03_BHRZ03_widening_assign
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_BHZ03_H79_H79_widening_assign
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_BGP99_BHRZ03_extrapolation_assign
-- TODO: write binding for Pointset_Powerset_NNC_Polyhedron_BGP99_H79_extrapolation_assign
-- TODO: write binding for delete_Double_Box
-- TODO: write binding for new_Double_Box_from_space_dimension
-- TODO: write binding for new_Double_Box_from_C_Polyhedron
-- TODO: write binding for new_Double_Box_from_NNC_Polyhedron
-- TODO: write binding for new_Double_Box_from_Grid
-- TODO: write binding for new_Double_Box_from_Rational_Box
-- TODO: write binding for new_Double_Box_from_BD_Shape_mpz_class
-- TODO: write binding for new_Double_Box_from_BD_Shape_mpq_class
-- TODO: write binding for new_Double_Box_from_Octagonal_Shape_mpz_class
-- TODO: write binding for new_Double_Box_from_Octagonal_Shape_mpq_class
-- TODO: write binding for new_Double_Box_from_Double_Box
-- TODO: write binding for new_Double_Box_from_BD_Shape_double
-- TODO: write binding for new_Double_Box_from_Octagonal_Shape_double
-- TODO: write binding for new_Double_Box_from_C_Polyhedron_with_complexity
-- TODO: write binding for new_Double_Box_from_NNC_Polyhedron_with_complexity
-- TODO: write binding for new_Double_Box_from_Grid_with_complexity
-- TODO: write binding for new_Double_Box_from_Rational_Box_with_complexity
-- TODO: write binding for new_Double_Box_from_BD_Shape_mpz_class_with_complexity
-- TODO: write binding for new_Double_Box_from_BD_Shape_mpq_class_with_complexity
-- TODO: write binding for new_Double_Box_from_Octagonal_Shape_mpz_class_with_complexity
-- TODO: write binding for new_Double_Box_from_Octagonal_Shape_mpq_class_with_complexity
-- TODO: write binding for new_Double_Box_from_Double_Box_with_complexity
-- TODO: write binding for new_Double_Box_from_BD_Shape_double_with_complexity
-- TODO: write binding for new_Double_Box_from_Octagonal_Shape_double_with_complexity
-- TODO: write binding for new_Double_Box_from_Constraint_System
-- TODO: write binding for new_Double_Box_from_Congruence_System
-- TODO: write binding for new_Double_Box_from_Generator_System
-- TODO: write binding for Double_Box_space_dimension
-- TODO: write binding for Double_Box_affine_dimension
-- TODO: write binding for Double_Box_relation_with_Constraint
-- TODO: write binding for Double_Box_relation_with_Generator
-- TODO: write binding for Double_Box_relation_with_Congruence
-- TODO: write binding for Double_Box_get_constraints
-- TODO: write binding for Double_Box_get_congruences
-- TODO: write binding for Double_Box_get_minimized_constraints
-- TODO: write binding for Double_Box_get_minimized_congruences
-- TODO: write binding for Double_Box_is_empty
-- TODO: write binding for Double_Box_is_universe
-- TODO: write binding for Double_Box_is_bounded
-- TODO: write binding for Double_Box_contains_integer_point
-- TODO: write binding for Double_Box_is_topologically_closed
-- TODO: write binding for Double_Box_is_discrete
-- TODO: write binding for Double_Box_topological_closure_assign
-- TODO: write binding for Double_Box_bounds_from_above
-- TODO: write binding for Double_Box_bounds_from_below
-- TODO: write binding for Double_Box_maximize
-- TODO: write binding for Double_Box_minimize
-- TODO: write binding for Double_Box_maximize_with_point
-- TODO: write binding for Double_Box_minimize_with_point
-- TODO: write binding for Double_Box_frequency
-- TODO: write binding for Double_Box_contains_Double_Box
-- TODO: write binding for Double_Box_strictly_contains_Double_Box
-- TODO: write binding for Double_Box_is_disjoint_from_Double_Box
-- TODO: write binding for Double_Box_equals_Double_Box
-- TODO: write binding for Double_Box_OK
-- TODO: write binding for Double_Box_add_constraint
-- TODO: write binding for Double_Box_add_congruence
-- TODO: write binding for Double_Box_add_constraints
-- TODO: write binding for Double_Box_add_congruences
-- TODO: write binding for Double_Box_refine_with_constraint
-- TODO: write binding for Double_Box_refine_with_congruence
-- TODO: write binding for Double_Box_refine_with_constraints
-- TODO: write binding for Double_Box_refine_with_congruences
-- TODO: write binding for Double_Box_intersection_assign
-- TODO: write binding for Double_Box_upper_bound_assign
-- TODO: write binding for Double_Box_difference_assign
-- TODO: write binding for Double_Box_concatenate_assign
-- TODO: write binding for Double_Box_time_elapse_assign
-- TODO: write binding for Double_Box_upper_bound_assign_if_exact
-- TODO: write binding for Double_Box_simplify_using_context_assign
-- TODO: write binding for Double_Box_constrains
-- TODO: write binding for Double_Box_unconstrain_space_dimension
-- TODO: write binding for Double_Box_unconstrain_space_dimensions
-- TODO: write binding for Double_Box_affine_image
-- TODO: write binding for Double_Box_affine_preimage
-- TODO: write binding for Double_Box_bounded_affine_image
-- TODO: write binding for Double_Box_bounded_affine_preimage
-- TODO: write binding for Double_Box_generalized_affine_image
-- TODO: write binding for Double_Box_generalized_affine_preimage
-- TODO: write binding for Double_Box_generalized_affine_image_lhs_rhs
-- TODO: write binding for Double_Box_generalized_affine_preimage_lhs_rhs
-- TODO: write binding for Double_Box_add_space_dimensions_and_embed
-- TODO: write binding for Double_Box_add_space_dimensions_and_project
-- TODO: write binding for Double_Box_remove_space_dimensions
-- TODO: write binding for Double_Box_remove_higher_space_dimensions
-- TODO: write binding for Double_Box_expand_space_dimension
-- TODO: write binding for Double_Box_fold_space_dimensions
-- TODO: write binding for Double_Box_map_space_dimensions
-- TODO: write binding for Double_Box_drop_some_non_integer_points
-- TODO: write binding for Double_Box_drop_some_non_integer_points_2
-- TODO: write binding for Double_Box_external_memory_in_bytes
-- TODO: write binding for Double_Box_total_memory_in_bytes
-- TODO: write binding for Double_Box_CC76_widening_assign_with_tokens
-- TODO: write binding for Double_Box_CC76_widening_assign
-- TODO: write binding for Double_Box_widening_assign_with_tokens
-- TODO: write binding for Double_Box_widening_assign
-- TODO: write binding for Double_Box_limited_CC76_extrapolation_assign_with_tokens
-- TODO: write binding for Double_Box_limited_CC76_extrapolation_assign
-- TODO: write binding for Double_Box_linear_partition
-- TODO: write binding for Double_Box_wrap_assign
-- TODO: write binding for new_Double_Box_recycle_Constraint_System
-- TODO: write binding for new_Double_Box_recycle_Congruence_System
-- TODO: write binding for new_Double_Box_recycle_Generator_System
-- TODO: write binding for assign_Double_Box_from_Double_Box
-- TODO: write binding for Double_Box_add_recycled_constraints
-- TODO: write binding for Double_Box_add_recycled_congruences
-- TODO: write binding for termination_test_MS_Double_Box
-- TODO: write binding for termination_test_PR_Double_Box
-- TODO: write binding for one_affine_ranking_function_MS_Double_Box
-- TODO: write binding for one_affine_ranking_function_PR_Double_Box
-- TODO: write binding for all_affine_ranking_functions_MS_Double_Box
-- TODO: write binding for all_affine_ranking_functions_PR_Double_Box
-- TODO: write binding for termination_test_MS_Double_Box_2
-- TODO: write binding for termination_test_PR_Double_Box_2
-- TODO: write binding for one_affine_ranking_function_MS_Double_Box_2
-- TODO: write binding for one_affine_ranking_function_PR_Double_Box_2
-- TODO: write binding for all_affine_ranking_functions_MS_Double_Box_2
-- TODO: write binding for all_affine_ranking_functions_PR_Double_Box_2
-- TODO: write binding for delete_BD_Shape_double
-- TODO: write binding for new_BD_Shape_double_from_space_dimension
-- TODO: write binding for new_BD_Shape_double_from_C_Polyhedron
-- TODO: write binding for new_BD_Shape_double_from_NNC_Polyhedron
-- TODO: write binding for new_BD_Shape_double_from_Grid
-- TODO: write binding for new_BD_Shape_double_from_Rational_Box
-- TODO: write binding for new_BD_Shape_double_from_BD_Shape_mpz_class
-- TODO: write binding for new_BD_Shape_double_from_BD_Shape_mpq_class
-- TODO: write binding for new_BD_Shape_double_from_Octagonal_Shape_mpz_class
-- TODO: write binding for new_BD_Shape_double_from_Octagonal_Shape_mpq_class
-- TODO: write binding for new_BD_Shape_double_from_Double_Box
-- TODO: write binding for new_BD_Shape_double_from_BD_Shape_double
-- TODO: write binding for new_BD_Shape_double_from_Octagonal_Shape_double
-- TODO: write binding for new_BD_Shape_double_from_C_Polyhedron_with_complexity
-- TODO: write binding for new_BD_Shape_double_from_NNC_Polyhedron_with_complexity
-- TODO: write binding for new_BD_Shape_double_from_Grid_with_complexity
-- TODO: write binding for new_BD_Shape_double_from_Rational_Box_with_complexity
-- TODO: write binding for new_BD_Shape_double_from_BD_Shape_mpz_class_with_complexity
-- TODO: write binding for new_BD_Shape_double_from_BD_Shape_mpq_class_with_complexity
-- TODO: write binding for new_BD_Shape_double_from_Octagonal_Shape_mpz_class_with_complexity
-- TODO: write binding for new_BD_Shape_double_from_Octagonal_Shape_mpq_class_with_complexity
-- TODO: write binding for new_BD_Shape_double_from_Double_Box_with_complexity
-- TODO: write binding for new_BD_Shape_double_from_BD_Shape_double_with_complexity
-- TODO: write binding for new_BD_Shape_double_from_Octagonal_Shape_double_with_complexity
-- TODO: write binding for new_BD_Shape_double_from_Constraint_System
-- TODO: write binding for new_BD_Shape_double_from_Congruence_System
-- TODO: write binding for new_BD_Shape_double_from_Generator_System
-- TODO: write binding for BD_Shape_double_space_dimension
-- TODO: write binding for BD_Shape_double_affine_dimension
-- TODO: write binding for BD_Shape_double_relation_with_Constraint
-- TODO: write binding for BD_Shape_double_relation_with_Generator
-- TODO: write binding for BD_Shape_double_relation_with_Congruence
-- TODO: write binding for BD_Shape_double_get_constraints
-- TODO: write binding for BD_Shape_double_get_congruences
-- TODO: write binding for BD_Shape_double_get_minimized_constraints
-- TODO: write binding for BD_Shape_double_get_minimized_congruences
-- TODO: write binding for BD_Shape_double_is_empty
-- TODO: write binding for BD_Shape_double_is_universe
-- TODO: write binding for BD_Shape_double_is_bounded
-- TODO: write binding for BD_Shape_double_contains_integer_point
-- TODO: write binding for BD_Shape_double_is_topologically_closed
-- TODO: write binding for BD_Shape_double_is_discrete
-- TODO: write binding for BD_Shape_double_topological_closure_assign
-- TODO: write binding for BD_Shape_double_bounds_from_above
-- TODO: write binding for BD_Shape_double_bounds_from_below
-- TODO: write binding for BD_Shape_double_maximize
-- TODO: write binding for BD_Shape_double_minimize
-- TODO: write binding for BD_Shape_double_maximize_with_point
-- TODO: write binding for BD_Shape_double_minimize_with_point
-- TODO: write binding for BD_Shape_double_frequency
-- TODO: write binding for BD_Shape_double_contains_BD_Shape_double
-- TODO: write binding for BD_Shape_double_strictly_contains_BD_Shape_double
-- TODO: write binding for BD_Shape_double_is_disjoint_from_BD_Shape_double
-- TODO: write binding for BD_Shape_double_equals_BD_Shape_double
-- TODO: write binding for BD_Shape_double_OK
-- TODO: write binding for BD_Shape_double_add_constraint
-- TODO: write binding for BD_Shape_double_add_congruence
-- TODO: write binding for BD_Shape_double_add_constraints
-- TODO: write binding for BD_Shape_double_add_congruences
-- TODO: write binding for BD_Shape_double_refine_with_constraint
-- TODO: write binding for BD_Shape_double_refine_with_congruence
-- TODO: write binding for BD_Shape_double_refine_with_constraints
-- TODO: write binding for BD_Shape_double_refine_with_congruences
-- TODO: write binding for BD_Shape_double_intersection_assign
-- TODO: write binding for BD_Shape_double_upper_bound_assign
-- TODO: write binding for BD_Shape_double_difference_assign
-- TODO: write binding for BD_Shape_double_concatenate_assign
-- TODO: write binding for BD_Shape_double_time_elapse_assign
-- TODO: write binding for BD_Shape_double_upper_bound_assign_if_exact
-- TODO: write binding for BD_Shape_double_simplify_using_context_assign
-- TODO: write binding for BD_Shape_double_constrains
-- TODO: write binding for BD_Shape_double_unconstrain_space_dimension
-- TODO: write binding for BD_Shape_double_unconstrain_space_dimensions
-- TODO: write binding for BD_Shape_double_affine_image
-- TODO: write binding for BD_Shape_double_affine_preimage
-- TODO: write binding for BD_Shape_double_bounded_affine_image
-- TODO: write binding for BD_Shape_double_bounded_affine_preimage
-- TODO: write binding for BD_Shape_double_generalized_affine_image
-- TODO: write binding for BD_Shape_double_generalized_affine_preimage
-- TODO: write binding for BD_Shape_double_generalized_affine_image_lhs_rhs
-- TODO: write binding for BD_Shape_double_generalized_affine_preimage_lhs_rhs
-- TODO: write binding for BD_Shape_double_add_space_dimensions_and_embed
-- TODO: write binding for BD_Shape_double_add_space_dimensions_and_project
-- TODO: write binding for BD_Shape_double_remove_space_dimensions
-- TODO: write binding for BD_Shape_double_remove_higher_space_dimensions
-- TODO: write binding for BD_Shape_double_expand_space_dimension
-- TODO: write binding for BD_Shape_double_fold_space_dimensions
-- TODO: write binding for BD_Shape_double_map_space_dimensions
-- TODO: write binding for BD_Shape_double_drop_some_non_integer_points
-- TODO: write binding for BD_Shape_double_drop_some_non_integer_points_2
-- TODO: write binding for BD_Shape_double_external_memory_in_bytes
-- TODO: write binding for BD_Shape_double_total_memory_in_bytes
-- TODO: write binding for BD_Shape_double_BHMZ05_widening_assign_with_tokens
-- TODO: write binding for BD_Shape_double_H79_widening_assign_with_tokens
-- TODO: write binding for BD_Shape_double_BHMZ05_widening_assign
-- TODO: write binding for BD_Shape_double_H79_widening_assign
-- TODO: write binding for BD_Shape_double_widening_assign_with_tokens
-- TODO: write binding for BD_Shape_double_widening_assign
-- TODO: write binding for BD_Shape_double_limited_BHMZ05_extrapolation_assign_with_tokens
-- TODO: write binding for BD_Shape_double_limited_H79_extrapolation_assign_with_tokens
-- TODO: write binding for BD_Shape_double_limited_CC76_extrapolation_assign_with_tokens
-- TODO: write binding for BD_Shape_double_limited_BHMZ05_extrapolation_assign
-- TODO: write binding for BD_Shape_double_limited_H79_extrapolation_assign
-- TODO: write binding for BD_Shape_double_limited_CC76_extrapolation_assign
-- TODO: write binding for BD_Shape_double_CC76_extrapolation_assign_with_tokens
-- TODO: write binding for BD_Shape_double_CC76_extrapolation_assign
-- TODO: write binding for BD_Shape_double_CC76_narrowing_assign
-- TODO: write binding for BD_Shape_double_linear_partition
-- TODO: write binding for BD_Shape_double_wrap_assign
-- TODO: write binding for new_BD_Shape_double_recycle_Constraint_System
-- TODO: write binding for new_BD_Shape_double_recycle_Congruence_System
-- TODO: write binding for new_BD_Shape_double_recycle_Generator_System
-- TODO: write binding for assign_BD_Shape_double_from_BD_Shape_double
-- TODO: write binding for BD_Shape_double_add_recycled_constraints
-- TODO: write binding for BD_Shape_double_add_recycled_congruences
-- TODO: write binding for termination_test_MS_BD_Shape_double
-- TODO: write binding for termination_test_PR_BD_Shape_double
-- TODO: write binding for one_affine_ranking_function_MS_BD_Shape_double
-- TODO: write binding for one_affine_ranking_function_PR_BD_Shape_double
-- TODO: write binding for all_affine_ranking_functions_MS_BD_Shape_double
-- TODO: write binding for all_affine_ranking_functions_PR_BD_Shape_double
-- TODO: write binding for termination_test_MS_BD_Shape_double_2
-- TODO: write binding for termination_test_PR_BD_Shape_double_2
-- TODO: write binding for one_affine_ranking_function_MS_BD_Shape_double_2
-- TODO: write binding for one_affine_ranking_function_PR_BD_Shape_double_2
-- TODO: write binding for all_affine_ranking_functions_MS_BD_Shape_double_2
-- TODO: write binding for all_affine_ranking_functions_PR_BD_Shape_double_2
-- TODO: write binding for delete_Octagonal_Shape_double
-- TODO: write binding for new_Octagonal_Shape_double_from_space_dimension
-- TODO: write binding for new_Octagonal_Shape_double_from_C_Polyhedron
-- TODO: write binding for new_Octagonal_Shape_double_from_NNC_Polyhedron
-- TODO: write binding for new_Octagonal_Shape_double_from_Grid
-- TODO: write binding for new_Octagonal_Shape_double_from_Rational_Box
-- TODO: write binding for new_Octagonal_Shape_double_from_BD_Shape_mpz_class
-- TODO: write binding for new_Octagonal_Shape_double_from_BD_Shape_mpq_class
-- TODO: write binding for new_Octagonal_Shape_double_from_Octagonal_Shape_mpz_class
-- TODO: write binding for new_Octagonal_Shape_double_from_Octagonal_Shape_mpq_class
-- TODO: write binding for new_Octagonal_Shape_double_from_Double_Box
-- TODO: write binding for new_Octagonal_Shape_double_from_BD_Shape_double
-- TODO: write binding for new_Octagonal_Shape_double_from_Octagonal_Shape_double
-- TODO: write binding for new_Octagonal_Shape_double_from_C_Polyhedron_with_complexity
-- TODO: write binding for new_Octagonal_Shape_double_from_NNC_Polyhedron_with_complexity
-- TODO: write binding for new_Octagonal_Shape_double_from_Grid_with_complexity
-- TODO: write binding for new_Octagonal_Shape_double_from_Rational_Box_with_complexity
-- TODO: write binding for new_Octagonal_Shape_double_from_BD_Shape_mpz_class_with_complexity
-- TODO: write binding for new_Octagonal_Shape_double_from_BD_Shape_mpq_class_with_complexity
-- TODO: write binding for new_Octagonal_Shape_double_from_Octagonal_Shape_mpz_class_with_complexity
-- TODO: write binding for new_Octagonal_Shape_double_from_Octagonal_Shape_mpq_class_with_complexity
-- TODO: write binding for new_Octagonal_Shape_double_from_Double_Box_with_complexity
-- TODO: write binding for new_Octagonal_Shape_double_from_BD_Shape_double_with_complexity
-- TODO: write binding for new_Octagonal_Shape_double_from_Octagonal_Shape_double_with_complexity
-- TODO: write binding for new_Octagonal_Shape_double_from_Constraint_System
-- TODO: write binding for new_Octagonal_Shape_double_from_Congruence_System
-- TODO: write binding for new_Octagonal_Shape_double_from_Generator_System
-- TODO: write binding for Octagonal_Shape_double_space_dimension
-- TODO: write binding for Octagonal_Shape_double_affine_dimension
-- TODO: write binding for Octagonal_Shape_double_relation_with_Constraint
-- TODO: write binding for Octagonal_Shape_double_relation_with_Generator
-- TODO: write binding for Octagonal_Shape_double_relation_with_Congruence
-- TODO: write binding for Octagonal_Shape_double_get_constraints
-- TODO: write binding for Octagonal_Shape_double_get_congruences
-- TODO: write binding for Octagonal_Shape_double_get_minimized_constraints
-- TODO: write binding for Octagonal_Shape_double_get_minimized_congruences
-- TODO: write binding for Octagonal_Shape_double_is_empty
-- TODO: write binding for Octagonal_Shape_double_is_universe
-- TODO: write binding for Octagonal_Shape_double_is_bounded
-- TODO: write binding for Octagonal_Shape_double_contains_integer_point
-- TODO: write binding for Octagonal_Shape_double_is_topologically_closed
-- TODO: write binding for Octagonal_Shape_double_is_discrete
-- TODO: write binding for Octagonal_Shape_double_topological_closure_assign
-- TODO: write binding for Octagonal_Shape_double_bounds_from_above
-- TODO: write binding for Octagonal_Shape_double_bounds_from_below
-- TODO: write binding for Octagonal_Shape_double_maximize
-- TODO: write binding for Octagonal_Shape_double_minimize
-- TODO: write binding for Octagonal_Shape_double_maximize_with_point
-- TODO: write binding for Octagonal_Shape_double_minimize_with_point
-- TODO: write binding for Octagonal_Shape_double_frequency
-- TODO: write binding for Octagonal_Shape_double_contains_Octagonal_Shape_double
-- TODO: write binding for Octagonal_Shape_double_strictly_contains_Octagonal_Shape_double
-- TODO: write binding for Octagonal_Shape_double_is_disjoint_from_Octagonal_Shape_double
-- TODO: write binding for Octagonal_Shape_double_equals_Octagonal_Shape_double
-- TODO: write binding for Octagonal_Shape_double_OK
-- TODO: write binding for Octagonal_Shape_double_add_constraint
-- TODO: write binding for Octagonal_Shape_double_add_congruence
-- TODO: write binding for Octagonal_Shape_double_add_constraints
-- TODO: write binding for Octagonal_Shape_double_add_congruences
-- TODO: write binding for Octagonal_Shape_double_refine_with_constraint
-- TODO: write binding for Octagonal_Shape_double_refine_with_congruence
-- TODO: write binding for Octagonal_Shape_double_refine_with_constraints
-- TODO: write binding for Octagonal_Shape_double_refine_with_congruences
-- TODO: write binding for Octagonal_Shape_double_intersection_assign
-- TODO: write binding for Octagonal_Shape_double_upper_bound_assign
-- TODO: write binding for Octagonal_Shape_double_difference_assign
-- TODO: write binding for Octagonal_Shape_double_concatenate_assign
-- TODO: write binding for Octagonal_Shape_double_time_elapse_assign
-- TODO: write binding for Octagonal_Shape_double_upper_bound_assign_if_exact
-- TODO: write binding for Octagonal_Shape_double_simplify_using_context_assign
-- TODO: write binding for Octagonal_Shape_double_constrains
-- TODO: write binding for Octagonal_Shape_double_unconstrain_space_dimension
-- TODO: write binding for Octagonal_Shape_double_unconstrain_space_dimensions
-- TODO: write binding for Octagonal_Shape_double_affine_image
-- TODO: write binding for Octagonal_Shape_double_affine_preimage
-- TODO: write binding for Octagonal_Shape_double_bounded_affine_image
-- TODO: write binding for Octagonal_Shape_double_bounded_affine_preimage
-- TODO: write binding for Octagonal_Shape_double_generalized_affine_image
-- TODO: write binding for Octagonal_Shape_double_generalized_affine_preimage
-- TODO: write binding for Octagonal_Shape_double_generalized_affine_image_lhs_rhs
-- TODO: write binding for Octagonal_Shape_double_generalized_affine_preimage_lhs_rhs
-- TODO: write binding for Octagonal_Shape_double_add_space_dimensions_and_embed
-- TODO: write binding for Octagonal_Shape_double_add_space_dimensions_and_project
-- TODO: write binding for Octagonal_Shape_double_remove_space_dimensions
-- TODO: write binding for Octagonal_Shape_double_remove_higher_space_dimensions
-- TODO: write binding for Octagonal_Shape_double_expand_space_dimension
-- TODO: write binding for Octagonal_Shape_double_fold_space_dimensions
-- TODO: write binding for Octagonal_Shape_double_map_space_dimensions
-- TODO: write binding for Octagonal_Shape_double_drop_some_non_integer_points
-- TODO: write binding for Octagonal_Shape_double_drop_some_non_integer_points_2
-- TODO: write binding for Octagonal_Shape_double_external_memory_in_bytes
-- TODO: write binding for Octagonal_Shape_double_total_memory_in_bytes
-- TODO: write binding for Octagonal_Shape_double_BHMZ05_widening_assign_with_tokens
-- TODO: write binding for Octagonal_Shape_double_BHMZ05_widening_assign
-- TODO: write binding for Octagonal_Shape_double_widening_assign_with_tokens
-- TODO: write binding for Octagonal_Shape_double_widening_assign
-- TODO: write binding for Octagonal_Shape_double_limited_BHMZ05_extrapolation_assign_with_tokens
-- TODO: write binding for Octagonal_Shape_double_limited_CC76_extrapolation_assign_with_tokens
-- TODO: write binding for Octagonal_Shape_double_limited_BHMZ05_extrapolation_assign
-- TODO: write binding for Octagonal_Shape_double_limited_CC76_extrapolation_assign
-- TODO: write binding for Octagonal_Shape_double_CC76_extrapolation_assign_with_tokens
-- TODO: write binding for Octagonal_Shape_double_CC76_extrapolation_assign
-- TODO: write binding for Octagonal_Shape_double_CC76_narrowing_assign
-- TODO: write binding for Octagonal_Shape_double_linear_partition
-- TODO: write binding for Octagonal_Shape_double_wrap_assign
-- TODO: write binding for new_Octagonal_Shape_double_recycle_Constraint_System
-- TODO: write binding for new_Octagonal_Shape_double_recycle_Congruence_System
-- TODO: write binding for new_Octagonal_Shape_double_recycle_Generator_System
-- TODO: write binding for assign_Octagonal_Shape_double_from_Octagonal_Shape_double
-- TODO: write binding for Octagonal_Shape_double_add_recycled_constraints
-- TODO: write binding for Octagonal_Shape_double_add_recycled_congruences
-- TODO: write binding for termination_test_MS_Octagonal_Shape_double
-- TODO: write binding for termination_test_PR_Octagonal_Shape_double
-- TODO: write binding for one_affine_ranking_function_MS_Octagonal_Shape_double
-- TODO: write binding for one_affine_ranking_function_PR_Octagonal_Shape_double
-- TODO: write binding for all_affine_ranking_functions_MS_Octagonal_Shape_double
-- TODO: write binding for all_affine_ranking_functions_PR_Octagonal_Shape_double
-- TODO: write binding for termination_test_MS_Octagonal_Shape_double_2
-- TODO: write binding for termination_test_PR_Octagonal_Shape_double_2
-- TODO: write binding for one_affine_ranking_function_MS_Octagonal_Shape_double_2
-- TODO: write binding for one_affine_ranking_function_PR_Octagonal_Shape_double_2
-- TODO: write binding for all_affine_ranking_functions_MS_Octagonal_Shape_double_2
-- TODO: write binding for all_affine_ranking_functions_PR_Octagonal_Shape_double_2