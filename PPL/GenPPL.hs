module Main where

import Data.Char
import Data.List
import Data.Maybe

main = do
  pre <- readFile "PPL.hsc.in"
  con <- readFile "ppl_c.i"
  let res = parse con
  writeFile "PPL.hsc" ((ss pre.gen (map handleException (nub res))) "")

parse :: String -> [Def]
parse ('\n':'i':'n':'t':'\n':'p':'p':'l':'_':xs) =
  let
    (name,rem1) = span (`notElem` "(\n ") xs
    (def,rem2)  = getFunParams name (dropWhile (`elem` "(\n ") rem1)
  in def:parse xs
parse (x:xs) = parse xs
parse [] = []

data Def = Def {
    cName :: String,
    hName :: String,
    constr :: Bool,
    destr :: Bool,
    ret  :: PPLRetType,
    args :: [(Bool, PPLType)]
  }
  | Ignore {
    cName :: String
  }
  deriving (Show, Eq)
		  
data PPLRetType
 = PTInt
 | PTDim
 | PTBool
 | PTErrorCode
   deriving (Show,Eq)

data PPLType
 = PTDimension
 | PTLinearExpression
 | PTCoefficient
 | PTConstraint
 | PTConstraintType
 | PTConstraintSystem
 | PTConstraintSystemIterator
 | PTGenerator
 | PTGeneratorType
 | PTGeneratorSystem
 | PTGeneratorSystemIterator
 | PTPolyhedron
 | PTUnsigned
 | PTBoundingBox
   deriving (Show,Eq)

handleException :: Def -> Def
-- all *space_dimensions have turned to PTBool in getFunParams
handleException all@(Def { cName="ppl_max_space_dimension" }) =
    all { ret=PTErrorCode }
handleException all@(Def { cName="ppl_Polyhedron_is_empty" }) =
    all { ret=PTBool }
handleException all@(Def { cName="ppl_Polyhedron_is_universe" }) =
    all { ret=PTBool }
handleException all@(Def { cName="ppl_Polyhedron_is_bounded" }) =
    all { ret=PTBool }
handleException all@(Def { cName="ppl_Polyhedron_bounds_from_above" }) =
    all { ret=PTBool }
handleException all@(Def { cName="ppl_Polyhedron_bounds_from_below" }) =
    all { ret=PTBool }
handleException all@(Def { cName="ppl_Polyhedron_is_topologically_closed" }) =
    all { ret=PTBool }
handleException all@(Def { cName="ppl_Polyhedron_contains_Polyhedron" }) =
    all { ret=PTBool }
handleException
  all@(Def { cName="ppl_Polyhedron_strictly_contains_Polyhedron" }) =
    all { ret=PTBool }
handleException
  all@(Def { cName="ppl_Polyhedron_is_disjoint_from_Polyhedron" }) =
    all { ret=PTBool }
handleException all@(Def { cName="ppl_Polyhedron_equals_Polyhedron" }) =
    all { ret=PTBool }
handleException all@(Def { cName="ppl_Generator_type" }) =
    Ignore { cName="ppl_Generator_type" }
handleException all@(Def { cName="ppl_Constraint_type" }) =
    Ignore { cName="ppl_Constraint_type" }
handleException all@(Def { cName="ppl_Bounding_Box_has_lower_bound" })
    = all { ret=PTBool }
handleException all@(Def { cName="ppl_Bounding_Box_has_upper_bound" })
    = all { ret=PTBool }
handleException all@(Def { cName="ppl_Bounding_Box_is_empty" })
    = all { ret=PTBool }
handleException all@(Def { cName="ppl_Polyhedron_affine_dimension" })
    = Ignore { cName = "ppl_Polyhedron_affine_dimension" }
handleException other = other

-- The function with the following suffixes have the return type Bool,
-- Dimension, respectively.
funSuffixesBool = ["OK","_and_minimize","equal_test"]
funSuffixesDim  = ["space_dimension"]

getFunParams :: String -> String -> (Def, String)
getFunParams name rem = getFunParams' [] rem
  where
    getFunParams' params (')':xs) =
      (Def { cName = "ppl_"++name,
	     hName = underscoreToCase (removeConst name) False,
	     constr = "new" `isPrefixOf` name,
	     destr  = "delete" `isPrefixOf` name,
	     ret =
	       if any (\n -> reverse n `isPrefixOf` reverse name)
	          funSuffixesBool then PTBool else 
	       if any (\n -> reverse n `isPrefixOf` reverse name)
	          funSuffixesDim then PTDim else PTErrorCode,
	     args = params }, xs)
    getFunParams' params ('p':'p':'l':'_':'c':'o':'n':'s':'t':'_':xs) =
	getFunParams' params xs
    getFunParams' params ('e':'n':'u':'m':xs) =
	getFunParams' params (dropWhile isSpace xs)
    getFunParams' params ('p':'p':'l':'_':xs) =
	getFunParams' params xs
    getFunParams' params xs =
      if isJust mParaType then
	 getFunParams' (params++[(isIn, paraType)]) rem
      else
         (Ignore { cName=name }, xs)
      where
        (pName, rem1) = span (\x -> isAlpha x || x=='_') xs
        mParaType@(~(Just paraType)) = lookup pName typeNames
	rem2 = dropWhile isSpace rem1
	(star, rem3) = span (=='*') rem2
	rem4 = dropWhile isSpace rem3
	isIn = null star
	rem5 = dropWhile isAlphaNum rem4
	rem = dropWhile (\x -> isSpace x || x==',') rem5

removeConst :: String -> String
removeConst [] = []
removeConst ('c':'o':'n':'s':'t':'_':xs) = removeConst xs
removeConst (x:xs) = x:removeConst xs


underscoreToCase :: String -> Bool -> String
underscoreToCase lexeme upper = 
  adjustHead . concat . map adjustCase $ filter (not . null) . parts $ lexeme
  where
    parts s = let (l, s') = break (== '_') s
              in  
              l : case s' of
                    []      -> []
                    (_:s'') -> parts s''
    --    
    adjustCase (c:cs) = toUpper c : cs
    --
    adjustHead ""     = ""
    adjustHead (c:cs) = if upper then toUpper c : cs else toLower c:cs

typeNames = [
 ("dimension_type",PTDimension),
 ("Linear_Expression_t",PTLinearExpression),
 ("Coefficient_t",PTCoefficient),
 ("Constraint_t",PTConstraint),
 ("_Constraint_Type",PTConstraintType),
 ("Constraint_System_t",PTConstraintSystem),
 ("Constraint_System_const_iterator_t",PTConstraintSystemIterator),
 ("Generator_t",PTGenerator),
 ("_Generator_Type",PTGeneratorType),
 ("Generator_System_t",PTGeneratorSystem),
 ("Generator_System_const_iterator_t",PTGeneratorSystemIterator),
 ("Polyhedron_t",PTPolyhedron),
 ("unsigned",PTUnsigned),
 ("Bounding_Box_t", PTBoundingBox)]

haskellNames PTUnsigned = "Int"
haskellNames t = drop 2 (show t)

ctypeName PTConstraintSystem = "Constraint_System"
ctypeName PTGeneratorSystem  = "Generator_System"
ctypeName PTBoundingBox      = "Bounding_Box"
ctypeName t = drop 2 (show t)

{-
haskellNames PTDimension = "Dimension"
haskellNames PTLinearExpression = "LinearExpression"
haskellNames PTCoefficient = "Coefficient"
haskellNames PTConstraint = "Constraint"
haskellNames PTConstraintType = "ConstraintType"
haskellNames PTConstraintSystem = "ConstraintSystem"
haskellNames PTConstraintSystemIterator = "ConstraintSystemIterator"
haskellNames PTGenerator = "Generator"
haskellNames PTGeneratorType = "GeneratorType"
haskellNames PTGeneratorSystem = "GeneratorSystem"
haskellNames PTGeneratorSystemIterator = "GeneratorSystemIterator"
haskellNames PTPolyhedron = "Polyhedron"
haskellNames PTBoundingBox = "BoundingBox"
-}

ss = showString
sc = showChar
nl = sc '\n'

fics :: String -> String -> String -> ShowS
fics cN hN ty = ss "\nforeign import ccall unsafe \"".ss cN.sc '\"'.
		ss "\n  ".ss hN.ss " :: ".ss ty.
		nl

headDown :: String -> String
headDown (c:cs) = toLower c:cs
headDown cs = cs

gen :: [Def] -> ShowS
gen [] = id
gen (Def { destr=True, cName=cN, hName=hN, args=[(_, ty)] }:ds) =
    fics ('&':cN) hN
	     ("FinalizerPtr "++haskellNames ty).
    gen ds
gen (d@Def { destr=True }:ds) =
    error ("Invalid destructor: "++show d)
gen (Def { cName=cN, hName=hN, constr=noCopy, args=args, ret=retTy }:ds) =
    let 
      ret=maybe (Right retTy) Left (lookup False args)
      (Just idx)=elemIndex False (map fst args)
    in
    nl.ss hN.ss " :: ".
    genArgTypes args.
    nl.ss "  IO ".(case ret of
      (Left con) -> ss (haskellNames con)
      (Right PTErrorCode) -> ss "()"
      (Right PTInt) -> ss "Int"
      (Right PTDim) -> ss "Dimension"
      (Right PTBool) -> ss "Bool").
    nl.ss hN.genArgNames 0 args.ss " = do".
    genPreMarshal 0 args.
    (case ret of
      (Left _) -> nl.ss "  checkRetVal \"".ss hN.ss "\" $ "
      (Right PTBool) -> nl.ss "  liftM (toBool.fromIntegral) $ "
      (Right PTErrorCode) -> nl.ss "  checkRetVal \"".ss hN.ss "\" $ "
      (Right _) -> nl.ss "  liftM fromIntegral $ ").
    nl.ss "    ".ss (headDown cN).
      foldl (.) id
		(zipWith (.)
		 (repeat (ss " mArg")) 
		 (map (ss.show) [0..length args-1])).
    genPostMarshal idx ret noCopy.
    nl.
    fics cN (headDown cN)
      (foldl (\s t -> s++t++" -> ") "" (map makeCArg args) ++ "IO CInt").
    nl.
    gen ds
gen (Ignore { cName=cName }:ds) = handleIgnore cName.gen ds

genArgTypes :: [(Bool, PPLType)] -> ShowS
genArgTypes [] = id
genArgTypes ((True, ty):args) =
    nl.ss "  ".ss (haskellNames ty).ss " ->".
    genArgTypes args
genArgTypes ((False, ty):args) = genArgTypes args

genArgNames :: Int -> [(Bool, PPLType)] -> ShowS
genArgNames num [] = id
genArgNames num ((True, ty):args)
  | ty `elem` [PTDimension, PTUnsigned, PTConstraintType, PTGeneratorType] =
      ss " arg".ss (show num).genArgNames (num+1) args
  | otherwise =
      ss " (".ss (haskellNames ty).ss " arg".ss (show num).sc ')'.
      genArgNames (num+1) args
genArgNames num ((False, _):args) = genArgNames (num+1) args

genPreMarshal :: Int -> [(Bool, PPLType)] -> ShowS
genPreMarshal num [] = id
genPreMarshal num ((True,ty):args)
    | ty `elem` [PTDimension, PTUnsigned] =
	nl.ss "  let mArg".ss (show num).ss " = fromIntegral arg".
        ss (show num).genPreMarshal (num+1) args
    | ty `elem` [PTConstraintType, PTGeneratorType] =
	nl.ss "  let mArg".ss (show num).ss " = (fromIntegral . fromEnum) arg".
        ss (show num).genPreMarshal (num+1) args
    | otherwise =
	nl.ss "  withForeignPtr arg".ss (show num).ss " $ \\mArg".
	ss (show num).ss " -> do".genPreMarshal (num+1) args
genPreMarshal num ((False,ty):args) =
  nl.ss "  alloca $ \\mArg".ss (show num).ss " -> do".
  genPreMarshal (num+1) args

genPostMarshal :: Int -> Either PPLType PPLRetType -> Bool -> ShowS
genPostMarshal num (Left PTUnsigned) _ = 
    nl.ss "  liftM fromIntegral $ peek mArg".ss (show num)
genPostMarshal num (Left PTDimension) _ = 
    nl.ss "  liftM fromIntegral $ peek mArg".ss (show num)
genPostMarshal num (Left ty) noCopy =
    nl.ss "  res <- peek mArg".ss (show num).
    (if noCopy then nl else let tyName = ctypeName ty in
     nl.ss "  alloca $ \\copyPtr -> do".
     nl.ss "    ppl_new_".ss tyName.ss "_from_".ss tyName.ss " copyPtr res".
     nl.ss "    res <- peek copyPtr".
     nl.ss "  ").
    ss "  liftM ".ss (haskellNames ty).
    ss " $ newForeignPtr delete". ss (haskellNames ty). ss " res"
genPostMarshal num (Right _) _ = id
  
makeCArg :: (Bool, PPLType) -> String
makeCArg (False, ty) = "Ptr ("++makeCArg (True, ty)++")"
makeCArg (True, PTUnsigned) = "#{type unsigned}"
makeCArg (True, PTDimension) = "#{type ppl_dimension_type}"
makeCArg (True, PTConstraintType) = "#{type int}"
makeCArg (True, PTGeneratorType) = "#{type int}"
makeCArg (True, ty) = "Ptr "++haskellNames ty




-- These functions are all exceptions in the sense that they don't fit
-- into simple schema.
handleIgnore :: String -> ShowS
handleIgnore "ppl_Generator_type" =
  nl.ss "generatorType ::". 
  nl.ss "  Generator ->".
  nl.ss "  IO GeneratorType".
  nl.ss "generatorType (Generator arg0) = do".
  nl.ss "  withForeignPtr arg0 $ \\mArg0 -> do".
  nl.ss "  liftM (toEnum . fromIntegral) $ ".
  nl.ss "    ppl_Generator_type mArg0".
  nl.
  nl.ss "foreign import ccall unsafe \"ppl_Generator_type\"".
  nl.ss "  ppl_Generator_type :: Ptr Generator -> IO CInt"

handleIgnore "ppl_Constraint_type" =
  nl.ss "constraintType ::". 
  nl.ss "  Constraint ->".
  nl.ss "  IO ConstraintType".
  nl.ss "constraintType (Constraint arg0) = do".
  nl.ss "  withForeignPtr arg0 $ \\mArg0 -> do".
  nl.ss "  liftM (toEnum . fromIntegral) $ ".
  nl.ss "    ppl_Constraint_type mArg0".
  nl.
  nl.ss "foreign import ccall unsafe \"ppl_Constraint_type\"".
  nl.ss "  ppl_Constraint_type :: Ptr Constraint -> IO CInt"

handleIgnore "new_Coefficient_from_mpz_t" =
  nl.ss "newCoefficientFromInteger :: Integer -> IO Coefficient".
  nl.ss "newCoefficientFromInteger arg1 =".
  nl.ss "  alloca $ \\mArg0 -> withInteger arg1 $ \\mArg1 -> do".
  nl.ss "  checkRetVal \"newCoefficientFromInteger\" $ ".
  nl.ss "    ppl_new_Coefficient_from_mpz_t mArg0 mArg1".
  nl.ss "  res <- peek mArg0".
  nl.ss "  liftM Coefficient $ newForeignPtr deleteCoefficient res".
  nl.
  nl.ss "foreign import ccall unsafe \"ppl_new_Coefficient_from_mpz_t\"".
  nl.ss "  ppl_new_Coefficient_from_mpz_t :: Ptr (Ptr Coefficient) ->".
  nl.ss "                                    Ptr MpzStruct -> IO CInt"

handleIgnore "assign_Coefficient_from_mpz_t" =
  nl.ss "assignCoefficientFromInteger :: Coefficient -> Integer -> IO ()".
  nl.ss "assignCoefficientFromInteger (Coefficient arg0) arg1 =".
  nl.ss "  withForeignPtr arg0 $ \\mArg0 -> withInteger arg1 $ \\mArg1 ->".
  nl.ss "  checkRetVal \"assignCoefficientFromInteger\" $ ".
  nl.ss "    ppl_assign_Coefficient_from_mpz_t mArg0 mArg1".
  nl.
  nl.ss "foreign import ccall unsafe \"ppl_assign_Coefficient_from_mpz_t\"".
  nl.ss "  ppl_assign_Coefficient_from_mpz_t :: Ptr Coefficient ->".
  nl.ss "                                       Ptr MpzStruct -> IO CInt"

handleIgnore "Coefficient_to_mpz_t" =
  nl.ss "coefficientToInteger :: Coefficient -> IO Integer".
  nl.ss "coefficientToInteger (Coefficient arg0) =".
  nl.ss "  withForeignPtr arg0 $ \\mArg0 -> do".
  nl.ss "    (val, res) <- extractInteger (ppl_Coefficient_to_mpz_t mArg0)".
  nl.ss "    checkRetVal \"coefficientToInteger\" $ return res".
  nl.ss "    return val".
  nl.
  nl.ss "foreign import ccall unsafe \"ppl_Coefficient_to_mpz_t\"".
  nl.ss "  ppl_Coefficient_to_mpz_t :: Ptr Coefficient ->".
  nl.ss "                              Ptr MpzStruct -> IO CInt"

handleIgnore "Polyhedron_remove_dimensions" =
  nl.ss "polyhedronRemoveDimensions :: Polyhedron -> [Dimension] -> IO ()".
  nl.ss "polyhedronRemoveDimensions (Polyhedron arg0) [] = return ()".
  nl.ss "polyhedronRemoveDimensions (Polyhedron arg0) arg1 =".
  nl.ss "  withForeignPtr arg0 $ \\mArg0 -> let len = length arg1 in".
  nl.ss "  allocaArray len $ \\mArg1 -> do".
  nl.ss "    pokeArray mArg1 arg1".
  nl.ss "    checkRetVal \"polyhedronRemoveDimensions\" $".
  nl.ss "      ppl_Polyhedron_remove_dimensions mArg0 mArg1 (fromIntegral len)".
  nl.
  nl.ss "foreign import ccall unsafe \"ppl_Polyhedron_remove_dimensions\"".
  nl.ss "  ppl_Polyhedron_remove_dimensions :: Ptr Polyhedron ->".
  nl.ss "                                      Ptr Dimension ->".
  nl.ss "                                      #{type size_t} ->".
  nl.ss "                                      IO CInt"

handleIgnore "Polyhedron_map_dimensions" =
  nl.ss "polyhedronMapDimensions :: Polyhedron -> [Dimension] -> IO ()".
  nl.ss "polyhedronMapDimensions (Polyhedron arg0) [] = return ()".
  nl.ss "polyhedronMapDimensions (Polyhedron arg0) arg1 =".
  nl.ss "  withForeignPtr arg0 $ \\mArg0 ->".
  nl.ss "  withArray arg1 $ \\mArg1 -> do".
  nl.ss "    checkRetVal \"polyhedronMapDimensions\" $".
  nl.ss "      ppl_Polyhedron_map_dimensions mArg0 mArg1".
     ss          " (fromIntegral $ length arg1)".
  nl.
  nl.ss "foreign import ccall unsafe \"ppl_Polyhedron_map_dimensions\"".
  nl.ss "  ppl_Polyhedron_map_dimensions :: Ptr Polyhedron ->".
  nl.ss "                                   Ptr Dimension ->".
  nl.ss "                                   #{type size_t} ->".
  nl.ss "                                   IO CInt"


handleIgnore name =
  nl.ss "-- TODO: write binding for ".ss name

