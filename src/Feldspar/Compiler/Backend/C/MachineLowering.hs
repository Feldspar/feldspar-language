{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -Wall #-}

module Feldspar.Compiler.Backend.C.MachineLowering
  ( rename
  ) where

import qualified Data.Map as M

import Feldspar.Compiler.Imperative.Representation
import Feldspar.Compiler.Imperative.Frontend
import Feldspar.Compiler.Backend.C.Platforms (extend, c99, tic64x)
import Feldspar.Compiler.Backend.C.RuntimeLibrary
import Feldspar.Compiler.Options

-- This module does function renaming as well as copy expansion, in a single pass.
--
-- Missing from the old C99 rules: Constant folding of 0 - x. That really belongs
-- in the frontend but there is no negate in NUM and multiplying by -1 gives crazy
-- results due to overflow.

-- | External interface for renaming.
rename :: Options -> Bool -> Module () -> Module ()
rename opts _             | codeGenerator (platform opts) /= "c" = id
rename opts addRuntimeLib = rename' opts addRuntimeLib x
  where x = getPlatformRenames opts

-- | Internal interface for renaming.
rename' :: Options -> Bool -> M.Map String [(Which, Destination)] -> Module ()
        -> Module ()
rename' opts addRuntimeLib m (Module ents) = Module ents'
  where ents' = extra ++ map (renameEnt opts m) ents
        extra | addRuntimeLib = machineLibrary opts
              | otherwise     = []

-- | Rename entities.
renameEnt :: Options -> M.Map String [(Which, Destination)] -> Entity () -> Entity ()
renameEnt opts m p@Proc{..}
  | Just body <- procBody = p { procBody = Just $ renameBlock opts m body }
renameEnt _    _ e        = e

-- | Rename blocks.
renameBlock :: Options -> M.Map String [(Which, Destination)] -> Block () -> Block ()
renameBlock opts m (Block vs p) = Block (map (renameDecl m) vs) (renameProg opts m p)

-- | Rename declarations.
renameDecl :: M.Map String [(Which, Destination)] -> Declaration () -> Declaration ()
renameDecl m (Declaration v (Just e)) = Declaration v (Just $ renameExp m e)
renameDecl _ d                        = d

-- | Rename programs.
renameProg :: Options -> M.Map String [(Which, Destination)] -> Program ()
           -> Program ()
renameProg _    _ e@Empty              = e
renameProg _    _ c@Comment{}          = c
renameProg _    m (Assign lhs rhs)     = Assign (renameExp m lhs) (renameExp m rhs)
renameProg _    m (ProcedureCall n ps) = ProcedureCall n (map (renameParam m) ps)
renameProg opts m (Sequence ps)        = Sequence $ map (renameProg opts m) ps
renameProg opts m (Switch scrut alts)
   = Switch (renameExp m scrut) (map (renameAlt opts m) alts)
renameProg opts m (SeqLoop cond calc block)
  = SeqLoop (renameExp m cond) (renameBlock opts m calc) (renameBlock opts m block)
renameProg opts m (ParLoop p v e0 e1 e2 b)
  = ParLoop p v (renameExp m e0) (renameExp m e1) (renameExp m e2) (renameBlock opts m b)
renameProg opts m (BlockProgram b)     = BlockProgram $ renameBlock opts m b

-- | Rename expressions.
renameExp :: M.Map String [(Which, Destination)] -> Expression () -> Expression ()
renameExp _ v@VarExpr{}         = v
renameExp m (ArrayElem e es)    = ArrayElem (renameExp m e) $ map (renameExp m) es
renameExp m (StructField e s)   = StructField (renameExp m e) s
renameExp _ c@ConstExpr{}       = c
renameExp m (FunctionCall f es) = res
  where f'@(Function new t) = renameFun m (typeof $ head es) f
        es' = map (renameExp m) es
        res | new /= "div"      = FunctionCall f' es'
            | [arg1,arg2] <- es
            , (_ :# NumType Signed _) <- t
            = StructField (fun div_t (div_f t) [arg1, arg2]) "quot"
            | otherwise = fun t "/" es'
          where
           div_t = StructType "div_t" [("quot", t), ("rem", t)]
           div_f (1 :# (NumType Signed S8))  = "div"
           div_f (1 :# (NumType Signed S16)) = "div"
           div_f (1 :# (NumType Signed S32)) = "div"
           div_f (1 :# (NumType Signed S40)) = "ldiv"
           div_f (1 :# (NumType Signed S64)) = "lldiv"
           div_f typ = error $ "div not defined for " ++ show typ

renameExp m (Cast t e)          = Cast t $ renameExp m e
renameExp m (AddrOf e)          = AddrOf $ renameExp m e
renameExp _ s@SizeOf{}          = s
renameExp m (Deref e)           = Deref $ renameExp m e

-- | Rename parameters.
renameParam :: M.Map String [(Which, Destination)] -> ActualParameter ()
            -> ActualParameter ()
renameParam m (ValueParameter e) = ValueParameter $ renameExp m e
renameParam _ p                  = p

-- | Rename switch alternatives.
renameAlt :: Options -> M.Map String [(Which, Destination)]
          -> (Pattern (), Block ()) -> (Pattern (), Block ())
renameAlt opts m (p, b) = (p, renameBlock opts m b)

-- | Renames functions that should be renamed. Identity function on others.
renameFun :: M.Map String [(Which, Destination)] -> Type -> Function -> Function
renameFun m argtype f@(Function name t)
  | Just ps <- M.lookup name m
  , Just s <- findFun name argtype ps t = Function s t
  | otherwise                           = f

-- | Finds the new name of the function, if any.
findFun :: String -> Type -> [(Which, Destination)] -> Type -> Maybe String
findFun name argtype m tp = go m
  where go []                          = Nothing
        go ((Only p, s):_) | true p tp = Just (newName name argtype tp s)
        go ((All, s):_)                = Just (newName name argtype tp s)
        go (_:t)                       = go t

-- | Returns a new name according to specification.
newName :: String -> Type -> Type -> Destination -> String
newName _    _       _  (Name s)                   = s
newName name _       tp (Extend FunType p)         = extend p name tp
newName name argtype _  (Extend ArgType p)         = extend p name argtype
newName _    _       tp (ExtendRename FunType p s) = extend p s tp
newName _    argtype _  (ExtendRename ArgType p s) = extend p s argtype

-- | Tells whether a predicate holds for a type.
true :: Predicate -> Type -> Bool
true Complex    t             = isComplex t
true Float      t             = isFloat t
true Signed32   t
  | Just 32 <- intWidth t
  , Just True <- intSigned t  = True
  | otherwise                 = False
true Unsigned32 t
  | Just 32 <- intWidth t
  , Just False <- intSigned t = True
  | otherwise                 = False

-- A rename is the name of the function to be renamed coupled with a
-- list of preconditions for renaming to happen and a the destination
-- name if the precondition is held. First match is executed and the
-- destination name becomes whatever the template specifies.

-- | C99 renaming list.
c99list :: [Rename]
c99list =
  [ ("/=",            [ (All, Name "!=")])
  , ("not",           [ (All, Name "!")])
  , ("quot",          [ (All, Name "/")])
  , ("rem",           [ (All, Name "%")])
  , (".&.",           [ (All, Name "&")])
  , (".|.",           [ (All, Name "|")])
  , ("xor",           [ (All, Name "^")])
  , ("complement",    [ (All, Name "~")])
  , ("shiftL",        [ (All, Name "<<")])
  , ("shiftLU",       [ (All, Name "<<")])
  , ("shiftR",        [ (All, Name ">>")])
  , ("shiftRU",       [ (All, Name ">>")])
  , ("creal",         [ (All, Name "crealf")])
  , ("cimag",         [ (All, Name "cimagf")])
  , ("conjugate",     [ (All, Name "conjf")])
  , ("magnitude",     [ (All, Name "cabsf")])
  , ("phase",         [ (All, Name "cargf")])
  , ("atan2",         [ (Only Complex, Name "atan2f") ])
  ] ++
  map mkC99TrigRule ["exp", "sqrt", "log", "**", "sin", "tan", "cos", "asin"
                    , "atan", "acos", "sinh", "tanh", "cosh", "asinh", "atanh"
                    , "acosh"] ++
  -- Extend these functions based on the function type.
  map (mkC99ExtendRule FunType) [ "abs", "signum", "logBase", "setBit", "clearBit"
                                , "complementBit", "rotateL", "rotateR"
                                , "reverseBits" ] ++
  -- Extend these functions based on the argument type.
  map (mkC99ExtendRule ArgType) [ "testBit", "bitScan", "bitCount", "complex"
                                 , "mkPolar", "cis"]

-- | Make C99 extend rule.
mkC99ExtendRule :: WhichType -> String -> Rename
mkC99ExtendRule t s = (s, [ (All, Extend t c99) ])

-- | Make C99 trig rule.
mkC99TrigRule :: String -> Rename
mkC99TrigRule s = (s, [ (Only Complex, Name ('c':s')), (All, Name s') ])
  where s' = s ++ "f"

-- | Tic64x renaming list.
tic64xlist :: [Rename]
tic64xlist =
  [ ("==",          [ (Only Complex, ExtendRename ArgType tic64x "equal") ])
  , ("abs",         [ (Only Float, Name "_fabs"), (Only Signed32, Name "_abs") ])
  , ("+",           [ (Only Complex, ExtendRename ArgType tic64x "add") ])
  , ("-",           [ (Only Complex, ExtendRename ArgType tic64x "sub") ])
  , ("*",           [ (Only Complex, ExtendRename ArgType tic64x "mult") ])
  , ("/",           [ (Only Complex, ExtendRename ArgType tic64x "div") ])
  ] ++
  map mkTic64xComplexRule ["exp", "sqrt", "log", "sin", "tan", "cos", "asin"
                          ,"atan", "acos", "sinh", "tanh", "cosh", "asinh"
                          ,"atanh","acosh","creal","cimag", "conjugate"
                          ,"magnitude","phase", "logBase"] ++
  [ ("**",          [ (Only Complex, ExtendRename ArgType tic64x "cpow") ])
  , ("rotateL",     [ (Only Unsigned32, Name "_rotl") ])
  , ("reverseBits", [ (Only Unsigned32, Name "_bitr") ])
  ]

-- | Create Tic64x rule for complex type.
mkTic64xComplexRule :: String -> Rename
mkTic64xComplexRule s = (s, [ (Only Complex, Extend ArgType tic64x) ] )

-- | Returns the platform renames based on the platform name.
getPlatformRenames :: Options -> M.Map String [(Which, Destination)]
getPlatformRenames opt =
  case name $ platform opt of
    "tic64x"                                     -> M.fromList (tic64xlist ++ c99list)
    s | s `elem` ["c99", "c99OpenMp", "c99Wool"] -> M.fromList c99list
      | otherwise                                -> M.fromList []
