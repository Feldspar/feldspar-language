--
-- Copyright (c) 2009-2011, ERICSSON AB
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright notice,
--       this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in the
--       documentation and/or other materials provided with the distribution.
--     * Neither the name of the ERICSSON AB nor the names of its contributors
--       may be used to endorse or promote products derived from this software
--       without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}
-- Some questionable partial patterns and pattern matches on strings
-- in the decoding function.
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Feldspar.Compiler.Imperative.Frontend where

import Feldspar.Compiler.Backend.C.Options
import Feldspar.Compiler.Imperative.Representation
import Feldspar.Core.Types (Length)
import Feldspar.Core.UntypedRepresentation (Fork(..))
import Feldspar.Range

import Data.Char (toLower)
import Data.List (stripPrefix)

toBlock :: Program () -> Block ()
toBlock (BlockProgram b) = b
toBlock p                = Block [] p

toProg :: Block () -> Program ()
toProg (Block [] p) = p
toProg e = BlockProgram e

-- | Where to place the program result
type Location = Maybe (Expression ())

-- | Copies expressions into a destination. If the destination is
-- an array the arguments are appended into the destination.
copyProg :: Location -> [Expression ()] -> Program ()
copyProg _ []      = error "copyProg: missing source parameter."
copyProg Nothing _ = Empty
copyProg (Just outExp) inExp =
  case inExp of
    [x] | outExp == x -> Empty
    _                 -> deepCopy outExp inExp

-- | Expand copying of aggregate data to copying of components
deepCopy :: Expression () -> [Expression ()] -> Program ()
deepCopy arg1 [arg2]
  | arg1 == arg2
  = Empty

deepCopy arg1 args
  | isAwLType (typeof arg1) || isNativeArray (typeof arg1)
  = Assign arg1 $ fun (typeof arg1) "copy" (arg1 : args)

deepCopy arg1 [arg2]
  | StructType _ fts <- typeof arg2
  = Sequence $ map (deepCopyField . fst) fts

  | otherwise
  = Assign arg1 arg2
    where deepCopyField fld = deepCopy (StructField arg1 fld) [StructField arg2 fld]

deepCopy _ _ = error "Multiple non-array arguments to copy"

flattenCopy :: Expression () -> [Expression ()] -> [Expression ()] ->
               Expression () -> [Program ()]
flattenCopy _ [] [] _ = []
flattenCopy dst (t:ts) (l:ls) cLen
  = Assign dst (arrayFun "copyArrayPos" $ arrayBufLen dst ++ arrayBufLen t ++ [cLen])
    : flattenCopy dst ts ls (ePlus cLen l)

ePlus :: Expression () -> Expression () -> Expression ()
ePlus (ConstExpr (IntConst 0 _)) e = e
ePlus e (ConstExpr (IntConst 0 _)) = e
ePlus e1 e2                        = binop (1 :# NumType Signed S32) "+" e1 e2

-- | Lower array copy
lowerCopy :: Options -> Type -> Expression () -> [Expression ()] -> [Program ()]
lowerCopy opts t dst ins
  | isAwLType t = lowerArrayCopy opts t dst ins
lowerCopy opts NativeArray{} dst [arg1,arg2]
  | l@(ConstExpr (IntConst n _)) <- arrayLength arg2
  = if n < safetyLimit opts
      then initArray (Just dst) l:map (\i -> Assign (ArrayElem dst [litI32 i]) (ArrayElem arg2 [litI32 i])) [0..(n-1)]
      else error $ unlines ["Frontend.lowerCopy: array size (" ++ show n ++ ") too large", show arg1, show arg2]
lowerCopy _ t e es = error $ "Frontend.lowerCopy: funny type (" ++ show t ++ ") or destination\n"
                                ++ show e ++ "\nor arguments\n"
                                ++ unlines (map show es)

-- | Lower general array copy
lowerArrayCopy :: Options -> Type -> Expression () -> [Expression ()] -> [Program ()]
lowerArrayCopy _ _ dst ins'@(arg1:in1:ins) -- FIXME: Remove the unused Options argument.
  | [ConstExpr ArrayConst{..}] <- ins'
  = initArray (Just dst) (litI32 $ toInteger $ length arrayValues)
    : zipWith (\i c -> Assign (ArrayElem dst [litI32 i]) (ConstExpr c)) [0..] arrayValues
  | [] <- ins
  = [ Assign (arrayBuffer   dst) (arrayFun "initCopyArray" $ concatMap arrayBufLen [arg1, in1])
    , Assign (arrayLengthLV dst) (arrayLength in1)
    ]
  | otherwise
  = [ initArray (Just dst) expDstLen, copyFirstSegment ] ++
      flattenCopy arg1 ins argnLens arg1len
    where expDstLen = foldr ePlus (litI32 0) aLens
          copyFirstSegment
            | dst == in1 = Empty
            | otherwise
            = Assign (arrayBuffer dst)
                     (arrayFun "copyArray" $ concatMap arrayBufLen [arg1, in1])
          aLens@(arg1len:argnLens) = map arrayLength (in1:ins)


-- | Initialize an array using \"initArray\"
initArray
    :: Location       -- ^ Array location
    -> Expression ()  -- ^ Array length
    -> Program ()
initArray Nothing _ = Empty
initArray (Just arr) len
  | isNativeArray $ typeof arr
  = Empty
  | otherwise
  = Sequence [ Assign (arrayBuffer arr) $ arrayFun "initArray" $ arrayBufLen arr ++ [len]
             , Assign (arrayLengthLV arr) len
             ]

-- | Generate a call to free an array represented as a variable
freeArray :: Variable () -> Program ()
freeArray = freeArrayE . VarExpr

-- | Generate a call to free an array represented as an expression
freeArrayE :: Expression () -> Program ()
freeArrayE arr
  = call (variant "freeArray" t) $ map ValueParameter $ take n $ arrayBufLen arr
   where StructType _ [("buffer", ArrayType _ t), _] = typeof arr
         n = if isShallow t then 1 else 2

-- | Generate 'freeArray' calls for all arrays in a list of declarations
freeArrays :: [Declaration ()] -> [Program ()]
freeArrays defs = map freeArrayE arrays
  where
    arrays = [f $ varToExpr v | v <- map declVar defs,
                                (f, t) <- flattenStructs $ typeof v,
                                isAwLType t]

-- | Get the length of an array
arrayLength :: Expression () -> Expression ()
arrayLength arr
  | Just l <- staticArrayLength arr = litI32 $ fromIntegral l
  | otherwise = StructField arr "length"

-- | Get the length of an array as an lval
arrayLengthLV :: Expression () -> Expression ()
arrayLengthLV arr = StructField arr "length"

arrayBuffer :: Expression () -> Expression ()
arrayBuffer arr = StructField arr "buffer"

arrayBufLen :: Expression () -> [Expression ()]
arrayBufLen arr = [arrayBuffer arr, arrayLength arr]

-- | If possible, return the static length of an array
staticArrayLength :: Expression t -> Maybe Length
staticArrayLength = go []  -- TODO: Extend to handle x.member1.member2
  where go :: [String] -> Expression t -> Maybe Length
        go []    (ConstExpr (ArrayConst l _)) = Just (fromIntegral $ length l)
        go []    (VarExpr (Variable (ArrayType r _) _)) | isSingleton r = Just (upperBound r)
        go []    (VarExpr (Variable (NativeArray (Just l) _) _)) = Just l
        go []    (Deref e) = go [] e -- TODO: this is questionable; we now look at an expression for the address of the array
        go ss    (StructField e s) = go (s:ss) e
        go ss    (AddrOf e) = go ss e
        go (s:_) (VarExpr (Variable (StructType _ fields) _))
          | Just (ArrayType r _) <- lookup s fields
          , isSingleton r = Just (upperBound r)
          | Just (NativeArray (Just l) _) <- lookup s fields
          = Just l
        go _ _ = Nothing

iVarInitCond :: Fork -> Expression () -> Program ()
iVarInitCond Future var = iVarInit var
iVarInitCond _      _   = Empty

iVarInit :: Expression () -> Program ()
iVarInit var = call "ivar_init" [ValueParameter var]

iVarGet :: Bool -> Expression () -> Expression () -> Program ()
iVarGet inTask loc ivar
    | Just eTy <- elemTyAwL typ
    = if isShallow eTy
          then call (mangle inTask "ivar_get_array_shallow") $ map ValueParameter [ AddrOf loc, ivar, size eTy ]
          else call (mangle inTask "ivar_get_array") $ map ValueParameter [ AddrOf loc, ivar, copyFun eTy ]
    | otherwise  = call (mangle inTask "ivar_get") [ TypeParameter typ
                                                   , ValueParameter (AddrOf loc)
                                                   , ValueParameter ivar]
      where
        typ = typeof loc
        mangle True  s = s
        mangle False s = s ++ "_nontask"
        size t = SizeOf t
        copyFun t = VarExpr $ Variable (1 :# Pointer VoidType) (variant "initCopyArray" t)

iVarPut :: Expression () -> Expression () -> Program ()
iVarPut ivar msg
    | Just eTy <- elemTyAwL typ
    = if isShallow eTy
          then call "ivar_put_array_shallow" [ValueParameter ivar, ValueParameter $ AddrOf msg, sizePar eTy]
          else call "ivar_put_array" [ValueParameter ivar, ValueParameter $ AddrOf msg, copyPar eTy]
    | otherwise              = call "ivar_put" [TypeParameter typ, ValueParameter ivar, ValueParameter (AddrOf msg)]
      where
        typ = typeof msg
        copyPar t = ValueParameter $ VarExpr $ Variable (1 :# Pointer VoidType) (variant "initCopyArray" t)
        sizePar t = ValueParameter $ SizeOf t

-- | Generate a call to free an IVar represented as a variable
iVarDestroy :: Variable () -> Program ()
iVarDestroy v = call "ivar_destroy" [ValueParameter $ AddrOf $ varToExpr v]

-- | Generate 'iVarDestroy' calls for all IVars in a list of declarations
freeIVars :: [Declaration ()] -> [Program ()]
freeIVars defs = map iVarDestroy ivars
  where
    ivars = filter (isIVar . typeof) $ map declVar defs

spawn :: Fork -> String -> [Variable ()] -> Program ()
spawn f taskName vs
 | f `elem` [None, Loop] = call taskName $ map mkV vs
 | otherwise = call spawnName allParams
  where
    mkV v = ValueParameter . varToExpr $ Variable (typeof v) (varName v)
    spawnName = "spawn" ++ show (length vs)
    taskParam = FunParameter taskName
    typeParams = map (TypeParameter . typeof) vs
    varParams = map (ValueParameter . mkv) vs
      where mkv v = VarExpr $ Variable (typeof v) (varName v)
    allParams = taskParam : concat (zipWith (\a b -> [a,b]) typeParams varParams)

run :: String -> [Variable ()] -> Program ()
run taskName vs = call runName allParams
  where
    runName = "run" ++ show (length vs)
    typeParams = map (TypeParameter . typeof) vs
    taskParam = FunParameter taskName
    allParams = taskParam : typeParams

intWidth :: Type -> Maybe Integer
intWidth (1 :# (NumType _ S8))             = Just 8
intWidth (1 :# (NumType _ S16))            = Just 16
intWidth (1 :# (NumType _ S32))            = Just 32
intWidth (1 :# (NumType _ S40))            = Just 40
intWidth (1 :# (NumType _ S64))            = Just 64
intWidth _                                 = Nothing

intSigned :: Type -> Maybe Bool
intSigned (1 :# (NumType Unsigned _))            = Just False
intSigned (1 :# (NumType Signed _))              = Just True
intSigned _                                      = Nothing

litF :: Float -> Expression t
litF n = ConstExpr (FloatConst n)

litD :: Double -> Expression t
litD n = ConstExpr (DoubleConst n)

litB :: Bool -> Expression ()
litB b = ConstExpr (BoolConst b)

litC :: Constant () -> Constant () -> Expression ()
litC r i = ConstExpr (ComplexConst r i)

litI :: Type -> Integer -> Expression ()
litI (_ :# t) n = ConstExpr (IntConst n t)

litI32 :: Integer -> Expression ()
litI32 = litI (1 :# NumType Unsigned S32)

int32, uint32 :: Type
int32 = 1 :# NumType Signed S32
uint32 = 1 :# NumType Unsigned S32

isComplex :: Type -> Bool
isComplex (_ :# ComplexType{})            = True
isComplex _                               = False

isFloat :: Type -> Bool
isFloat (_ :# FloatType{})            = True
isFloat _                             = False

isAwLType :: Type -> Bool
isAwLType (StructType _ [("buffer", ArrayType{}), ("length",_)])
            = True
isAwLType _ = False

elemTyAwL :: Type -> Maybe Type
elemTyAwL (StructType _ [("buffer", ArrayType _ t), ("length",_)]) = Just t
elemTyAwL _ = Nothing

isArray :: Type -> Bool
isArray ArrayType{}                   = True
isArray _                             = False

isNativeArray :: Type -> Bool
isNativeArray NativeArray{}                   = True
isNativeArray _                               = False

isIVar :: Type -> Bool
isIVar IVarType{} = True
isIVar _              = False

isPointer :: Type -> Bool
isPointer (_ :# Pointer{})            = True
isPointer _                           = False

isVarExpr :: Expression () -> Bool
isVarExpr VarExpr{} = True
isVarExpr _         = False

isComposite :: Type -> Bool
isComposite ArrayType{}   = True
isComposite NativeArray{} = True
isComposite StructType{}  = True
isComposite _             = False

isShallow :: Type -> Bool
isShallow ArrayType{} = False
isShallow NativeArray{} = False
isShallow (StructType _ fs) = all (isShallow . snd) fs
isShallow _ = True

variant :: String -> Type -> String
variant str t | isShallow t = str
              | otherwise = str ++ "_" ++ encodeType t

arrayFun :: String -> [Expression ()] -> Expression ()
arrayFun name (dst : dLen : es)
  = fun arrTy (variant name eTy) (dst : dLen : addSize es eTy)
   where arrTy = typeof dst
         eTy   = elemType arrTy
         elemType (ArrayType _ t) = t
         elemType (StructType _ [("buffer", ArrayType _ t), _]) = t
         elemType t = error $ "Frontend.elemType: not an array " ++ show t
         addSize es' t | isShallow t = SizeOf t:es'
                       | otherwise   = es'

-- | The type of an array paired with its length
mkAwLType :: Range Length -> Type -> Type
mkAwLType r t = StructType n [("buffer", ArrayType r t), ("length", uint32)]
  where n = "awl_" ++ encodeType t

-- | Does the parameters allow a fast/cheap (in register) return.
canFastReturn :: Type -> Bool
canFastReturn t
  | not $ isComposite t
  , not $ isPointer t -- Conservative, pointers are handled the regular way.
  , not $ isIVar t      = True
canFastReturn _         = False

containsNativeArray :: Type -> Bool
containsNativeArray t = any (isNativeArray . snd) $ flattenStructs t

-- | Returns a list of access functions and types for the leaves of the struct tree of the type
flattenStructs :: Type -> [(Expression () -> Expression (), Type)]
flattenStructs t'@(StructType _ fts)
  | not $ isAwLType t' = [(\e -> af $ StructField e fname, t'')
                          | (fname, t) <- fts, (af, t'') <- flattenStructs t]
flattenStructs t' = [(id, t')]

hasReference :: Type -> Bool
hasReference ArrayType{}                 = True
hasReference NativeArray{}               = True -- TODO: Reconsider this safe approximation if we start using native arrays for performance
hasReference (_ :# Pointer{})            = True
hasReference IVarType{}                  = True
hasReference (StructType _ fs)           = any (hasReference . snd) fs
hasReference _                           = False

varToExpr :: Variable t -> Expression t
varToExpr = VarExpr

exprToVar :: Expression () -> Variable ()
exprToVar (VarExpr v) = v
exprToVar (Deref e)   = exprToVar e
exprToVar e           = error $ "Frontend.exprToVar: Unexpected variable:" ++ show e

binop :: Type -> String -> Expression () -> Expression () -> Expression ()
binop t n e1 e2 = fun t n [e1, e2]

fun :: Type -> String -> [Expression ()] -> Expression ()
fun t n = FunctionCall (Function n t)

mkIf :: Expression () -> Block () -> Maybe (Block ()) -> Program ()
mkIf ce tb Nothing   = Switch ce [(Pat (litB True), tb)]
mkIf ce tb (Just eb) = Switch ce [(Pat (litB True), tb), (Pat (litB False), eb)]

call :: String -> [ActualParameter ()] -> Program ()
call = ProcedureCall

for :: ParType -> Variable () -> Expression () -> Expression () -> Expression () -> Block () -> Program ()
for _ _ _ _ _ (Block [] (Sequence ps)) | all (== Empty) ps = Empty
for p n s e i b = ParLoop p n s e i b

while :: Block () -> Expression () -> Block () -> Program ()
while p e = SeqLoop e p

-- | Wrap a list of statements in Sequence unless there is only one statement in the list
mkSequence :: [Program ()] -> Program ()
mkSequence [p] = p
mkSequence ps  = Sequence ps

{-
Encoded format is:

<type tag>_<inner_type_tag(s)>

Where the type tag is some unique prefix except for the scalar
types. The tag for StructTypes include the number of elements in the
struct to simplify the job for decodeType.

-}

encodeType :: Type -> String
encodeType = go
  where
    go VoidType              = "void"
    -- Machine vectors do not change memory layout, so keep internal.
    go (_ :# t)              = goScalar t
    go (IVarType t)          = "i_" ++ go t
    go (NativeArray _ t)     = "narr_" ++ go t
    go (StructType n _)      = n
    go (ArrayType _ t)       = "arr_" ++ go t
    goScalar BoolType        = "bool"
    goScalar BitType         = "bit"
    goScalar FloatType       = "float"
    goScalar DoubleType      = "double"
    goScalar (NumType s w)   = map toLower (show s) ++ show w
    goScalar (ComplexType t) = "complex_" ++ go t
    goScalar (Pointer t)     = "ptr_" ++ go t

-- Almost the inverse of encodeType. Some type encodings are lossy so
-- they are impossible to recover.
decodeType :: String -> [Type]
decodeType = goL []
  where
    goL acc [] = reverse acc
    goL acc s  = goL (out:acc) rest'
       where (out, rest) = go s
             rest' = case rest of
                      '_':t -> t
                      _     -> rest

    go (stripPrefix "void"     -> Just t) = (VoidType, t)
    go (stripPrefix "bool"     -> Just t) = (1 :# BoolType, t)
    go (stripPrefix "bit"      -> Just t) = (1 :# BitType, t)
    go (stripPrefix "float"    -> Just t) = (1 :# FloatType, t)
    go (stripPrefix "double"   -> Just t) = (1 :# DoubleType, t)
    go (stripPrefix "unsigned" -> Just t) = (1 :# NumType Unsigned w, t')
     where (w, t') = decodeSize t
    go (stripPrefix "signed"   -> Just t) = (1 :# NumType Signed w, t')
     where (w, t') = decodeSize t
    go (stripPrefix "complex"  -> Just t) = (1 :# ComplexType tn, t')
     where (tn, t') = go t
    go (stripPrefix "ptr_"     -> Just t) = (1 :# Pointer tt, t')
     where (tt, t') = go t
    go (stripPrefix "i_"       -> Just t) = (IVarType tt, t')
     where (tt, t') = go t
    go (stripPrefix "narr_"    -> Just t) = (NativeArray Nothing tt, t')
     where (tt, t') = go t
    go h@('s':'_':t) = (StructType h' $ zipWith mkMember [1 :: Int ..] ts, t'')
       where mkMember n' tmem = ("member" ++ show n', tmem)
             Just (n, t') = decodeLen t
             (ts, t'') = structGo n t' []
             structGo 0 s acc = (reverse acc, s)
             structGo n' ('_':s) acc = structGo (n' - 1) s' (ts':acc)
                      where (ts', s') = go s
             h' = take (length h - length t'') h
    go (stripPrefix "arr_"     -> Just t) = (ArrayType fullRange tt, t')
      where (tt, t') = go t
    go (stripPrefix "awl_"     -> Just t) = (mkAwLType fullRange tt, t')
      where (tt, t') = go t
    go s = error ("decodeType: " ++ s)

    decodeSize (stripPrefix "S32" -> Just t) = (S32, t)
    decodeSize (stripPrefix "S8"  -> Just t) = (S8, t)
    decodeSize (stripPrefix "S16" -> Just t) = (S16, t)
    decodeSize (stripPrefix "S40" -> Just t) = (S40, t)
    decodeSize (stripPrefix "S64" -> Just t) = (S64, t)

    decodeLen e
      | [p@(_,_)] <- reads e :: [(Int,String)]
      = Just p
      | otherwise = Nothing
