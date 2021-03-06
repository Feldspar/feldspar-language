--
-- Copyright (c) 2019, ERICSSON AB
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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -Wall #-}

{- | The ValueInfo module defines the types and basic operations for value set
     analysis. In this implementation, a value set is either

     * an inclusive range [lo, hi] wrapped in a constructor giving the type of
       elements in the range, or
     * a product (v1, ..., vk) of values denoting the cartesian product of the vi.
-}

module Feldspar.Core.ValueInfo
  ( ValueInfo(..)
  , ValueKind(..)
  , aLit
  , elementsVI
  , setLB
  , addVI
  , mulVI
  , fromSingletonVI
  -- Unused
  , botInfo
  , botInfoST
  , topInfo
  , topInfoST
  ) where

import Feldspar.Compiler.Options (Pretty(..))
import Feldspar.Core.Types (WordN)
import Feldspar.Core.UntypedRepresentation
import Feldspar.Lattice (Lattice, empty, universal)
import Feldspar.Range

import Data.Int
import Data.List (intercalate)
import Data.Word

-- | The ValueInfo type wraps ranges of various types as well as cartesian
--   products into a universal domain.
--   The value info of a function is the value info of its return value.
data ValueInfo = VIBool   (Range Bool)
               | VIInt8   (Range Int8)
               | VIInt16  (Range Int16)
               | VIInt32  (Range Int32)
               | VIInt64  (Range Int64)
               | VIWord8  (Range Word8)
               | VIWord16 (Range Word16)
               | VIWord32 (Range Word32)
               | VIWord64 (Range Word64)
               | VIWordN  (Range WordN)
               | VIFloat  -- (Range Float)
               | VIDouble -- (Range Double)
               | VIProd ValueKind [ValueInfo] -- ^ @ValueKind@ informs pretty-printing
               deriving (Eq)

-- | The kind of product
data ValueKind = ProdKind -- ^ Used for everything except tuples and arrays
               | RangeKind -- ^ Used for dimension info on arrays
               | TupleKind -- ^ Used for tuples
  deriving Eq

instance Show ValueInfo where
  show (VIBool r)   = show r
  show (VIInt8 r)   = show r
  show (VIInt16 r)  = show r
  show (VIInt32 r)  = show r
  show (VIInt64 r)  = show r
  show (VIWord8 r)  = show r
  show (VIWord16 r) = show r
  show (VIWord32 r) = show r
  show (VIWord64 r) = show r
  show (VIWordN r)  = show r
  show VIFloat{}    = "[*,*]"
  show VIDouble{}   = "[*,*]"
  show (VIProd k vs)
    | ProdKind <- k
    = "VIProd " ++ show vs
    | TupleKind <- k
    = '(':intercalate ", " (map show vs) ++ ")"
    | RangeKind <- k
    = case vs of
        [] -> error "Faulty ValueInfo range information."
        h:t -> show h ++ " :> " ++ show t

instance Pretty ValueInfo where
  pretty = show

-- | Annotate a literal with value info.
aLit :: Lit -> UntypedFeld ValueInfo
aLit l = In (literalVI l) (Literal l)

-- | Construct an Elements value info from those of the index and value parts
elementsVI :: ValueInfo -> ValueInfo -> ValueInfo
elementsVI idx val = VIProd RangeKind [idx, val]

-- | Forcing a range to an integer type given by a signedness and a size.
constantIntRange :: Signedness -> Size
                 -> (forall a . Lattice (Range a) => Range a) -> ValueInfo
constantIntRange Signed   S8   r = VIInt8 r
constantIntRange Signed   S16  r = VIInt16 r
constantIntRange Signed   S32  r = VIInt32 r
constantIntRange Signed   S64  r = VIInt64 r
constantIntRange Unsigned S8   r = VIWord8 r
constantIntRange Unsigned S16  r = VIWord16 r
constantIntRange Unsigned S32  r = VIWord32 r
constantIntRange Unsigned S64  r = VIWord64 r
constantIntRange _        S40  _
  = error "ValueInfo.constantIntRange: S40 not supported"
constantIntRange _        S128 _
  = error "ValueInfo.constantIntRange: S128 not supported"

-- | Make value info from a literal
literalVI :: Lit -> ValueInfo
literalVI (LBool b) = singletonVI b
literalVI (LInt sgn sz n) = go sgn sz
  where go Signed     S8 = singletonVI (fromInteger n :: Int8)
        go Signed    S16 = singletonVI (fromInteger n :: Int16)
        go Signed    S32 = singletonVI (fromInteger n :: Int32)
        go Signed    S40 = singletonVI (fromInteger n :: Int64)
        go Signed    S64 = singletonVI (fromInteger n :: Int64)
        go Signed   S128 = error "literalVI: S128 not supported"
        go Unsigned   S8 = singletonVI (fromInteger n :: Word8)
        go Unsigned  S16 = singletonVI (fromInteger n :: Word16)
        go Unsigned  S32 = singletonVI (fromInteger n :: Word32)
        go Unsigned  S40 = singletonVI (fromInteger n :: Word64)
        go Unsigned  S64 = singletonVI (fromInteger n :: Word64)
        go Unsigned S128 = error "literalVI: S128 not supported"
literalVI (LFloat  x) = singletonVI x
literalVI (LDouble x) = singletonVI x
literalVI (LComplex re im) = VIProd ProdKind [literalVI re, literalVI im]
literalVI LString{} = error "literalVI: LString not supported"
literalVI (LArray t xs) = foldr (lubVI . literalVI) (botInfo t) xs
literalVI (LTup xs) = VIProd TupleKind $ map literalVI xs

-- | The bottom (most informative) elements of the info domains for each scalar type.
botInfoST :: ScalarType -> ValueInfo
botInfoST BoolType         = VIBool $ Range True False
botInfoST (IntType sgn sz) = constantIntRange sgn sz empty
botInfoST FloatType        = VIFloat
botInfoST DoubleType       = VIDouble
botInfoST (ComplexType t)  = VIProd ProdKind [botInfo t, botInfo t]

-- | The bottom (most informative) elements of the info domains for each type.
botInfo :: Type -> ValueInfo
-- FIXME: Should (n :# t) be VIProd (replicate n botInfoST t)?
botInfo (_ :# t)         = botInfoST t
botInfo StringType       = error "botInfo: StringType not supported"
botInfo (TupType ts)     = VIProd TupleKind $ map botInfo ts
botInfo (MutType t)      = botInfo t
botInfo (RefType t)      = botInfo t
botInfo (ArrayType r t)  = VIProd RangeKind [VIWordN r, botInfo t]
botInfo (MArrType r t)   = VIProd RangeKind [VIWordN r, botInfo t]
botInfo (ParType t)      = botInfo t -- Provisionally
botInfo (ElementsType t) = VIProd RangeKind [botInfo indexType, botInfo t]
botInfo (IVarType t)     = botInfo t
botInfo (FunType _ t)    = botInfo t
botInfo (FValType t)     = botInfo t

-- | The top (least informative) elements of the info domains for each scalar type.
topInfoST :: ScalarType -> ValueInfo
topInfoST BoolType         = VIBool $ Range False True
topInfoST (IntType sgn sz) = constantIntRange sgn sz universal
topInfoST FloatType        = VIFloat
topInfoST DoubleType       = VIDouble
topInfoST (ComplexType t)  = VIProd ProdKind [topInfo t, topInfo t]

-- | The top (least informative) elements of the info domains for each type.
topInfo :: Type -> ValueInfo
-- FIXME: Should (n :# t) be VIProd (replicate n topInfoST t)?
topInfo (_ :# t)         = topInfoST t
topInfo StringType       = error "topInfo: StringType not supported"
topInfo (TupType ts)     = VIProd TupleKind $ map topInfo ts
topInfo (MutType t)      = topInfo t
topInfo (RefType t)      = topInfo t
topInfo (ArrayType r t)  = VIProd RangeKind [VIWordN r, topInfo t]
topInfo (MArrType r t)   = VIProd RangeKind [VIWordN r, topInfo t]
topInfo (ParType t)      = topInfo t -- Provisionally
topInfo (ElementsType t) = VIProd RangeKind [topInfo indexType, topInfo t]
topInfo (IVarType t)     = topInfo t
topInfo (FunType _ t)    = topInfo t
topInfo (FValType t)     = topInfo t

-- | Overloaded construction of value info ranges
class RangeVI a where
  rangeVI :: a -> a -> ValueInfo

instance RangeVI Bool where
  rangeVI l h = VIBool $ Range l h

instance RangeVI Int8 where
  rangeVI l h = VIInt8 $ Range l h

instance RangeVI Int16 where
  rangeVI l h = VIInt16 $ Range l h

instance RangeVI Int32 where
  rangeVI l h = VIInt32 $ Range l h

instance RangeVI Int64 where
  rangeVI l h = VIInt64 $ Range l h

instance RangeVI Word8 where
  rangeVI l h = VIWord8 $ Range l h

instance RangeVI Word16 where
  rangeVI l h = VIWord16 $ Range l h

instance RangeVI Word32 where
  rangeVI l h = VIWord32 $ Range l h

instance RangeVI Word64 where
  rangeVI l h = VIWord64 $ Range l h

instance RangeVI Float where
  rangeVI _ _ = VIFloat

instance RangeVI Double where
  rangeVI _ _ = VIDouble

-- | Overloaded creation of singleton ranges
singletonVI :: RangeVI a => a -> ValueInfo
singletonVI x = rangeVI x x

-- | Least upper bound and greatest lower bound for value info
lubVI :: ValueInfo -> ValueInfo -> ValueInfo
lubVI = bop rangeUnion

-- | Setting lower bound
setLB :: Integral a => a -> ValueInfo -> ValueInfo
setLB l = uop (\ r -> r{lowerBound = fromIntegral l})

-- | Apply a binary operation to two ValueInfos
bop :: (forall a . (Ord a, Num (Range a)) => Range a -> Range a -> Range a)
    -> ValueInfo -> ValueInfo -> ValueInfo
bop op (VIWord8 r1)  (VIWord8 r2)  = VIWord8   (op r1 r2)
bop op (VIInt8 r1)   (VIInt8 r2)   = VIInt8    (op r1 r2)
bop op (VIWord16 r1) (VIWord16 r2) = VIWord16  (op r1 r2)
bop op (VIInt16 r1)  (VIInt16 r2)  = VIInt16   (op r1 r2)
bop op (VIWord32 r1) (VIWord32 r2) = VIWord32  (op r1 r2)
bop op (VIInt32 r1)  (VIInt32 r2)  = VIInt32   (op r1 r2)
bop op (VIWord64 r1) (VIWord64 r2) = VIWord64  (op r1 r2)
bop op (VIInt64 r1)  (VIInt64 r2)  = VIInt64   (op r1 r2)
bop op (VIWordN r1)  (VIWordN r2)  = VIWordN   (op r1 r2)
bop _  VIFloat{}     VIFloat{}     = VIFloat
bop _  VIDouble{}    VIDouble{}    = VIDouble
bop op (VIProd k1 l1) (VIProd k2 l2)
  | k1 == k2 = VIProd k1 (zipWith (bop op) l1 l2)
bop _ _ _ = error "ValueInfo.hs:bop: mismatched patttern."

-- | Apply a unary operation to a ValueInfo
uop :: (forall a . Integral a => Range a -> Range a) -> ValueInfo -> ValueInfo
uop _ VIBool{}
  = error "ValueInfo.hs:uop: Bool is not an integral type"
uop op (VIWord8 r)                = VIWord8   (op r)
uop op (VIInt8 r)                 = VIInt8    (op r)
uop op (VIWord16 r)               = VIWord16  (op r)
uop op (VIInt16 r)                = VIInt16   (op r)
uop op (VIWord32 r)               = VIWord32  (op r)
uop op (VIInt32 r)                = VIInt32   (op r)
uop op (VIWord64 r)               = VIWord64  (op r)
uop op (VIInt64 r)                = VIInt64   (op r)
uop op (VIWordN r)                = VIWordN   (op r)
uop _  VIFloat{}                  = VIFloat
uop _  VIDouble{}                 = VIDouble
uop op (VIProd k l)               = VIProd k  (map (uop op) l)

-- | Arithmetic on ValueInfo
addVI :: ValueInfo -> ValueInfo -> ValueInfo
addVI = bop (+)
mulVI :: ValueInfo -> ValueInfo -> ValueInfo
mulVI = bop (*)

-- | Check if a ValueInfo uniquely determines a value and in that case return that value
fromSingletonVI :: Type -> ValueInfo -> Maybe (UntypedFeld ValueInfo)
fromSingletonVI (1 :# IntType sg sz) vi = go vi
  where go (VIWord8  r) = fromSingletonRange f r
        go (VIInt8   r) = fromSingletonRange f r
        go (VIWord16 r) = fromSingletonRange f r
        go (VIInt16  r) = fromSingletonRange f r
        go (VIWord32 r) = fromSingletonRange f r
        go (VIInt32  r) = fromSingletonRange f r
        go (VIWord64 r) = fromSingletonRange f r
        go (VIInt64  r) = fromSingletonRange f r
        go _           = Nothing
        f :: Integral a => a -> UntypedFeld ValueInfo
        f = In vi . Literal . LInt sg sz . fromIntegral
fromSingletonVI _ _ = Nothing

fromSingletonRange :: Eq a => (a -> UntypedFeld ValueInfo) -> Range a -> Maybe (UntypedFeld ValueInfo)
fromSingletonRange f r = if isSingleton r then Just $ f $ lowerBound r else Nothing
