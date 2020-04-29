{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

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

{- | The ValueInfo module defines the types and basic operations for value set
     analysis. In this implementation, a value set is either

     * an inclusive range [lo, hi] wrapped in a constructor giving the type of
       elements in the range, or
     * a product (v1, ..., vk) of values denoting the cartesian product of the vi.
-}

module Feldspar.ValueInfo where

import Feldspar.Range
import Feldspar.Lattice ((/\), (\/))

import Data.Bits
import Data.Int
import Data.Word

-- | The ValueInfo type wraps ranges of various types as well as cartesian
--   products into a universal domain.
--   The value info of a function is the value info of its return value.
data ValueInfo = VIBool   (Range Int) -- ^ We represent False as 0 and True as 1.
               | VIInt8   (Range Int8)
               | VIInt16  (Range Int16)
               | VIInt32  (Range Int32)
               | VIInt64  (Range Int64)
               | VIIntN   (Range IntN)
               | VIWord8  (Range Word8)
               | VIWord16 (Range Word16)
               | VIWord32 (Range Word32)
               | VIWord64 (Range Word64)
               | VIWordN  (Range WordN)
               | VIFloat  -- (Range Float)
               | VIDouble -- (Range Double)
               | VIProd [ValueInfo]
               deriving (Eq)

instance Show ValueInfo where
  show (VIBool r)   = "VIBool " ++ show r
  show (VIInt8 r)   = "VIInt8 " ++ show r
  show (VIInt16 r)  = "VIInt16 " ++ show r
  show (VIInt32 r)  = "VIInt32 " ++ show r
  show (VIInt64 r)  = "VIInt64 " ++ show r
  show (VIIntN r)   = "VIIntN " ++ show r
  show (VIWord8 r)  = "VIWord8 " ++ show r
  show (VIWord16 r) = "VIWord16 " ++ show r
  show (VIWord32 r) = "VIWord32 " ++ show r
  show (VIWord64 r) = "VIWord64 " ++ show r
  show (VIWordN r)  = "VIWordN " ++ show r
  show (VIFloat)    = "VIFloat"
  show (VIDouble)   = "VIDouble"
  show (VIProd vs)  = "VIProd " ++ show vs

-- | Overloaded construction of value info ranges
class RangeVI a where
  rangeVI :: a -> a -> ValueInfo

instance RangeVI Bool where
  rangeVI l h = VIBool $ Range (fromEnum l) (fromEnum h)

instance RangeVI Int8 where
  rangeVI l h = VIInt8 $ Range l h

instance RangeVI Int16 where
  rangeVI l h = VIInt16 $ Range l h

instance RangeVI Int32 where
  rangeVI l h = VIInt32 $ Range l h

instance RangeVI Int64 where
  rangeVI l h = VIInt64 $ Range l h

instance RangeVI IntN where
  rangeVI l h = VIIntN $ Range l h

instance RangeVI Word8 where
  rangeVI l h = VIWord8 $ Range l h

instance RangeVI Word16 where
  rangeVI l h = VIWord16 $ Range l h

instance RangeVI Word32 where
  rangeVI l h = VIWord32 $ Range l h

instance RangeVI Word64 where
  rangeVI l h = VIWord64 $ Range l h

instance RangeVI WordN where
  rangeVI l h = VIWordN $ Range l h

instance RangeVI Float where
  rangeVI l h = VIFloat

instance RangeVI Double where
  rangeVI l h = VIDouble

instance RangeVI a => RangeVI [a] where
  rangeVI ls hs = VIProd $ zipWith rangeVI ls hs

-- | Overloaded creation of singleton ranges
singletonVI :: RangeVI a => a -> ValueInfo
singletonVI x = rangeVI x x

-- | The bottom and top elements of the value info domain for booleans
boolTop :: ValueInfo
boolTop = VIBool $ Range 0 1
boolBot :: ValueInfo
boolBot = VIBool $ Range 1 0

-- | Least upper bound and greatest lower bound for value info
lubVI :: ValueInfo -> ValueInfo -> ValueInfo
lubVI = bop rangeUnion
glbVI :: ValueInfo -> ValueInfo -> ValueInfo
glbVI = bop rangeIntersection

-- | Setting lower bound
setLB :: Integral a => a -> ValueInfo -> ValueInfo
setLB l = uop (\ r -> r{lowerBound = fromIntegral l})

-- | Apply a binary operation to two ValueInfos
bop :: (forall a . (Bounded a, Ord a, Num a, FiniteBits a) => Range a -> Range a -> Range a)
    -> ValueInfo -> ValueInfo -> ValueInfo
bop op (VIBool r1)   (VIBool r2)   = VIBool    (op r1 r2)
bop op (VIWord8 r1)  (VIWord8 r2)  = VIWord8   (op r1 r2)
bop op (VIInt8 r1)   (VIInt8 r2)   = VIInt8    (op r1 r2)
bop op (VIWord16 r1) (VIWord16 r2) = VIWord16  (op r1 r2)
bop op (VIInt16 r1)  (VIInt16 r2)  = VIInt16   (op r1 r2)
bop op (VIWord32 r1) (VIWord32 r2) = VIWord32  (op r1 r2)
bop op (VIInt32 r1)  (VIInt32 r2)  = VIInt32   (op r1 r2)
bop op (VIWord64 r1) (VIWord64 r2) = VIWord64  (op r1 r2)
bop op (VIInt64 r1)  (VIInt64 r2)  = VIInt64   (op r1 r2)
bop op (VIWordN r1)  (VIWordN r2)  = VIWordN   (op r1 r2)
bop op (VIIntN r1)   (VIIntN r2)   = VIIntN    (op r1 r2)
bop _  (VIFloat)     (VIFloat)     = VIFloat
bop _  (VIDouble)    (VIDouble)    = VIDouble
bop op (VIProd l1)   (VIProd l2)   = VIProd    (zipWith (bop op) l1 l2)
bop _ _ _ = error "ValueInfo.hs:bop: mismatched patttern."

-- | Apply a unary operation to a ValueInfo
uop :: (forall a . Integral a => Range a -> Range a) -> ValueInfo -> ValueInfo
uop op (VIBool r)                 = VIBool    (op r)
uop op (VIWord8 r)                = VIWord8   (op r)
uop op (VIInt8 r)                 = VIInt8    (op r)
uop op (VIWord16 r)               = VIWord16  (op r)
uop op (VIInt16 r)                = VIInt16   (op r)
uop op (VIWord32 r)               = VIWord32  (op r)
uop op (VIInt32 r)                = VIInt32   (op r)
uop op (VIWord64 r)               = VIWord64  (op r)
uop op (VIInt64 r)                = VIInt64   (op r)
uop op (VIWordN r)                = VIWordN   (op r)
uop op (VIIntN r)                 = VIIntN    (op r)
uop _  (VIFloat)                  = VIFloat
uop _  (VIDouble)                 = VIDouble
uop op (VIProd l)                 = VIProd    (map (uop op) l)

-- | Arithmetic on ValueInfo
addVI :: ValueInfo -> ValueInfo -> ValueInfo
addVI = bop (+)
mulVI :: ValueInfo -> ValueInfo -> ValueInfo
mulVI = bop (*)
