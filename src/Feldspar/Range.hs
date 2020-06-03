{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

-- | Bounded integer ranges

module Feldspar.Range where

import qualified Data.Array.IO as IO
import Data.Bits
import Data.Int
import Data.Word
import Data.Default
import Data.Hash
import qualified Test.QuickCheck as Q
import System.Random (Random(..))

import Control.DeepSeq (NFData(..))
import Foreign.Storable (Storable)
import Language.Haskell.TH.Syntax (Lift(..))

import Feldspar.Lattice

--------------------------------------------------------------------------------
-- * Definition
--------------------------------------------------------------------------------

-- | A bounded range of values of type @a@
data Range a = Range
  { lowerBound :: a
  , upperBound :: a
  }
    deriving (Eq, Lift)

instance (Show a, Bounded a, Eq a) => Show (Range a)
  where
    show (Range l u) = "[" ++ sl ++ "," ++ su ++ "]"
      where
        sl = if l == minBound then "*" else show l
        su = if u == maxBound then "*" else show u

--------------------------------------------------------------------------------
-- * Integers
--------------------------------------------------------------------------------

-- FIXME: These types are declared here to be able to close UnsignedRep.

-- | Target-dependent unsigned integers
newtype WordN = WordN Word32
  deriving
    ( Eq, Ord, Num, Enum, IO.Ix, Real, Integral, Bits, Bounded
    , Q.Arbitrary, Random, Storable, NFData, Default
    , FiniteBits, Hashable, Lift
    )

-- | Target-dependent signed integers
newtype IntN = IntN Int32
  deriving
    ( Eq, Ord, Num, Enum, IO.Ix, Real, Integral, Bits, Bounded
    , Q.Arbitrary, Random, Storable, NFData, Default
    , FiniteBits, Hashable
    )

instance Show WordN
  where
    show (WordN a) = show a

instance Show IntN
  where
    show (IntN a) = show a

-- | Convenience alias for bounded integers
type BoundedInt a = (Ord a, Bounded a, Integral a, FiniteBits a,
                     Integral (UnsignedRep a), FiniteBits (UnsignedRep a))

-- | Type famliy to determine the bit representation of a type
type family UnsignedRep a where
  UnsignedRep Int8   = Word8
  UnsignedRep Word8  = Word8
  UnsignedRep Int16  = Word16
  UnsignedRep Word16 = Word16
  UnsignedRep Int32  = Word32
  UnsignedRep Word32 = Word32
  UnsignedRep Int64  = Word64
  UnsignedRep Word64 = Word64
  UnsignedRep WordN  = Word32
  UnsignedRep IntN   = Word32

-- | Convert an 'Integral' to its unsigned representation while preserving
-- bit width
unsigned :: (Integral a, Num (UnsignedRep a)) => a -> UnsignedRep a
unsigned = fromIntegral

-- | A convenience function for defining range propagation.
--   @handleSign propU propS@ chooses @propU@ for unsigned types and
--   @propS@ for signed types.
handleSign :: forall a b . Bits a =>
    (Range a -> b) -> (Range a -> b) -> Range a -> b
handleSign u s
    | isSigned (undefined::a) = s
    | otherwise               = u

-- | Shows a bound.
showBound :: (Show a, Bounded a, Eq a) => a -> String
showBound a
    | a  `elem` [maxBound,minBound] = "*"
    | otherwise                     = show a

-- | A textual representation of ranges.
showRange :: (Show a, Bounded a, Ord a, Eq a) => Range a -> String
showRange r@(Range l u)
  | isEmpty r     = "[]"
  | isSingleton r = show u
  | otherwise     = "[" ++ showBound l ++ "," ++ showBound u ++ "]"

-- | Requires a monotonic function
mapMonotonic :: (a -> b) -> Range a -> Range b
mapMonotonic f (Range l u) = Range (f l) (f u)

-- | Requires a monotonic function
mapMonotonic2 :: (a -> b -> c) -> Range a -> Range b -> Range c
mapMonotonic2 f (Range l1 u1) (Range l2 u2) = Range (f l1 l2) (f u1 u2)



--------------------------------------------------------------------------------
-- * Lattice operations
--------------------------------------------------------------------------------

instance (Bounded a, Ord a) => Lattice (Range a)
  where
    bot  = emptyRange
    top  = fullRange
    (\/) = rangeUnion
    (/\) = rangeIntersection

-- | The range containing no elements
emptyRange :: Bounded a => Range a
emptyRange = Range maxBound minBound

-- | The range containing all elements of a type
fullRange :: Bounded a => Range a
fullRange = Range minBound maxBound

-- | Construct a range
range :: Ord a => a -> a -> Range a
range x y | y < x     = Range y x
          | otherwise = Range x y

-- | The range containing one element
singletonRange :: a -> Range a
singletonRange a = Range a a

-- | The range from @0@ to the maximum element
naturalRange :: (Bounded a, Num a) => Range a
naturalRange = Range 0 maxBound

-- | The range from @1@ to the maximum element
positiveRange :: (Bounded a, Num a) => Range a
positiveRange = Range 1 maxBound

-- | The range from the smallest negative element to @-1@.
--   Undefined for unsigned types
negativeRange :: forall a . (Bounded a, Num a, Bits a) => Range a
negativeRange
  | isSigned (undefined::a) = Range minBound (-1)
  | otherwise               = emptyRange

-- | The size of a range.
rangeSize :: Enum a => Range a -> Int
rangeSize (Range l u) = fromEnum u - fromEnum l + fromEnum 1

-- | Checks if the range is empty
isEmpty :: Ord a => Range a -> Bool
isEmpty (Range l u) = u < l

-- | Checks if the range contains all values of the type
isFull :: (Bounded a, Eq a) => Range a -> Bool
isFull = (==fullRange)

-- | Checks is the range contains exactly one element
isSingleton :: Eq a => Range a -> Bool
isSingleton (Range l u) = l==u

-- | @r1 \`isSubRangeOf\` r2@ checks is all the elements in @r1@ are included
--   in @r2@
isSubRangeOf :: Ord a => Range a -> Range a -> Bool
isSubRangeOf r1@(Range l1 u1) r2@(Range l2 u2)
    | isEmpty r1 = True
    | isEmpty r2 = False
    | otherwise  = (l1>=l2) && (u1<=u2)

-- | Checks whether a range is a sub-range of the natural numbers.
isNatural :: (Ord a, Num a, Bounded a) => Range a -> Bool
isNatural = (`isSubRangeOf` naturalRange)

-- | Checks whether a range is a sub-range of the positive numbers.
isPositive :: (Ord a, Num a, Bounded a) => Range a -> Bool
isPositive = (`isSubRangeOf` positiveRange)

-- | Checks whether a range is a sub-range of the negative numbers.
isNegative :: (Ord a, Bounded a, Num a, Bits a) => Range a -> Bool
isNegative = (`isSubRangeOf` negativeRange)

-- | @a \`inRange\` r@ checks is @a@ is an element of the range @r@.
inRange :: Ord a => a -> Range a -> Bool
inRange a r = singletonRange a `isSubRangeOf` r

-- | A convenience function for defining range propagation. If the input
--   range is empty then the result is also empty.
rangeOp :: Ord a => (Range a -> Range a) -> Range a -> Range a
rangeOp f r = if isEmpty r then r else f r

-- | See 'rangeOp'.
rangeOp2 :: Ord a =>
    (Range a -> Range a -> Range a) -> Range a -> Range a -> Range a
rangeOp2 f r1 r2
  | isEmpty r1 = r1
  | isEmpty r2 = r2
  | otherwise  = f r1 r2

-- | Union on ranges.
rangeUnion :: Ord a => Range a -> Range a -> Range a
r1 `rangeUnion` r2
    | isEmpty r1 = r2
    | isEmpty r2 = r1
    | otherwise  = r1 `union` r2
  where
    union (Range l1 u1) (Range l2 u2) = Range (min l1 l2) (max u1 u2)

-- | Intersection on ranges.
rangeIntersection :: Ord a => Range a -> Range a -> Range a
rangeIntersection = rangeOp2 intersection
  where
    intersection (Range l1 u1) (Range l2 u2) = Range (max l1 l2) (min u1 u2)

-- | @disjoint r1 r2@ returns true when @r1@ and @r2@ have no elements in
--   common.
disjoint :: (Bounded a, Ord a) => Range a -> Range a -> Bool
disjoint r1 r2 = isEmpty (r1 /\ r2)

-- | @rangeByRange ra rb@: Computes the range of the following set
--
-- > {x | a <- ra, b <- rb, x <- Range a b}
rangeByRange :: (Bounded a, Ord a) => Range a -> Range a -> Range a
rangeByRange r1 r2
    | isEmpty r1 = emptyRange
    | isEmpty r2 = emptyRange
    | otherwise = Range (lowerBound r1) (upperBound r2)

-- | @rangeGap r1 r2@ returns a range of all the elements between @r1@ and
--   @r2@ including the boundary elements. If @r1@ and @r2@ have elements in
--   common the result is an empty range.
rangeGap :: (Ord a, Bounded a) => Range a -> Range a -> Range a
rangeGap = rangeOp2 gap
  where
    gap (Range l1 u1) (Range l2 u2)
      | u1 < l2 = range u1 l2
      | u2 < l1 = range u2 l1
    gap _ _     = emptyRange
  -- If the result is non-empty, it will include the boundary elements from the
  -- two ranges.

-- | @r1 \`rangeLess\` r2:@
--
-- Checks if all elements of @r1@ are less than all elements of @r2@.
rangeLess :: Ord a => Range a -> Range a -> Bool
rangeLess r1 r2
  | isEmpty r1 || isEmpty r2 = True
rangeLess (Range _ u1) (Range l2 _) = u1 < l2

-- | @r1 \`rangeLessEq\` r2:@
--
-- Checks if all elements of @r1@ are less than or equal to all elements of
-- @r2@.
rangeLessEq :: Ord a => Range a -> Range a -> Bool
rangeLessEq (Range _ u1) (Range l2 _) = u1 <= l2



--------------------------------------------------------------------------------
-- * Propagation
--------------------------------------------------------------------------------

-- | Implements 'fromInteger' as a 'singletonRange', and implements correct
-- range propagation for arithmetic operations.
instance (Bounded a, Ord a, Num a, FiniteBits a) => Num (Range a)
  where
    fromInteger = singletonRange . fromInteger
    abs         = rangeAbs
    signum      = rangeSignum
    negate      = rangeNeg
    (+)         = rangeAdd
    (*)         = rangeMul
    (-)         = rangeSub

-- | Propagates range information through @abs@.
rangeAbs :: (Bounded a, Bits a, Num a, Ord a) => Range a -> Range a
rangeAbs = rangeOp $ \r -> case r of
    Range l u
      | isNatural  r -> r
      | r == singletonRange minBound -> r
      | minBound `inRange` r -> range minBound maxBound
      | isNegative r -> range (abs u) (abs l)
      | otherwise    -> range 0 (abs l `max` abs u)

-- | Propagates range information through 'signum'.
rangeSignum :: (Bounded a, Ord a, Num a, Bits a) => Range a -> Range a
rangeSignum = handleSign rangeSignumUnsigned rangeSignumSigned

-- | Signed case for 'rangeSignum'.
rangeSignumSigned :: (Bounded a, Ord a, Num a, Bits a) => Range a -> Range a
rangeSignumSigned = rangeOp sign
  where
    sign r
      | range (-1) 1 `isSubRangeOf` r = range (-1) 1
      | range (-1) 0 `isSubRangeOf` r = range (-1) 0
      | range 0 1    `isSubRangeOf` r = range 0 1
      | inRange 0 r                   = singletonRange 0
      | isNatural r                   = singletonRange 1
      | isNegative r                  = singletonRange (-1)

-- | Unsigned case for 'rangeSignum'.
rangeSignumUnsigned :: (Ord a, Num a) => Range a -> Range a
rangeSignumUnsigned = rangeOp sign
    where
      sign r
          | r == singletonRange 0 = r
          | not (0 `inRange` r)   = singletonRange 1
          | otherwise             = range 0 1

-- | Propagates range information through negation.
rangeNeg :: (Bounded a, Bits a, Ord a, Num a) => Range a -> Range a
rangeNeg = handleSign rangeNegUnsigned rangeNegSigned

-- | Unsigned case for 'rangeNeg'.
rangeNegUnsigned :: (Bounded a, Ord a, Num a) => Range a -> Range a
rangeNegUnsigned (Range l u)
    | l == 0 && u /= 0 = fullRange
    | otherwise        = range (-u) (-l)
-- Code from Hacker's Delight

-- | Signed case for 'rangeNeg'.
rangeNegSigned :: (Bounded a, Ord a, Num a) => Range a -> Range a
rangeNegSigned (Range l u)
    | l == minBound && u == minBound = singletonRange minBound
    | l == minBound                  = fullRange
    | otherwise                      = range (-u) (-l)
-- Code from Hacker's Delight

-- | Propagates range information through addition.
rangeAdd :: (Bounded a, Bits a, Ord a, Num a) => Range a -> Range a -> Range a
rangeAdd = handleSign rangeAddUnsigned rangeAddSigned

-- | Unsigned case for 'rangeAdd'.
rangeAddUnsigned :: (Bounded a, Ord a, Num a)
                 => Range a -> Range a -> Range a
rangeAddUnsigned (Range l1 u1) (Range l2 u2)
    | s >= l1 && t < u1 = fullRange
    | otherwise         = range s t
  where
    s = l1 + l2
    t = u1 + u2
-- Code from Hacker's Delight

-- | Signed case for 'rangeAdd'.
rangeAddSigned :: (Bounded a, Bits a, Ord a, Num a)
               => Range a -> Range a -> Range a
rangeAddSigned (Range a b) (Range c d)
    | (u .|. v) < 0 = fullRange
    | otherwise     = range s t
  where
    s = a + c
    t = b + d
    u = a .&. c .&. complement s .&. complement (b .&. d .&. complement t)
    v = ((a `xor` c) .|. complement (a `xor` s)) .&. (complement b .&. complement d .&. t)

-- | Propagates range information through subtraction.
rangeSub :: (Bounded a, Bits a, Ord a, Num a) => Range a -> Range a -> Range a
rangeSub = handleSign rangeSubUnsigned rangeSubSigned

-- | Unsigned case for 'rangeSub'.
rangeSubUnsigned :: (Bounded a, Ord a, Num a) => Range a -> Range a -> Range a
rangeSubUnsigned (Range l1 u1) (Range l2 u2)
    | s > l1 && t <= u1 = fullRange
    | otherwise         = range s t
  where
    s = l1 - u2
    t = u1 - l2
  -- Note: This is more accurate than the default definition using 'negate',
  --       because 'negate' always overflows for unsigned numbers.
  -- Code from Hacker's Delight

rangeSubSigned :: (Bounded a, Bits a, Ord a, Num a)
               => Range a -> Range a -> Range a
rangeSubSigned (Range a b) (Range c d)
    | (u .|. v) < 0 = fullRange
    | otherwise     = range s t
  where
    s = a - d
    t = b - c
    u = a .&. complement d .&. complement s .&.
        complement (b .&. complement c .&. complement t)
    v = (xor a (complement d) .|. complement (xor a s)) .&.
        (complement b .&. c .&. t)
-- Code from Hacker's Delight

-- | Saturating unsigned subtraction
subSat :: (Ord a, Num a) => a -> a -> a
subSat a b = a - min a b

-- | Range propagation for 'subSat'
rangeSubSat :: (Ord a, Num a) => Range a -> Range a -> Range a
rangeSubSat r1 r2 = range
    (subSat (lowerBound r1) (upperBound r2))
    (subSat (upperBound r1) (lowerBound r2))

-- | Propagates range information through multiplication
rangeMul :: (Bounded a, Ord a, Num a, FiniteBits a)
         => Range a -> Range a -> Range a
rangeMul = handleSign rangeMulUnsigned rangeMulSigned

-- | Signed case for 'rangeMul'.
rangeMulSigned :: forall a . (Bounded a, Ord a, Num a, FiniteBits a)
               => Range a -> Range a -> Range a
rangeMulSigned r1 r2
    | r1 == singletonRange 0 || r2 == singletonRange 0 = singletonRange 0
    -- The following case is important because the 'maxAbs' function doesn't
    -- work for 'minBound' on signed numbers.
    | lowerBound r1 == minBound || lowerBound r2 == minBound
        = range minBound maxBound
    | bits (maxAbs r1) + bits (maxAbs r2) <= finiteBitSize (undefined :: a) - 1
        = range (minimum [b1,b2,b3,b4]) (maximum [b1,b2,b3,b4])
    | otherwise = range minBound maxBound
  where maxAbs (Range l u) = max (abs l) (abs u)
        b1 = lowerBound r1 * lowerBound r2
        b2 = lowerBound r1 * upperBound r2
        b3 = upperBound r1 * lowerBound r2
        b4 = upperBound r1 * upperBound r2

-- | Unsigned case for 'rangeMul'.
rangeMulUnsigned :: forall a . (Bounded a, Ord a, Num a, FiniteBits a)
                 => Range a -> Range a -> Range a
rangeMulUnsigned r1 r2
    | bits (upperBound r1) + bits (upperBound r2)
      <= finiteBitSize (undefined :: a)
        = mapMonotonic2 (*) r1 r2
    | otherwise = universal

-- | Returns the position of the highest bit set to 1. Counting starts at 1.
bits :: forall a . FiniteBits a => a -> Int
bits b = finiteBitSize (undefined :: a) - countLeadingZeros b

-- | Propagates range information through exponentiation.
rangeExp :: (Bounded a, Integral a, FiniteBits a)
         => Range a -> Range a -> Range a
rangeExp = handleSign rangeExpUnsigned rangeExpSigned

-- | Unsigned case for 'rangeExp'.
rangeExpUnsigned :: (Bounded a, Integral a, FiniteBits a)
                 => Range a -> Range a -> Range a
rangeExpUnsigned m@(Range l1 u1) e@(Range l2 u2)
    | toInteger (bits u1) * toInteger u2 > toInteger (finiteBitSize l1) + 1 = universal
    | toInteger u1 ^ toInteger u2 > toInteger (maxBound `asTypeOf` l1) = universal
    | 0 `inRange` m && 0 `inRange` e = range 0 (max b1 b2)
    | otherwise = range b1 b2
  where b1 = l1 ^ l2
        b2 = u1 ^ u2

-- | Signed case for 'rangeExp'
rangeExpSigned :: (Bounded a, Num a, Ord a) => Range a -> Range a -> Range a
rangeExpSigned m _ | m == singletonRange (-1) = range (-1) 1
rangeExpSigned _ _ = universal

-- | @a \`maxPlus\` b@ adds @a@ and @b@ but if the addition overflows then
--   'maxBound' is returned.
maxPlus :: (Bounded a, Ord a, Num a) => a -> a -> a
maxPlus b d = if s < b then maxBound
              else s
  where s = b + d

-- | Accurate lower bound for '.|.' on unsigned numbers.
minOrUnsigned :: (Integral a, Integral (UnsignedRep a),
                  FiniteBits (UnsignedRep a))
              => a -> a -> a -> a -> a
minOrUnsigned a b c d =
    fromIntegral $ minOr (unsigned a) (unsigned b) (unsigned c) (unsigned d)

minOr :: (Ord a, Num a, FiniteBits a) => a -> a -> a -> a -> a
minOr a b c d = loop (bit (finiteBitSize a - 1))
  where loop 0 = a .|. c
        loop m
            | complement a .&. c .&. m > 0 =
                let temp = (a .|. m) .&. negate m
                in if temp <= b
                   then temp .|. c
                   else loop (shiftR m 1)
            | a .&. complement c .&. m > 0 =
                let temp = (c .|. m) .&. negate m
                in if temp <= d
                   then a .|. temp
                   else loop (shiftR m 1)
            | otherwise = loop (shiftR m 1)
-- Code from Hacker's Delight.

-- | Accurate upper bound for '.|.' on unsigned numbers.
maxOrUnsigned :: (Integral a, Integral (UnsignedRep a),
                  FiniteBits (UnsignedRep a))
              => a -> a -> a -> a -> a
maxOrUnsigned a b c d =
    fromIntegral $ maxOr (unsigned a) (unsigned b) (unsigned c) (unsigned d)

maxOr :: (Ord a, Num a, FiniteBits a) => a -> a -> a -> a -> a
maxOr a b c d = loop (bit (finiteBitSize a - 1))
  where loop 0 = b .|. d
        loop m
             | b .&. d .&. m > 0 =
                 let temp = (b - m) .|. (m - 1)
                 in if temp >= a
                    then temp .|. d
                    else let tmp = (d - m) .|. (m - 1)
                         in if tmp >= c
                            then b .|. tmp
                            else loop (shiftR m 1)
             | otherwise = loop (shiftR m 1)
-- Code from Hacker's Delight.

-- | Accurate lower bound for '.&.' on unsigned numbers
minAndUnsigned :: (Integral a, Bits a,
                   Integral (UnsignedRep a), FiniteBits (UnsignedRep a))
               => a -> a -> a -> a -> a
minAndUnsigned a b c d =
  complement $ maxOrUnsigned (complement b) (complement a)
                             (complement d) (complement c)

-- | Accurate upper bound for '.&.' on unsigned numbers
maxAndUnsigned :: (Integral a, Bits a,
                   Integral (UnsignedRep a), FiniteBits (UnsignedRep a))
               => a -> a -> a -> a -> a
maxAndUnsigned a b c d =
  complement $ minOrUnsigned (complement b) (complement a)
                             (complement d) (complement c)

-- | Accurate lower bound for 'xor' on unsigned numbers
minXorUnsigned :: (Integral a, Bits a,
                   Integral (UnsignedRep a), FiniteBits (UnsignedRep a))
               => a -> a -> a -> a -> a
minXorUnsigned a b c d = x .|. y
  where
    x = minAndUnsigned a b (complement d) (complement c)
    y = minAndUnsigned (complement b) (complement a) c d

-- | Accurate upper bound for 'xor' on unsigned numbers
maxXorUnsigned :: (Integral a, Bits a,
                   Integral (UnsignedRep a), FiniteBits (UnsignedRep a))
               => a -> a -> a -> a -> a
maxXorUnsigned a b c d = maxOrUnsigned 0 x 0 y
  where
    x = maxAndUnsigned a b (complement d) (complement c)
    y = maxAndUnsigned (complement b) (complement a) c d

minOrSigned :: (Integral a,
                Integral (UnsignedRep a), FiniteBits (UnsignedRep a))
            => a -> a -> a -> a -> a
minOrSigned a b c d = case (a<0,b<0,c<0,d<0) of
    (True ,True ,True ,True ) -> minOrUnsigned a b c d
    (True ,True ,True ,False) -> a
    (True ,True ,False,False) -> minOrUnsigned a b c d
    (True ,False,True ,True ) -> c
    (True ,False,True ,False) -> min a c
    (True ,False,False,False) -> minOrUnsigned a (-1) c d
    (False,False,True ,True ) -> minOrUnsigned a b c d
    (False,False,True ,False) -> minOrUnsigned a b c (-1)
    (False,False,False,False) -> minOrUnsigned a b c d
    (_    ,_    ,_    ,_    ) -> error "Can't propagate over 'or'"

maxOrSigned :: (Integral a,
                Integral (UnsignedRep a), FiniteBits (UnsignedRep a))
            => a -> a -> a -> a -> a
maxOrSigned a b c d = case (a<0,b<0,c<0,d<0) of
    (True ,True ,True ,True ) -> maxOrUnsigned a b c d
    (True ,True ,True ,False) -> -1
    (True ,True ,False,False) -> maxOrUnsigned a b c d
    (True ,False,True ,True ) -> -1
    (True ,False,True ,False) -> maxOrUnsigned 0 b 0 d
    (True ,False,False,False) -> maxOrUnsigned 0 b c d
    (False,False,True ,True ) -> maxOrUnsigned a b c d
    (False,False,True ,False) -> maxOrUnsigned a b 0 d
    (False,False,False,False) -> maxOrUnsigned a b c d
    (_    ,_    ,_    ,_    ) -> error "Can't propagate over 'or'"

minAndSigned :: (Integral a, Bits a,
                Integral (UnsignedRep a), FiniteBits (UnsignedRep a))
             => a -> a -> a -> a -> a
minAndSigned a b c d =
    complement $ maxOrSigned (complement b) (complement a)
                             (complement d) (complement c)

maxAndSigned :: (Integral a, Bits a,
                Integral (UnsignedRep a), FiniteBits (UnsignedRep a))
             => a -> a -> a -> a -> a
maxAndSigned a b c d =
    complement $ minOrSigned (complement b) (complement a)
                             (complement d) (complement c)

-- | Propagates range information through '.|.'.
rangeOr :: forall a . (Integral a, Bits a, Integral (UnsignedRep a),
                       FiniteBits (UnsignedRep a))
                      => Range a -> Range a -> Range a
rangeOr = handleSign rangeOrUnsignedAccurate rangeOrSignedAccurate

-- | Accurate range propagation through '.|.' for unsigned types.
rangeOrUnsignedAccurate :: (Integral a, Integral (UnsignedRep a),
                            FiniteBits (UnsignedRep a))
                        => Range a -> Range a -> Range a
rangeOrUnsignedAccurate (Range l1 u1) (Range l2 u2) =
    range (minOrUnsigned l1 u1 l2 u2) (maxOrUnsigned l1 u1 l2 u2)
-- Code from Hacker's Delight.

rangeOrSignedAccurate :: (Integral a, Integral (UnsignedRep a),
                          FiniteBits (UnsignedRep a))
                      => Range a -> Range a -> Range a
rangeOrSignedAccurate (Range a b) (Range c d) =
    range (minOrSigned a b c d) (maxOrSigned a b c d)

-- | Propagating range information through '.&.'.
rangeAnd :: forall a . (Integral a, Bits a,
                        Integral (UnsignedRep a), FiniteBits (UnsignedRep a))
                       => Range a -> Range a -> Range a
rangeAnd = handleSign rangeAndUnsignedAccurate rangeAndSignedAccurate

-- | Accurate range propagation through '.&.' for unsigned types
rangeAndUnsignedAccurate :: (Integral a, Bits a,
                             Integral (UnsignedRep a),
                             FiniteBits (UnsignedRep a))
                         => Range a -> Range a -> Range a
rangeAndUnsignedAccurate (Range a b) (Range c d) =
    range (minAndUnsigned a b c d) (maxAndUnsigned a b c d)

rangeAndSignedAccurate :: (Integral a, Bits a,
                           Integral (UnsignedRep a),
                           FiniteBits (UnsignedRep a))
                       => Range a -> Range a -> Range a
rangeAndSignedAccurate (Range a b) (Range c d) =
    range (minAndSigned a b c d) (maxAndSigned a b c d)

-- | Propagating range information through 'xor'.
rangeXor :: forall a . (Bounded a, Integral a, Bits a,
                        Integral (UnsignedRep a), FiniteBits (UnsignedRep a))
                       => Range a -> Range a -> Range a
rangeXor = handleSign rangeXorUnsignedAccurate (\_ _ -> universal)

-- | Accurate range propagation through 'xor' for unsigned types
rangeXorUnsignedAccurate :: (Integral a, Bits a,
                             Integral (UnsignedRep a),
                             FiniteBits (UnsignedRep a))
                         => Range a -> Range a -> Range a
rangeXorUnsignedAccurate (Range a b) (Range c d) =
    range (minXorUnsigned a b c d) (maxXorUnsigned a b c d)

-- |
-- | Propagating range information through 'shiftLU'.
rangeShiftLU :: (Bounded a, Integral a, FiniteBits a, Integral b)
             => Range a -> Range b -> Range a
rangeShiftLU = handleSign rangeShiftLUUnsigned (\_ _ -> universal)
-- TODO: improve accuracy

-- | Unsigned case for 'rangeShiftLU'.
rangeShiftLUUnsigned :: (Bounded a, Ord a, Num a, FiniteBits a, Integral b)
                     => Range a -> Range b -> Range a
rangeShiftLUUnsigned (Range _ u1) (Range _ u2)
    | toInteger (bits u1) + fromIntegral u2 > toInteger (finiteBitSize u1) = universal
rangeShiftLUUnsigned (Range l1 u1) (Range l2 u2)
    = range (shiftL l1 (fromIntegral l2)) (shiftL u1 (fromIntegral u2))

-- | Propagating range information through 'shiftRU'.
rangeShiftRU :: (Num a, Bits a, Ord a, Ord b, Integral b, Bounded a)
             => Range a -> Range b -> Range a
rangeShiftRU = handleSign rangeShiftRUUnsigned (\_ _ -> universal)
-- TODO: improve accuracy

-- | Unsigned case for 'rangeShiftRU'.
rangeShiftRUUnsigned :: (Num a, Bits a, Ord a, Integral b)
                     => Range a -> Range b -> Range a
rangeShiftRUUnsigned (Range l1 u1) (Range l2 u2)
    = range (correctShiftRU l1 u2) (correctShiftRU u1 l2)

-- | This is a replacement fror Haskell's shiftR. If we carelessly use
--   Haskell's variant then we will get left shifts for very large shift values.
correctShiftRU :: (Num a, Bits a, Integral b) => a -> b -> a
correctShiftRU _ i | i > fromIntegral (maxBound :: Int) = 0
correctShiftRU a i = shiftR a (fromIntegral i)

-- | Propagating range information through 'complement'
rangeComplement :: (Bits a, Ord a) => Range a -> Range a
rangeComplement (Range l u) = range (complement l) (complement u)

-- | Propagates range information through 'max'.
rangeMax :: Ord a => Range a -> Range a -> Range a
rangeMax r1 r2
    | isEmpty r1        = r2
    | isEmpty r2        = r1
    | r1 `rangeLess` r2 = r2
    | r2 `rangeLess` r1 = r1
    | otherwise         = mapMonotonic2 max r1 r2

-- | Analogous to 'rangeMax'
rangeMin :: Ord a => Range a -> Range a -> Range a
rangeMin r1 r2
    | isEmpty r1        = r2
    | isEmpty r2        = r1
    | r1 `rangeLess` r2 = r1
    | r2 `rangeLess` r1 = r2
    | otherwise         = mapMonotonic2 min r1 r2

instance Ord a => Ord (Range a)
  where
    compare = error "compare: I don't make sense for (Range a)"
    min = rangeMin
    max = rangeMax

-- | Propagates range information through 'mod'.
-- Note that we assume Haskell semantics for 'mod'.
rangeMod :: (Bounded a, Ord a, Enum a, Num a, Bits a)
         => Range a -> Range a -> Range a
rangeMod d r
    | isSigned (lowerBound d) &&
      minBound `inRange` d && (-1) `inRange` r = fullRange
    | d `rangeLess` r && isNatural r && isNatural d = d
    | isNatural r = range 0 (pred (upperBound r))
    | r `rangeLess` d && isNeg r && isNeg d = d
    | isNeg r = range (succ (lowerBound r)) 0
    where
      isNeg = (`isSubRangeOf` negs)
      negs  = negativeRange \/ singletonRange 0
rangeMod _ (Range l u) = Range (succ l) (pred u)

-- | Propagates range information through 'rem'.
-- Note that we assume Haskell semantics for 'rem'.
rangeRem :: (Bounded a, Ord a, Enum a, Num a, Bits a)
         => Range a -> Range a -> Range a
rangeRem d r
    | isSigned (lowerBound d) &&
      minBound `inRange` d && (-1) `inRange` r = fullRange
    | d `rangeLessAbs` r && isNatural d = d
    | isNatural d = range 0 (upperBound (rangeAbs r))
    | d `absRangeLessAbs` r && isNeg d = d
    | isNeg d = range (negate (upperBound (rangeAbs r))) 0
    where
      isNeg = (`isSubRangeOf` negs)
      negs  = negativeRange \/ singletonRange 0
rangeRem _ (Range l u)
    | abs l >= abs u || l == minBound = range (succ $ negate $ abs l) (predAbs l)
    | otherwise      = range (succ $ negate $ abs u) (predAbs u)

predAbs :: (Bounded a, Eq a, Num a, Enum a) => a -> a
predAbs l | l == minBound = abs (succ l)
          | otherwise     = pred (abs l)

-- | Propagates range information through 'div'
rangeDiv :: (Bounded a, Integral a, Bits a) => Range a -> Range a -> Range a
rangeDiv = handleSign rangeDivU (\_ _ -> universal)

-- | Unsigned case for 'rangeDiv'
rangeDivU :: (Bounded a, Integral a, Eq a) => Range a -> Range a -> Range a
rangeDivU (Range _  _ ) (Range l2 u2) | l2 == 0 || u2 == 0 = universal
rangeDivU (Range l1 u1) (Range l2 u2) = Range (l1 `quot` u2) (u1 `quot`l2)

-- | Propagates range information through 'quot'.
rangeQuot :: (Bounded a, Integral a, Bits a) => Range a -> Range a -> Range a
rangeQuot = handleSign rangeQuotU (\_ _ -> universal)

-- | Unsigned case for 'rangeQuot'.
rangeQuotU :: (Bounded a, Integral a) => Range a -> Range a -> Range a
rangeQuotU (Range _  _ ) (Range l2 u2) | l2 == 0 || u2 == 0 = universal
rangeQuotU (Range l1 u1) (Range l2 u2) = Range (l1 `quot` u2) (u1 `quot` l2)

-- | Writing @d \`rangeLess\` abs r@ doesn't mean what you think it does because
-- 'r' may contain minBound which doesn't have a positive representation.
-- Instead, this function should be used.
rangeLessAbs :: (Bounded a, Ord a, Bits a, Num a, Enum a)
             => Range a -> Range a -> Bool
rangeLessAbs d r
    | r == singletonRange minBound
        = lowerBound d /= minBound
    | lowerBound r == minBound
        = d `rangeLess` rangeAbs (range (succ (lowerBound r)) (upperBound r))
    | otherwise = d `rangeLess` rangeAbs r

-- | Similar to 'rangeLessAbs' but replaces the expression
--   @abs d \`rangeLess\` abs r@ instead.
absRangeLessAbs :: (Bounded a, Ord a, Num a, Bits a, Enum a)
                => Range a -> Range a -> Bool
absRangeLessAbs d r
    | lowerBound d == minBound = False
    | otherwise = rangeAbs d `rangeLessAbs` r
