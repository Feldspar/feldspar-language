{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

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

-- TODO This module should be broken up into smaller pieces. Since most
-- functions seem to be useful not only for Feldspar, it would probably be good
-- to make a separate package. In any case, the modules should go under
-- `Data.Range`. If there are functions that are very Feldspar specific, these
-- should go into `Feldspar.Core.Constructs.*` (or whereever suitable).

import Data.Bits
import Feldspar.Lattice

--------------------------------------------------------------------------------
-- * Definition
--------------------------------------------------------------------------------

-- | A bounded range of values of type @a@
data Range a = Range
  { lowerBound :: a
  , upperBound :: a
  }
    deriving (Eq, Show)

-- | Convenience alias for bounded integers
class    (Ord a, Num a, Bounded a, Integral a, Bits a) => BoundedInt a
instance (Ord a, Num a, Bounded a, Integral a, Bits a) => BoundedInt a

-- | A convenience function for defining range propagation.
--   @handleSign propU propS@ chooses @propU@ for unsigned types and
--   @propS@ for signed types.
handleSign :: forall a b . BoundedInt a =>
    (Range a -> b) -> (Range a -> b) -> (Range a -> b)
handleSign u s
    | isSigned (undefined::a) = s
    | otherwise               = u

-- | Shows a bound.
showBound :: (Show a, BoundedInt a) => a -> String
showBound a
    | a  `elem` [maxBound,minBound] = "*"
    | otherwise                     = show a

-- | A textual representation of ranges.
showRange :: (Show a, BoundedInt a) => Range a -> String
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

instance BoundedInt a => Lattice (Range a)
  where
    empty     = emptyRange
    universal = fullRange
    (\/)      = rangeUnion
    (/\)      = rangeIntersection

-- | The range containing no elements
emptyRange :: BoundedInt a => Range a
emptyRange = Range maxBound minBound

-- | The range containing all elements of a type
fullRange :: BoundedInt a => Range a
fullRange = Range minBound maxBound

-- | Construct a range
range :: Ord a => a -> a -> Range a
range x y | y < x     = Range y x
          | otherwise = Range x y

-- | The range containing one element
singletonRange :: a -> Range a
singletonRange a = Range a a

-- | The range from @0@ to the maximum element
naturalRange :: BoundedInt a => Range a
naturalRange = Range 0 maxBound

-- | The range from the smallest negative element to @-1@.
--   Undefined for unsigned types
negativeRange :: forall a . BoundedInt a => Range a
negativeRange
  | isSigned (undefined::a) = Range minBound (-1)
  | otherwise               = emptyRange

-- | The size of a range. Beware that the size may not always be representable
--   for signed types. For instance
--   @rangeSize (range minBound maxBound) :: Int@ gives a nonsense answer.
rangeSize :: BoundedInt a => Range a -> a
rangeSize (Range l u) = u-l+1

-- | Checks if the range is empty
isEmpty :: BoundedInt a => Range a -> Bool
isEmpty (Range l u) = u < l

-- | Checks if the range contains all values of the type
isFull :: BoundedInt a => Range a -> Bool
isFull = (==fullRange)

-- | Checks is the range contains exactly one element
isSingleton :: BoundedInt a => Range a -> Bool
isSingleton (Range l u) = l==u

-- | @r1 \`isSubRangeOf\` r2@ checks is all the elements in @r1@ are included
--   in @r2@
isSubRangeOf :: BoundedInt a => Range a -> Range a -> Bool
isSubRangeOf r1@(Range l1 u1) r2@(Range l2 u2)
    | isEmpty r1 = True
    | isEmpty r2 = False
    | otherwise  = (l1>=l2) && (u1<=u2)

-- | Checks whether a range is a sub-range of the natural numbers.
isNatural :: BoundedInt a => Range a -> Bool
isNatural = (`isSubRangeOf` naturalRange)

-- | Checks whether a range is a sub-range of the negative numbers.
isNegative :: BoundedInt a => Range a -> Bool
isNegative = (`isSubRangeOf` negativeRange)

-- | @a \`inRange\` r@ checks is @a@ is an element of the range @r@.
inRange :: BoundedInt a => a -> Range a -> Bool
inRange a r = singletonRange a `isSubRangeOf` r

-- | A convenience function for defining range propagation. If the input
--   range is empty then the result is also empty.
rangeOp :: BoundedInt a => (Range a -> Range a) -> (Range a -> Range a)
rangeOp f r = if isEmpty r then r else f r

-- | See 'rangeOp'.
rangeOp2 :: BoundedInt a =>
    (Range a -> Range a -> Range a) -> (Range a -> Range a -> Range a)
rangeOp2 f r1 r2
  | isEmpty r1 = r1
  | isEmpty r2 = r2
  | otherwise  = f r1 r2

-- | Union on ranges.
rangeUnion :: BoundedInt a => Range a -> Range a -> Range a
r1 `rangeUnion` r2
    | isEmpty r1 = r2
    | isEmpty r2 = r1
    | otherwise  = union r1 r2
  where
    union (Range l1 u1) (Range l2 u2) = Range (min l1 l2) (max u1 u2)

-- | Intersection on ranges.
rangeIntersection :: BoundedInt a => Range a -> Range a -> Range a
rangeIntersection = rangeOp2 intersection
  where
    intersection (Range l1 u1) (Range l2 u2) = Range (max l1 l2) (min u1 u2)

-- | @disjoint r1 r2@ returns true when @r1@ and @r2@ have no elements in
--   common.
disjoint :: BoundedInt a => Range a -> Range a -> Bool
disjoint r1 r2 = isEmpty (r1 /\ r2)

-- | @rangeGap r1 r2@ returns a range of all the elements between @r1@ and
--   @r2@ including the boundary elements. If @r1@ and @r2@ have elements in
--   common the result is an empty range.
rangeGap :: BoundedInt a => Range a -> Range a -> Range a
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
rangeLess :: BoundedInt a => Range a -> Range a -> Bool
rangeLess r1 r2
  | isEmpty r1 || isEmpty r2 = True
rangeLess (Range _ u1) (Range l2 _) = u1 < l2

-- | @r1 \`rangeLessEq\` r2:@
--
-- Checks if all elements of @r1@ are less than or equal to all elements of
-- @r2@.
rangeLessEq :: BoundedInt a => Range a -> Range a -> Bool
rangeLessEq (Range _ u1) (Range l2 _) = u1 <= l2



--------------------------------------------------------------------------------
-- * Propagation
--------------------------------------------------------------------------------

-- | @rangeByRange ra rb@: Computes the range of the following set
--
-- > {x | a <- ra, b <- rb, x <- Range a b}
rangeByRange :: BoundedInt a => Range a -> Range a -> Range a
rangeByRange r1 r2
    | isEmpty r1 = emptyRange
    | isEmpty r2 = emptyRange
    | otherwise = Range (lowerBound r1) (upperBound r2)

-- | Implements 'fromInteger' as a 'singletonRange', and implements correct
-- range propagation for arithmetic operations.
instance BoundedInt a => Num (Range a)
  where
    fromInteger = singletonRange . fromInteger
    abs         = rangeAbs
    signum      = rangeSignum
    negate      = rangeNeg
    (+)         = rangeAdd
    (*)         = rangeMul
    (-)         = rangeSub

-- | Propagates range information through @abs@.
rangeAbs :: BoundedInt a => Range a -> Range a
rangeAbs = rangeOp $ \r -> case r of
    Range l u
      | isNatural  r -> r
      | r == singletonRange minBound -> r
      | minBound `inRange` r -> range minBound maxBound
      | isNegative r -> range (abs u) (abs l)
      | otherwise    -> range 0 (abs l `max` abs u)

-- | Propagates range information through 'signum'.
rangeSignum :: BoundedInt a => Range a -> Range a
rangeSignum = handleSign rangeSignumUnsigned rangeSignumSigned

-- | Signed case for 'rangeSignum'.
rangeSignumSigned :: BoundedInt a => Range a -> Range a
rangeSignumSigned = rangeOp sign
  where
    sign r
      | range (-1) 1 `isSubRangeOf` r = range (-1) 1
      | range (-1) 0 `isSubRangeOf` r = range (-1) 0
      | range 0 1    `isSubRangeOf` r = range 0 1
      | inRange 0 r                   = 0
      | isNatural r                   = 1
      | isNegative r                  = -1

-- | Unsigned case for 'rangeSignum'.
rangeSignumUnsigned :: BoundedInt a => Range a -> Range a
rangeSignumUnsigned = rangeOp sign
    where
      sign r
          | r == singletonRange 0 = r
          | not (0 `inRange` r)   = singletonRange 1
          | otherwise             = range 0 1

-- | Propagates range information through negation.
rangeNeg :: BoundedInt a => Range a -> Range a
rangeNeg = handleSign rangeNegUnsigned rangeNegSigned

-- | Unsigned case for 'rangeNeg'.
rangeNegUnsigned :: BoundedInt a => Range a -> Range a
rangeNegUnsigned (Range l u)
    | l == 0 && u /= 0 = fullRange
    | otherwise        = range (-u) (-l)
-- Code from Hacker's Delight

-- | Signed case for 'rangeNeg'.
rangeNegSigned :: BoundedInt a => Range a -> Range a
rangeNegSigned (Range l u)
    | l == minBound && u == minBound = singletonRange minBound
    | l == minBound                  = fullRange
    | otherwise                      = range (-u) (-l)
-- Code from Hacker's Delight

-- | Propagates range information through addition.
rangeAdd :: BoundedInt a => Range a -> Range a -> Range a
rangeAdd = handleSign rangeAddUnsigned rangeAddSigned

-- | Unsigned case for 'rangeAdd'.
rangeAddUnsigned :: BoundedInt a => Range a -> Range a -> Range a
rangeAddUnsigned (Range l1 u1) (Range l2 u2)
    | s >= l1 && t < u1 = fullRange
    | otherwise         = range s t
  where
    s = l1 + l2
    t = u1 + u2
-- Code from Hacker's Delight

-- | Signed case for 'rangeAdd'.
rangeAddSigned :: BoundedInt a => Range a -> Range a -> Range a
rangeAddSigned (Range a b) (Range c d)
    | (u .|. v) < 0 = fullRange
    | otherwise     = range s t
  where
    s = a + c
    t = b + d
    u = a .&. c .&. complement s .&. complement (b .&. d .&. complement t)
    v = ((a `xor` c) .|. complement (a `xor` s)) .&. (complement b .&. complement d .&. t)

-- | Propagates range information through subtraction.
rangeSub :: BoundedInt a => Range a -> Range a -> Range a
rangeSub = handleSign rangeSubUnsigned rangeSubSigned

-- | Unsigned case for 'rangeSub'.
rangeSubUnsigned :: BoundedInt a => Range a -> Range a -> Range a
rangeSubUnsigned (Range l1 u1) (Range l2 u2)
    | s > l1 && t <= u1 = fullRange
    | otherwise         = range s t
  where
    s = l1 - u2
    t = u1 - l2
  -- Note: This is more accurate than the default definition using 'negate',
  --       because 'negate' always overflows for unsigned numbers.
  -- Code from Hacker's Delight

rangeSubSigned :: BoundedInt a => Range a -> Range a -> Range a
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
subSat :: BoundedInt a => a -> a -> a
subSat a b = a - min a b

-- | Range propagation for 'subSat'
rangeSubSat :: BoundedInt a => Range a -> Range a -> Range a
rangeSubSat r1 r2 = Range
    (subSat (lowerBound r1) (upperBound r2))
    (subSat (upperBound r1) (lowerBound r2))

-- | Propagates range information through multiplication
rangeMul :: BoundedInt a => Range a -> Range a -> Range a
rangeMul = handleSign rangeMulUnsigned rangeMulSigned

-- | Signed case for 'rangeMul'.
rangeMulSigned :: forall a . BoundedInt a => Range a -> Range a -> Range a
rangeMulSigned r1 r2
    | r1 == singletonRange 0 || r2 == singletonRange 0 = singletonRange 0
    -- The following case is important because the 'maxAbs' function doesn't
    -- work for 'minBound' on signed numbers.
    | lowerBound r1 == minBound || lowerBound r2 == minBound
        = range minBound maxBound
    | bits (maxAbs r1) + bits (maxAbs r2) <= bitSize (undefined :: a) - 1
        = range (minimum [b1,b2,b3,b4]) (maximum [b1,b2,b3,b4])
    | otherwise = range minBound maxBound
  where maxAbs (Range l u) = max (abs l) (abs u)
        b1 = lowerBound r1 * lowerBound r2
        b2 = lowerBound r1 * upperBound r2
        b3 = upperBound r1 * lowerBound r2
        b4 = upperBound r1 * upperBound r2

-- | Unsigned case for 'rangeMul'.
rangeMulUnsigned :: forall a . BoundedInt a => Range a -> Range a -> Range a
rangeMulUnsigned r1 r2
    | bits (upperBound r1) + bits (upperBound r2)
      <= bitSize (undefined :: a)
        = mapMonotonic2 (*) r1 r2
    | otherwise = universal

-- | Returns the position of the highest bit set to 1. Counting starts at 1.
-- Beware! It doesn't terminate for negative numbers.
bits :: (Num b, Bits b) => b -> Int
bits b = loop b 0
    where loop 0 c = c
          loop n c = loop (n `shiftR` 1) (c+1)

-- | Propagates range information through exponentiation.
rangeExp :: BoundedInt a => Range a -> Range a -> Range a
rangeExp = handleSign rangeExpUnsigned rangeExpSigned

-- | Unsigned case for 'rangeExp'.
rangeExpUnsigned :: BoundedInt a => Range a -> Range a -> Range a
rangeExpUnsigned m@(Range l1 u1) e@(Range l2 u2)
    | toInteger (bits u1) * toInteger u2 > toInteger (bitSize l1) + 1 = universal
    | toInteger u1 ^ toInteger u2 > toInteger (maxBound `asTypeOf` l1) = universal
    | 0 `inRange` m && 0 `inRange` e = range 0 (max b1 b2)
    | otherwise = range b1 b2
  where b1 = l1 ^ l2
        b2 = u1 ^ u2

-- | Sigend case for 'rangeExp'
rangeExpSigned :: BoundedInt a => Range a -> Range a -> Range a
rangeExpSigned m _ | m == singletonRange (-1) = range (-1) 1
rangeExpSigned _ _ = universal

-- | Propagates range information through '.|.'.
rangeOr :: forall a . BoundedInt a => Range a -> Range a -> Range a
rangeOr = handleSign rangeOrUnsignedAccurate (\_ _ -> universal)

-- | Cheap and inaccurate range propagation for '.|.' on unsigned numbers.
rangeOrUnsignedCheap :: BoundedInt a => Range a -> Range a -> Range a
rangeOrUnsignedCheap (Range l1 u1) (Range l2 u2) =
    range (max l1 l2) (maxPlus u1 u2)
-- Code from Hacker's Delight.

-- | @a \`maxPlus\` b@ adds @a@ and @b@ but if the addition overflows then
--   'maxBound' is returned.
maxPlus :: BoundedInt a => a -> a -> a
maxPlus b d = if s < b then maxBound
              else s
  where s = b + d

-- | Accurate lower bound for '.|.' on unsigned numbers.
minOrUnsigned :: BoundedInt a => a -> a -> a -> a -> a
minOrUnsigned a b c d = loop (bit (bitSize a - 1))
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
maxOrUnsigned :: BoundedInt a => a -> a -> a -> a -> a
maxOrUnsigned a b c d = loop (bit (bitSize a - 1))
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

-- | Accurate range propagation through '.|.' for unsigned types.
rangeOrUnsignedAccurate :: BoundedInt a => Range a -> Range a -> Range a
rangeOrUnsignedAccurate (Range l1 u1) (Range l2 u2) =
    range (minOrUnsigned l1 u1 l2 u2) (maxOrUnsigned l1 u1 l2 u2)
-- Code from Hacker's Delight.

-- | Propagating range information through '.&.'.
rangeAnd :: forall a . BoundedInt a => Range a -> Range a -> Range a
rangeAnd = handleSign rangeAndUnsignedCheap (\_ _ -> universal)

-- | Cheap and inaccurate range propagation for '.&.' on unsigned numbers.
rangeAndUnsignedCheap :: BoundedInt a => Range a -> Range a -> Range a
rangeAndUnsignedCheap (Range _ u1) (Range _ u2) = range 0 (min u1 u2)
-- Code from Hacker's Delight.

-- | Propagating range information through 'xor'.
rangeXor :: forall a . BoundedInt a => Range a -> Range a -> Range a
rangeXor = handleSign rangeXorUnsigned  (\_ _ -> universal)

-- | Unsigned case for 'rangeXor'.
rangeXorUnsigned :: BoundedInt a => Range a -> Range a -> Range a
rangeXorUnsigned (Range _ u1) (Range _ u2) = range 0 (maxPlus u1 u2)
-- Code from Hacker's Delight.

-- | Propagating range information through 'shiftLU'.
rangeShiftLU :: (BoundedInt a, BoundedInt b) => Range a -> Range b -> Range a
rangeShiftLU = handleSign rangeShiftLUUnsigned (\_ _ -> universal)
-- TODO: improve accuracy

-- | Unsigned case for 'rangeShiftLU'.
rangeShiftLUUnsigned :: (Bounded a, Bits a, Integral a, Integral b)
                     => Range a -> Range b -> Range a
rangeShiftLUUnsigned (Range _ u1) (Range _ u2)
    | toInteger (bits u1) + fromIntegral u2 > toInteger (bitSize u1) = universal
rangeShiftLUUnsigned (Range l1 u1) (Range l2 u2)
    = range (shiftL l1 (fromIntegral l2)) (shiftL u1 (fromIntegral u2))

-- | Propagating range information through 'shiftRU'.
rangeShiftRU :: (BoundedInt a, BoundedInt b) => Range a -> Range b -> Range a
rangeShiftRU = handleSign rangeShiftRUUnsigned (\_ _ -> universal)
-- TODO: improve accuracy

-- | Unsigned case for 'rangeShiftRU'.
rangeShiftRUUnsigned :: (Num a, Bits a, Ord a, Bounded b, Integral b, Bits b)
                     => Range a -> Range b -> Range a
rangeShiftRUUnsigned (Range l1 u1) (Range l2 u2)
    = range (correctShiftRU l1 u2) (correctShiftRU u1 l2)

-- | This is a replacement fror Haskell's shiftR. If we carelessly use
--   Haskell's variant then we will get left shifts for very large shift values.
correctShiftRU :: (Num a, Bits a, BoundedInt b) => a -> b -> a
correctShiftRU _ i | i > fromIntegral (maxBound :: Int) = 0
correctShiftRU a i = shiftR a (fromIntegral i)

-- | Propagating range information through 'complement'
rangeComplement :: (Bits a, BoundedInt a) => Range a -> Range a
rangeComplement (Range l u) = range (complement l) (complement u)

-- | Propagates range information through 'max'.
rangeMax :: BoundedInt a => Range a -> Range a -> Range a
rangeMax r1 r2
    | isEmpty r1        = r2
    | isEmpty r2        = r1
    | r1 `rangeLess` r2 = r2
    | r2 `rangeLess` r1 = r1
    | otherwise         = mapMonotonic2 max r1 r2

-- | Analogous to 'rangeMax'
rangeMin :: BoundedInt a => Range a -> Range a -> Range a
rangeMin r1 r2
    | isEmpty r1        = r2
    | isEmpty r2        = r1
    | r1 `rangeLess` r2 = r1
    | r2 `rangeLess` r1 = r2
    | otherwise         = mapMonotonic2 min r1 r2

instance BoundedInt a => Ord (Range a)
  where
    compare = error "compare: I don't make sense for (Range a)"
    min = rangeMin
    max = rangeMax

-- | Propagates range information through 'mod'.
-- Note that we assume Haskell semantics for 'mod'.
rangeMod :: BoundedInt a => Range a -> Range a -> Range a
rangeMod d r
    | isSigned (lowerBound d) &&
      minBound `inRange` d && (-1) `inRange` r = fullRange
    | d `rangeLess` r && isNatural r && isNatural d = d
    | isNatural r = range 0 (pred (upperBound r))
    | r `rangeLess` d && isNeg r && isNeg d = d
    | isNeg r = range (succ (lowerBound r)) 0
    where
      isNeg = (`isSubRangeOf` negs)
      negs  = negativeRange \/ 0
rangeMod _ (Range l u) = Range (succ l) (pred u)

-- | Propagates range information through 'rem'.
-- Note that we assume Haskell semantics for 'rem'.
rangeRem :: BoundedInt a => Range a -> Range a -> Range a
rangeRem d r
    | isSigned (lowerBound d) &&
      minBound `inRange` d && (-1) `inRange` r = fullRange
    | d `rangeLessAbs` r && isNatural d = d
    | isNatural d = range 0 (pred (upperBound (abs r)))
    | d `absRangeLessAbs` r && isNeg d = d
    | isNeg d = range (negate (upperBound (abs r))) 0
    where
      isNeg = (`isSubRangeOf` negs)
      negs  = negativeRange \/ 0
rangeRem _ (Range l u)
    | abs l >= abs u || l == minBound = range (succ $ negate $ abs l) (predAbs l)
    | otherwise      = range (succ $ negate $ abs u) (predAbs u)

predAbs :: (Bounded a, Eq a, Num a, Enum a) => a -> a
predAbs l | l == minBound = abs (succ l)
          | otherwise     = pred (abs l)

-- | Propagates range information through 'div'
rangeDiv :: BoundedInt a => Range a -> Range a -> Range a
rangeDiv = handleSign rangeDivU (\_ _ -> universal)

-- | Unsigned case for 'rangeDiv'
rangeDivU :: BoundedInt a => Range a -> Range a -> Range a
rangeDivU (Range _  _ ) (Range l2 u2) | l2 == 0 || u2 == 0 = universal
rangeDivU (Range l1 u1) (Range l2 u2) = Range (l1 `quot` u2) (u1 `quot`l2)

-- | Propagates range information through 'quot'.
rangeQuot :: BoundedInt a => Range a -> Range a -> Range a
rangeQuot = handleSign rangeQuotU (\_ _ -> universal)

-- | Unsigned case for 'rangeQuot'.
rangeQuotU :: BoundedInt a => Range a -> Range a -> Range a
rangeQuotU (Range _  _ ) (Range l2 u2) | l2 == 0 || u2 == 0 = universal
rangeQuotU (Range l1 u1) (Range l2 u2) = Range (l1 `quot` u2) (u1 `quot` l2)

-- | Writing @d \`rangeLess\` abs r@ doesn't mean what you think it does because
-- 'r' may contain minBound which doesn't have a positive representation.
-- Instead, this function should be used.
rangeLessAbs :: (Bounded a, Integral a, Bits a)
             => Range a -> Range a -> Bool
rangeLessAbs d r
    | r == singletonRange minBound
        = lowerBound d /= minBound
    | lowerBound r == minBound
        = d `rangeLess` abs (range (succ (lowerBound r)) (upperBound r))
    | otherwise = d `rangeLess` abs r

-- | Similar to 'rangeLessAbs' but replaces the expression
--   @abs d \`rangeLess\` abs r@ instead.
absRangeLessAbs :: (Bounded a, Integral a, Bits a)
                => Range a -> Range a -> Bool
absRangeLessAbs d r
    | lowerBound d == minBound = False
    | otherwise = abs d `rangeLessAbs` r


--------------------------------------------------------------------------------
-- * Products of ranges
--------------------------------------------------------------------------------

{- These functions are used to compute the ranges of DefaultInt and
   DefaultWord. The size information of these two types is the union of the
   sizes for all possible IntX/WordX that we support as defaults -}

liftR :: (BoundedInt b, BoundedInt c, BoundedInt d) =>
         (forall a. (BoundedInt a) => Range a) -> (Range b,Range c,Range d)
liftR r = (r,r,r)

binopR :: (BoundedInt a, BoundedInt b, BoundedInt c) =>
          (forall d. BoundedInt d => Range d -> Range d -> Range d) ->
          (Range a, Range b, Range c) ->
          (Range a, Range b, Range c) ->
          (Range a, Range b, Range c)
binopR bop (a1,b1,c1) (a2,b2,c2)
    = (bop a1 a2, bop b1 b2, bop c1 c2)


mapR :: (BoundedInt a, BoundedInt b, BoundedInt c) =>
        (forall d . BoundedInt d => Range d -> Range d) ->
        (Range a, Range b, Range c) ->
        (Range a, Range b, Range c)
mapR f (a,b,c) = (f a, f b, f c)

approx :: (BoundedInt a, BoundedInt b, BoundedInt c, BoundedInt d)
          => (Range a, Range b, Range c) -> Range d
approx (r1,r2,r3) | isFull r1 || isFull r2 || isFull r3
                         = fullRange
approx (r1,r2,r3) = mapMonotonic fromIntegral r1 \/
                    mapMonotonic fromIntegral r2 \/
                    mapMonotonic fromIntegral r3

instance (BoundedInt a, BoundedInt b, BoundedInt c) =>
    Num (Range a,Range b,Range c) where
  (+)           = binopR (+)
  (-)           = binopR (-)
  (*)           = binopR (*)
  signum        = mapR rangeSignum
  fromInteger i = liftR (fromInteger i)
  abs           = mapR abs
  negate        = mapR negate


