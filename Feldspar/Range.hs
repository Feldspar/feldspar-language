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

{-# LANGUAGE CPP                  #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Bounded integer ranges

module Feldspar.Range where



-- TODO This module should be broken up into smaller pieces. Since most
-- functions seem to be useful not only for Feldspar, it would probably be good
-- to make a separate package. In any case, the modules should go under
-- `Data.Range`. If there are functions that are very Feldspar specific, these
-- should go into `Feldspar.Core.Constructs.*` (or whereever suitable).



import Data.Bits
import Data.Int
import Data.Word
import Data.Typeable
import System.Random -- Should maybe be exported from QuickCheck
import Test.QuickCheck hiding ((.&.))
import qualified Test.QuickCheck as QC

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
rangeAddSigned (Range l1 u1) (Range l2 u2)
    | (u .|. v) < 0 = fullRange
    | otherwise     = range s t
  where
    s = l1 + l2
    t = u1 + u2
    u = l1 .&. l2 .&. complement s .&.
        complement (u1 .&. u2 .&. complement t)
    v = ((xor l1 l2) .|. complement (xor l1 s)) .&.
        (complement u1 .&. complement u2 .&. t)
-- Code from Hacker's Delight

-- | Propagates range information through subtraction.
rangeSub :: BoundedInt a => Range a -> Range a -> Range a
rangeSub = handleSign rangeSubUnsigned (-)

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
bits :: Bits b => b -> Int
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
rangeExpSigned m e | m == singletonRange (-1) = range (-1) 1
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
maxPlus b d = if sum < b then maxBound
              else sum
  where sum = b + d

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
                    else let temp = (d - m) .|. (m - 1)
                         in if temp >= c
                            then b .|. temp
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
rangeShiftRUUnsigned :: (Bits a, Ord a, Bounded b, Integral b, Bits b)
                     => Range a -> Range b -> Range a
rangeShiftRUUnsigned (Range l1 u1) (Range l2 u2)
    = range (correctShiftRU l1 u2) (correctShiftRU u1 l2)

-- | This is a replacement fror Haskell's shiftR. If we carelessly use
--   Haskell's variant then we will get left shifts for very large shift values.
correctShiftRU :: (Bits a, BoundedInt b) => a -> b -> a
correctShiftRU _ i | i > fromIntegral (maxBound :: Int) = 0
correctShiftRU a i = shiftR a (fromIntegral i)

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

rangeDiv :: BoundedInt a => Range a -> Range a -> Range a
rangeDiv = handleSign rangeDivU (\_ _ -> universal)

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



--------------------------------------------------------------------------------
-- * Testing
--------------------------------------------------------------------------------

instance (BoundedInt a, Arbitrary a) => Arbitrary (Range a)
  where
    arbitrary = do
      [bound1,bound2] <- vectorOf 2 $ oneof
                         [ arbitrary
                         , elements [minBound,-1,0,1,maxBound]]
      frequency
                [ (10, return $
                     Range (min bound1 bound2) (max bound1 bound2))
                , (1 , return $
                     Range (max bound1 bound2) (min bound1 bound2)) -- Empty
                , (1 , return $
                     Range bound1 bound1)  -- Singleton
                ]

    shrink (Range x y) =
      [ Range x' y | x' <- shrink x ] ++
      [ Range x y' | y' <- shrink y ]

#if __GLASGOW_HASKELL__ < 704
instance Random Word32 where
  random g = (fromIntegral i,g')
   where (i :: Int,g') = random g
  randomR (l,u) g = (fromIntegral i,g')
    where (i :: Integer, g') = randomR (fromIntegral l,fromIntegral u) g

instance Random Int8 where
  random g = (fromIntegral i,g')
   where (i :: Int,g') = random g
  randomR (l,u) g = (fromIntegral i,g')
    where (i :: Integer, g') = randomR (fromIntegral l,fromIntegral u) g

instance Random Word8 where
  random g = (fromIntegral i,g')
   where (i :: Int,g') = random g
  randomR (l,u) g = (fromIntegral i,g')
    where (i :: Integer, g') = randomR (fromIntegral l,fromIntegral u) g
#endif

fromRange :: BoundedInt a => Random a => Range a -> Gen a
fromRange r
    | isEmpty r = error "fromRange: empty range"
    | otherwise = choose (lowerBound r, upperBound r)

rangeTy :: Range t -> t -> Range t
rangeTy r _ = r

-- | Applies a (monadic) function to all the types we are interested in testing
-- with for Feldspar.
--
-- Example usage: 'atAllTypes (quickCheck . prop_mul)'
atAllTypes :: (Monad m) =>
              (forall t . (BoundedInt t, Random t, Arbitrary t, Typeable t) =>
                      t -> m a)
                  -> m ()
atAllTypes test = sequence_ [test (undefined :: Int)
                            ,test (undefined :: Int8)
                            ,test (undefined :: Word32)
                            ,test (undefined :: Word8)
                            ]

-- | Test if a operation is "strict" wrt. empty ranges
prop_isStrict1 t op ra = isEmpty ra ==> isEmpty (op ra)
  where _ = ra `rangeTy` t

-- | Test if an operation is "strict" wrt. empty ranges
prop_isStrict2 t op ra rb =
    isEmpty ra || isEmpty rb ==> isEmpty (op ra rb)
  where _ = ra `rangeTy` t

-- TODO Think about strictness of range operations (in the sense of `isStrict1`
-- and `isStrict2`). Probably all range propagation operations should be strict,
-- but many of them are currently not:
--
--     *Feldspar.Range> quickCheck (prop_isStrict2 (undefined :: Int) (+))
--     *** Failed! Falsifiable (after 1 test and 1 shrink):
--     Range {lowerBound = 0, upperBound = 1}
--     Range {lowerBound = 1, upperBound = 0}



--------------------------------------------------------------------------------
-- ** Lattice operations
--------------------------------------------------------------------------------

prop_empty t = isEmpty (emptyRange `rangeTy` t)

prop_full t = isFull (fullRange `rangeTy` t)

prop_isEmpty t r = isEmpty r ==> (upperBound r < lowerBound (r `rangeTy` t))

prop_singletonRange t a = isSingleton (singletonRange (a `asTypeOf` t))

prop_singletonSize t r = isSingleton (r `rangeTy` t) ==> (rangeSize r == 1)

prop_emptySubRange1 t r1 r2 =
    isEmpty (r1 `rangeTy` t) ==> (not (isEmpty r2) ==>
                                      not (r2 `isSubRangeOf` r1))

prop_emptySubRange2 t r1 r2 =
    isEmpty (r1 `rangeTy` t) ==> (not (isEmpty r2) ==> (r1 `isSubRangeOf` r2))

prop_rangeGap t r1 r2 =
    (isEmpty gap1 && isEmpty gap2) || (gap1 == gap2)
  where
    gap1 = rangeGap r1 r2
    gap2 = rangeGap r2 r1
    _    = r1 `rangeTy` t

prop_union1 t x r1 r2 =
    ((x `inRange` r1) || (x `inRange` r2)) ==> (x `inRange` (r1\/r2))
  where _ = x `asTypeOf` t

prop_union2 t x r1 r2 =
    (x `inRange` (r1\/r2)) ==>
        ((x `inRange` r1) || (x `inRange` r2) || (x `inRange` rangeGap r1 r2))
  where _ = x `asTypeOf` t

prop_union3 t r1 r2 = (r1 `rangeTy` t) `isSubRangeOf` (r1\/r2)
prop_union4 t r1 r2 = (r2 `rangeTy` t) `isSubRangeOf` (r1\/r2)


prop_intersect1 t x r1 r2 =
    ((x `inRange` r1) && (x `inRange` r2)) ==> (x `inRange` (r1/\r2))
  where _ = x `asTypeOf` t
prop_intersect2 t x r1 r2 =
    (x `inRange` (r1/\r2)) ==> ((x `inRange` r1) && (x `inRange` r2))
  where _ = x `asTypeOf` t

prop_intersect3 t r1 r2 = (r1/\r2) `isSubRangeOf` (r1 `rangeTy` t)
prop_intersect4 t r1 r2 = (r1/\r2) `isSubRangeOf` (r2 `rangeTy` t)

prop_intersect5 t r1 r2 =
    isEmpty r1 || isEmpty r2 ==> isEmpty (r1/\r2)
  where _ = r1 `rangeTy` t

prop_disjoint t x r1 r2 =
    disjoint r1 r2 ==> (x `inRange` r1) ==> not (x `inRange` r2)
  where _ = x `asTypeOf` t


prop_rangeLess1 t r1 r2 =
    rangeLess r1 r2 ==> disjoint r1 (r2 `rangeTy` t)

prop_rangeLess2 t r1 r2 =
    not (isEmpty r1) && not (isEmpty r2) ==>
    forAll (fromRange r1) $ \x ->
    forAll (fromRange r2) $ \y ->
    rangeLess r1 r2 ==> x < y
  where _ = r1 `rangeTy` t

prop_rangeLessEq t r1 r2 =
    not (isEmpty r1) && not (isEmpty r2) ==>
    forAll (fromRange r1) $ \x ->
    forAll (fromRange r2) $ \y ->
    rangeLessEq r1 r2 ==> x <= y
  where _ = r1 `rangeTy` t


--------------------------------------------------------------------------------
-- ** Propagation
--------------------------------------------------------------------------------

prop_propagation1 :: (Show t, BoundedInt t, Random t) =>
                     t -> (forall a . Num a => a -> a) -> Range t -> Property
prop_propagation1 _ op r =
    not (isEmpty r) ==>
    forAll (fromRange r) $ \x ->
    op x `inRange` op r

-- | This function is useful for range propagation functions like
-- 'rangeMax', 'rangeMod' etc.
-- It takes two ranges, picks an element out of either ranges and
-- checks if applying the operation to the individual elements is in
-- the resulting range after range propagation.
--
-- The third argument is a precondition that is satisfied before the test is
-- run. A good example is to make sure that the second argument is non-zero
-- when testing division.
rangePropagationSafetyPre :: (Show t, Random t, BoundedInt t, BoundedInt a) =>
    t ->
    (t -> t -> a) -> (Range t -> Range t -> Range a) ->
    (t -> t -> Bool) ->
    Range t -> Range t -> Property
rangePropagationSafetyPre _ op rop pre r1 r2 =
    not (isEmpty r1) && not (isEmpty r2) ==>
    forAll (fromRange r1) $ \v1 ->
    forAll (fromRange r2) $ \v2 ->
        pre v1 v2 ==>
        op v1 v2 `inRange` rop r1 r2

rangePropagationSafetyPre2 ::
    (Show t, Show t2, Random t, BoundedInt t, Random t2, BoundedInt t2, BoundedInt a) =>
    t -> t2 ->
    (t -> t2 -> a) -> (Range t -> Range t2 -> Range a) ->
    (t -> t2 -> Bool) ->
    Range t -> Range t2 -> Property
rangePropagationSafetyPre2 _ _ op rop pre r1 r2 =
    not (isEmpty r1) && not (isEmpty r2) ==>
    forAll (fromRange r1) $ \v1 ->
    forAll (fromRange r2) $ \v2 ->
        pre v1 v2 ==>
        op v1 v2 `inRange` rop r1 r2

rangePropagationSafety t op rop = rangePropagationSafetyPre t op rop noPre
  where
    noPre _ _ = True

rangePropSafety1 t op rop ran =
    not (isEmpty ran) ==>
    forAll (fromRange ran) $ \val ->
        op val `inRange` rop ran
  where _ = ran `rangeTy` t

prop_propagation2
    :: (Show t, BoundedInt t, Random t) => t -> (forall a . Num a => a -> a -> a)
    -> Range t -> Range t -> Property
prop_propagation2 t op = rangePropagationSafety t op op

prop_rangeByRange1 t ra rb =
    forAll (fromRange ra) $ \a ->
    forAll (fromRange rb) $ \b ->
    forAll (fromRange (Range a b)) $ \x ->
        not (isEmpty ra) && not (isEmpty rb) && not (isEmpty (Range a b)) ==>
          inRange x (rangeByRange ra rb)
  where _ = ra `rangeTy` t

prop_rangeByRange2 t = prop_isStrict2 t rangeByRange

prop_fromInteger t a = isSingleton (fromInteger a `rangeTy` t)

prop_abs  t = prop_propagation1 t abs
prop_sign t = prop_propagation1 t signum
prop_neg  t = prop_propagation1 t negate
prop_add  t = prop_propagation2 t (+)
prop_sub  t = prop_propagation2 t (-)
prop_mul  t = prop_propagation2 t (*)

prop_exp  t = rangePropagationSafetyPre t (^) rangeExp (\_ e -> e >= 0)

prop_mulU t = rangePropagationSafety t (*) rangeMulUnsigned

prop_subSat t = rangePropagationSafety t subSat rangeSubSat

prop_isNegative t r =
    not (isEmpty r) && (r /= Range minBound minBound) ==>
        isNegative r ==> not (isNegative $ negate r)
  where _ = rangeTy r t

prop_abs2 t r =
    lowerBound r /= (minBound `asTypeOf` t) ==> isNatural (abs r)

prop_or t = rangePropagationSafety t (.|.) rangeOr

prop_and t = rangePropagationSafety t (.&.) rangeAnd

prop_xor t = rangePropagationSafety t xor rangeXor

prop_shiftLU t1 t2
    = rangePropagationSafetyPre2 t1 t2 fixShiftL rangeShiftLU (\_ _ -> True)
  where fixShiftL a b = shiftL a (fromIntegral b)

prop_shiftRU t1 t2
    = rangePropagationSafetyPre2 t1 t2 fixShiftR rangeShiftRU (\_ _ -> True)
  where fixShiftR = correctShiftRU

prop_rangeMax1 t r1 = rangeMax r1 r1 == (r1 `rangeTy` t)

prop_rangeMax2 t r1 r2 =
    not (isEmpty r1) && not (isEmpty r2) ==>
    upperBound r1 <= upperBound max && upperBound r2 <= upperBound max
    where
      max = rangeMax r1 (r2 `rangeTy` t)

prop_rangeMax3 t r1 r2 =
    not (isEmpty r1) && not (isEmpty r2) ==>
  lowerBound (rangeMax r1 r2) == max (lowerBound r1) (lowerBound r2)
  where _ = r1 `rangeTy` t

prop_rangeMax4 t r1 r2 =
    not (isEmpty r1) && not (isEmpty r2) ==>
    rangeMax r1 r2 == rangeMax r2 r1
  where _ = r1 `rangeTy` t

prop_rangeMax5 t r1 r2 =
    (isEmpty r1 && not (isEmpty r2) ==>
    rangeMax r1 r2 == r2)
    QC..&.
    (isEmpty r2 && not (isEmpty r1) ==>
    rangeMax r1 r2 == r1)
  where _ = r1 `rangeTy` t

prop_rangeMax6 t v1 v2 =
    max v1 v2 `inRange` rangeMax (singletonRange v1) (singletonRange v2)
  where _ = v1 `asTypeOf` t

prop_rangeMax7 a = rangePropagationSafety a max rangeMax

prop_rangeMin1 t r1 = rangeMin r1 r1 == (r1 `rangeTy` t)

prop_rangeMin2 t r1 r2 =
    not (isEmpty r1) && not (isEmpty r2) ==>
    lowerBound min <= lowerBound r1 && lowerBound min <= lowerBound r2
    where
      min = rangeMin r1 (r2 `rangeTy` t)

prop_rangeMin3 t r1 r2 =
    not (isEmpty r1) && not (isEmpty r2) ==>
  upperBound (rangeMin r1 r2) == min (upperBound r1) (upperBound r2)
  where _ = r1 `rangeTy` t

prop_rangeMin4 t r1 r2 =
    not (isEmpty r1) && not (isEmpty r2) ==>
    rangeMin r1 r2 == rangeMin r2 r1
  where _ = r1 `rangeTy` t

prop_rangeMin5 t r1 r2 =
    (isEmpty r1 && not (isEmpty r2) ==>
    rangeMin r1 r2 == r2)
    QC..&.
    (isEmpty r2 && not (isEmpty r1) ==>
    rangeMin r1 r2 == r1)
  where _ = r1 `rangeTy` t

prop_rangeMin6 t v1 v2 =
    min v1 v2 `inRange` rangeMin (singletonRange v1) (singletonRange v2)
  where _ = v1 `asTypeOf` t

prop_rangeMin7 t = rangePropagationSafety t min rangeMin

prop_rangeMod1 t v1 v2 =
    v2 /= 0 ==>
    mod v1 v2 `inRange` rangeMod (singletonRange v1) (singletonRange v2)
  where _ = v1 `asTypeOf` t

prop_rangeMod2 t =
    rangePropagationSafetyPre t mod rangeMod divPre

prop_rangeMod3 t =
        isFull $ rangeMod (singletonRange (minBound `asTypeOf` t))
                          (singletonRange (-1))

prop_rangeRem t =
    rangePropagationSafetyPre t rem rangeRem divPre

prop_rangeRem1 t =
        isFull $ rangeRem (singletonRange (minBound `asTypeOf` t))
                          (singletonRange (-1))

prop_rangeQuot t =
    rangePropagationSafetyPre t quot rangeQuot divPre

prop_rangeQuot1 t =
        isFull $ rangeQuot (singletonRange (minBound `asTypeOf` t))
                           (singletonRange (-1))

-- | Precondition for division like operators.
--   Avoids division by zero and arithmetic overflow.
divPre v1 v2 = v2 /= 0 && not (v1 == minBound && v2 == (-1))

