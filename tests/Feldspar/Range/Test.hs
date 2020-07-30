{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ConstraintKinds #-}

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

module Feldspar.Range.Test ( rangeTests ) where

import Feldspar.Range
import System.Random -- Should maybe be exported from QuickCheck
import Test.QuickCheck hiding ((.&.))
import qualified Test.QuickCheck as QC
import Test.Tasty
import Test.Tasty.QuickCheck hiding ((.&.))

import Control.Applicative
import Data.Bits
import Data.Int
import Data.Word
import Data.Typeable

import Feldspar.Lattice

import Debug.Trace

rangeTests = [ testGroup "Range Int8"   $ typedTestsSigned   "Int8"   (undefined :: Int8)
        , testGroup "Range Int64"  $ typedTestsSigned   "Int64"  (undefined :: Int64)
        , testGroup "Range Word8"  $ typedTestsUnsigned "Word8"  (undefined :: Word8)
        , testGroup "Range Word32" $ typedTestsUnsigned "Word32" (undefined :: Word32)
        , testGroup "Range Bool"   $ typedTestsEnum     "Bool"   (undefined :: Bool)
        , testGroup "Range Int8, Range Int8"   $ typedTestsTwo "Int8, Int8"   (undefined :: Int8)  (undefined :: Int8)
        , testGroup "Range Word8, Range Word8" $ typedTestsTwo "Word8, Word8" (undefined :: Word8) (undefined :: Word8)
        ]

typedTests name typ = typedTestsEnum name typ ++ typedTestsNum name typ

-- | Tests where Enum a => Range a.
typedTestsEnum name typ =
    [ testProperty (unwords ["prop_empty"          , name]) (prop_empty typ)
    , testProperty (unwords ["prop_full"           , name]) (prop_full typ)
    , testProperty (unwords ["prop_isEmpty"        , name]) (prop_isEmpty typ)
    , testProperty (unwords ["prop_singletonRange" , name]) (prop_singletonRange typ)
    , testProperty (unwords ["prop_singletonSize"  , name]) (prop_singletonSize typ)
    , testProperty (unwords ["prop_emptySubRange1" , name]) (prop_emptySubRange1 typ)
    , testProperty (unwords ["prop_emptySubRange2" , name]) (prop_emptySubRange2 typ)
    , testProperty (unwords ["prop_rangeGap"       , name]) (prop_rangeGap typ)
    , testProperty (unwords ["prop_union1"         , name]) (prop_union1 typ)
    , testProperty (unwords ["prop_union2"         , name]) (prop_union2 typ)
    , testProperty (unwords ["prop_union3"         , name]) (prop_union3 typ)
    , testProperty (unwords ["prop_union4"         , name]) (prop_union4 typ)
    , testProperty (unwords ["prop_intersect1"     , name]) (prop_intersect1 typ)
    , testProperty (unwords ["prop_intersect2"     , name]) (prop_intersect2 typ)
    , testProperty (unwords ["prop_intersect3"     , name]) (prop_intersect3 typ)
    , testProperty (unwords ["prop_intersect4"     , name]) (prop_intersect4 typ)
    , testProperty (unwords ["prop_disjoint"       , name]) (prop_disjoint typ)
    , testProperty (unwords ["prop_rangeLess1"     , name]) (prop_rangeLess1 typ)
    , testProperty (unwords ["prop_rangeLessEq"    , name]) (prop_rangeLessEq typ)
    , testProperty (unwords ["prop_rangeByRange1"  , name]) (prop_rangeByRange1 typ)]

-- | Tests where Num a => Range a.
typedTestsNum name typ =
    [ -- FIXME: Move intersect5, rangeLess2 and rangeByRange2 to typedTestsEnum.
      --        The tests are here to enable partial test coverage of Bool.
      testProperty (unwords ["prop_intersect5"     , name]) (prop_intersect5 typ)
    , testProperty (unwords ["prop_rangeLess2"     , name]) (prop_rangeLess2 typ)
    , testProperty (unwords ["prop_rangeByRange2"  , name]) (prop_rangeByRange2 typ)
    , testProperty (unwords ["prop_fromInteger"    , name]) (prop_fromInteger typ)
    , testProperty (unwords ["prop_abs"            , name]) (prop_abs typ)
    , testProperty (unwords ["prop_sign"           , name]) (prop_sign typ)
    , testProperty (unwords ["prop_neg"            , name]) (prop_neg typ)
    , testProperty (unwords ["prop_add"            , name]) (prop_add typ)
    , testProperty (unwords ["prop_sub"            , name]) (prop_sub typ)
    , testProperty (unwords ["prop_mul"            , name]) (prop_mul typ)
    , testProperty (unwords ["prop_exp"            , name]) (prop_exp typ)
    , testProperty (unwords ["prop_abs2"           , name]) (prop_abs2 typ)
    , testProperty (unwords ["prop_or"             , name]) (prop_or typ)
    , testProperty (unwords ["prop_and"            , name]) (prop_and typ)
    , testProperty (unwords ["prop_xor"            , name]) (prop_xor typ)
    , testProperty (unwords ["prop_rangeMax1"      , name]) (prop_rangeMax1 typ)
    , testProperty (unwords ["prop_rangeMax2"      , name]) (prop_rangeMax2 typ)
    , testProperty (unwords ["prop_rangeMax3"      , name]) (prop_rangeMax3 typ)
    , testProperty (unwords ["prop_rangeMax4"      , name]) (prop_rangeMax4 typ)
    , testProperty (unwords ["prop_rangeMax5_1"    , name]) (prop_rangeMax5_1 typ)
    , testProperty (unwords ["prop_rangeMax5_2"    , name]) (prop_rangeMax5_2 typ)
    , testProperty (unwords ["prop_rangeMax6"      , name]) (prop_rangeMax6 typ)
    , testProperty (unwords ["prop_rangeMax7"      , name]) (prop_rangeMax7 typ)
    , testProperty (unwords ["prop_rangeMin1"      , name]) (prop_rangeMin1 typ)
    , testProperty (unwords ["prop_rangeMin2"      , name]) (prop_rangeMin2 typ)
    , testProperty (unwords ["prop_rangeMin3"      , name]) (prop_rangeMin3 typ)
    , testProperty (unwords ["prop_rangeMin4"      , name]) (prop_rangeMin4 typ)
    , testProperty (unwords ["prop_rangeMin5_1"    , name]) (prop_rangeMin5_1 typ)
    , testProperty (unwords ["prop_rangeMin5_2"    , name]) (prop_rangeMin5_2 typ)
    , testProperty (unwords ["prop_rangeMin6"      , name]) (prop_rangeMin6 typ)
    , testProperty (unwords ["prop_rangeMin7"      , name]) (prop_rangeMin7 typ)
    , testProperty (unwords ["prop_rangeMod1"      , name]) (prop_rangeMod1 typ)
    , testProperty (unwords ["prop_rangeMod2"      , name]) (prop_rangeMod2 typ)
    , testProperty (unwords ["prop_rangeRem"       , name]) (prop_rangeRem typ)
    , testProperty (unwords ["prop_rangeQuot"      , name]) (prop_rangeQuot typ)
    ]

typedTestsUnsigned name typ = typedTests name typ ++
    [ testProperty (unwords ["prop_mulU"           , name]) (prop_mulU typ)
    , testProperty (unwords ["prop_subSat"         , name]) (prop_subSat typ)
    ]

typedTestsSigned name typ = typedTests name typ ++
    [ testProperty (unwords ["prop_isNegative"     , name]) (prop_isNegative typ)
    , testProperty (unwords ["prop_rangeMod3"      , name]) (prop_rangeMod3 typ)
    , testProperty (unwords ["prop_rangeRem1"      , name]) (prop_rangeRem1 typ)
    , testProperty (unwords ["prop_rangeQuot1"     , name]) (prop_rangeQuot1 typ)
    ]

typedTestsTwo name t1 t2 =
    [ testProperty (unwords ["prop_shiftLU"        , name]) (prop_shiftLU t1 t2)
    , testProperty (unwords ["prop_shiftRU"        , name]) (prop_shiftRU t1 t2)
    ]

--------------------------------------------------------------------------------
-- * Testing
--------------------------------------------------------------------------------

instance (Bounded a, Ord a, Enum a, Arbitrary a) => Arbitrary (Range a)
  where
    arbitrary = do
      tmp <- vectorOf 2 $ oneof
                         [ arbitrary
                         , elements $ [minBound,toEnum 0,toEnum 1,maxBound]
                            ++ [toEnum (-1) | fromEnum (minBound :: a) < 0]]
      case tmp of
        [bound1,bound2] -> frequency
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

newtype EmptyRange a = EmptyRange {getEmpty :: Range a}
  deriving Eq

deriving instance (Eq a, Lattice (Range a), Show a) => Show (EmptyRange a)

instance (Arbitrary a, Random a, Ord a, Bounded a) => Arbitrary (EmptyRange a) where
    arbitrary = do
      l <- arbitrary `suchThat` (>(minBound :: a))
      return $ EmptyRange $ Range l minBound

newtype NonEmptyRange a = NonEmptyRange {getNonEmpty :: Range a}
  deriving Eq

deriving instance (Eq a, Lattice (Range a), Show a) => Show (NonEmptyRange a)

instance (Arbitrary a, Random a, Ord a, Bounded a) => Arbitrary (NonEmptyRange a) where
    arbitrary = do
      l <- arbitrary `suchThat` (<(maxBound :: a))
      u <- choose (l,maxBound)
      return $ NonEmptyRange $ Range l u

-- | Generate a range guaranteed to include the element
aroundRange :: (Bounded a, Random a) => a -> Gen (Range a)
aroundRange x = do
    l <- choose (minBound,x)
    u <- choose (x,maxBound)
    return $ Range l u

disjointRanges :: (Arbitrary a, Enum a, Ord a, Bounded a, Random a) => Gen (Range a, Range a)
disjointRanges = do
    NonEmptyRange r <- arbitrary
    u1 <- choose (minBound,lowerBound r)
    l1 <- choose (minBound,u1)
    -- The if avoids returning (Range minBound minBound, Range minBound x).
    l2 <- choose (if u1 == upperBound r then maxBound else upperBound r,maxBound)
    u2 <- choose (l2,maxBound)
    return (Range l1 u1, Range l2 u2)

prop_disjointGen t = forAll disjointRanges $ \(r1,r2) -> disjoint r1 (r2 `rangeTy`t)

fromRange :: (Bounded a, Ord a, Random a) => Range a -> Gen a
fromRange r
    | isEmpty r = error "fromRange: empty range"
    | otherwise = frequency [(1,return (lowerBound r))
                            ,(1,return (upperBound r))
                            ,(1,choose (lowerBound r, upperBound r))
                            ]

rangeTy :: Range t -> t -> Range t
rangeTy r _ = r

-- | Applies a (monadic) function to all the types we are interested in testing
-- with for Feldspar.
--
-- Example usage: 'atAllTypes (quickCheck . prop_mul)'
atAllTypes :: (Monad m) =>
              (forall t . (Show t, Bounded t, Random t, Arbitrary t, Typeable t) =>
                      t -> m a)
                  -> m ()
atAllTypes test = sequence_ [test (undefined :: Int)
                            ,test (undefined :: Int8)
                            ,test (undefined :: Word32)
                            ,test (undefined :: Word8)
                            ]

-- | Test if a operation is "strict" wrt. empty ranges
prop_isStrict1 t op (EmptyRange ra) = isEmpty (op ra)
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

prop_isEmpty t (EmptyRange r) = isEmpty (r `rangeTy` t)

prop_singletonRange t a = isSingleton (singletonRange (a `asTypeOf` t))

prop_singletonSize t r = isSingleton (r `rangeTy` t) ==> (rangeSize r == 1)

prop_emptySubRange1 t (EmptyRange r1) (NonEmptyRange r2) =
    not (r2 `isSubRangeOf` (r1 `rangeTy` t))

prop_emptySubRange2 t (EmptyRange r1) (NonEmptyRange r2) =
    r1 `isSubRangeOf` (r2 `rangeTy` t)

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


prop_intersect1 t x = forAll (aroundRange x) $ \r1 ->
                        forAll (aroundRange x) $ \r2 ->
                           x `inRange` (r1/\r2)
  where _ = x `asTypeOf` t
prop_intersect2 t x =
    forAll (aroundRange x) $ \r1 ->
      forAll (aroundRange x) $ \r2 ->
        (x `inRange` (r1/\r2)) ==> ((x `inRange` r1) && (x `inRange` r2))
  where _ = x `asTypeOf` t

prop_intersect3 t r1 r2 = (r1/\r2) `isSubRangeOf` (r1 `rangeTy` t)
prop_intersect4 t r1 r2 = (r1/\r2) `isSubRangeOf` (r2 `rangeTy` t)

prop_intersect5 t r1 r2 =
    isEmpty r1 || isEmpty r2 ==> isEmpty (r1/\r2)
  where _ = r1 `rangeTy` t

prop_disjoint t = forAll disjointRanges $ \(r1,r2) ->
                    forAll (fromRange r1) $ \x ->
                      not (x `inRange` (r2 `rangeTy` t))


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

prop_propagation1 :: (Show t, Bounded t, Ord t, Num t, Num (Range t), Random t) =>
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
rangePropagationSafetyPre :: (Show t, Bounded t, Random t, Ord t, Ord a) =>
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
    (Show t, Show t2, Random t, Bounded t, Ord t, Random t2, Bounded t2, Ord t2, Bounded a, Ord a) =>
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
    :: (Show t, Bounded t, Ord t, Num t, Num (Range t), Random t) => t -> (forall a . Num a => a -> a -> a)
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
    = rangePropagationSafetyPre2 t1 t2 fixShiftL rangeShiftLU (\_ n -> n >= 0)
  where fixShiftL a b = shiftL a (fromIntegral b)

prop_shiftRU t1 t2
    = rangePropagationSafetyPre2 t1 t2 fixShiftR rangeShiftRU (\_ n -> n >= 0)
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

prop_rangeMax5_1 t (EmptyRange r1) (NonEmptyRange r2) =
    rangeMax r1 r2 == (r2 `rangeTy` t)
prop_rangeMax5_2 t (NonEmptyRange r1) (EmptyRange r2) =
    rangeMax r1 r2 == (r1 `rangeTy` t)

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

prop_rangeMin5_1 t (EmptyRange r1) (NonEmptyRange r2) =
    rangeMin r1 r2 == (r2 `rangeTy` t)
prop_rangeMin5_2 t (NonEmptyRange r1) (EmptyRange r2) =
    rangeMin r1 r2 == (r1 `rangeTy` t)

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
