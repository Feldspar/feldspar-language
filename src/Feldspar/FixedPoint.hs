{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

{-# LANGUAGE UndecidableInstances #-}
module Feldspar.FixedPoint
    ( Fix(..), Fixable(..)
    , freezeFix, freezeFix', unfreezeFix, unfreezeFix'
    , (?!), fixFold
    )
where

import qualified Prelude
import Feldspar hiding (sugar,desugar)
import Feldspar.Vector

import Language.Syntactic hiding (fold)

-- | Abstract real number type with exponent and mantissa
data Fix a =
    Fix
    { exponent  :: Data IntN
    , mantissa  :: Data a
    }
    deriving (Prelude.Eq,Prelude.Show)

instance
    ( Integral a
    , Bits a
    , Prelude.Real a
    ) => Num (Fix a)
  where
    fromInteger n = Fix 0 (Prelude.fromInteger n)
    (+) = fixAddition
    (*) = fixMultiplication
    negate = fixNegate
    abs = fixAbsolute
    signum = fixSignum

instance
    ( Integral a
    , Bits a
    , Prelude.Real a
    ) => Fractional (Fix a)
  where
    (/) = fixDiv'
    recip = fixRecip'
    fromRational = fixfromRational

fixAddition :: (Integral a, Bits a, Prelude.Real a) => Fix a -> Fix a -> Fix a
fixAddition f1@(Fix e1 _) f2@(Fix e2 _) = Fix e m
   where
     e = max e1 e2
     m = mantissa (fix e f1) + mantissa (fix e f2)

fixMultiplication :: (Integral a, Bits a, Prelude.Real a) => Fix a -> Fix a -> Fix a
fixMultiplication (Fix e1 m1) (Fix e2 m2) = Fix e m
   where
     e =  e1 + e2
     m =  m1 * m2

fixNegate :: (Integral a, Bits a, Prelude.Real a) => Fix a -> Fix a
fixNegate (Fix e1 m1)  = Fix e1 m
   where
     m = negate m1

fixAbsolute :: (Integral a, Bits a, Prelude.Real a) => Fix a -> Fix a
fixAbsolute (Fix e1 m1)  = Fix e1 m
   where
     m = abs m1

fixSignum :: (Integral a, Bits a, Prelude.Real a) => Fix a -> Fix a
fixSignum (Fix _ m1)  = Fix 0 m
   where
     m = signum m1

fixDiv' :: (Integral a, Bits a, Prelude.Real a)
           => Fix a -> Fix a -> Fix a
fixDiv' (Fix e1 m1) (Fix e2 m2) = Fix e m
   where
     e = e1 - e2
     m = div m1 m2

fixRecip' :: forall a . (Integral a, Bits a, Prelude.Real a)
             => Fix a -> Fix a
fixRecip' (Fix e m) = Fix (e + value (wordLength (T :: T a) - 1)) (div sh m)
   where
     sh  :: Data a
     sh  = (1::Data a) .<<. value (fromInteger $ toInteger $ wordLength (T :: T a) - 1)

fixfromRational :: forall a . (Integral a) =>
                   Prelude.Rational -> Fix a
fixfromRational inp = Fix e m
   where
      inpAsFloat :: Float
      inpAsFloat = fromRational inp
      intPart :: Float
      intPart = fromRational $ toRational $ Prelude.floor inpAsFloat
      intPartWidth :: IntN
      intPartWidth =  Prelude.ceiling $ Prelude.logBase 2 intPart
      fracPartWith :: IntN
      fracPartWith =  wordLength (T :: T a) - intPartWidth - 2
      m = value $ Prelude.floor $ inpAsFloat * 2.0 Prelude.** fromRational (toRational fracPartWith)
      e = negate $ value fracPartWith

instance (Type a) => Syntactic (Fix a) where
  type Domain (Fix a)   = FeldDomain
  type Internal (Fix a) = (IntN, a)
  desugar = desugar . freezeFix
  sugar   = unfreezeFix . sugar

-- | Converts an abstract real number to a pair of exponent and mantissa
freezeFix :: (Type a) => Fix a -> (Data IntN,Data a)
freezeFix (Fix e m) = (e,m)

-- | Converts an abstract real number to fixed point integer with given exponent
freezeFix' :: (Bits a) => IntN -> Fix a -> Data a
freezeFix' e f = mantissa $ fix (value e) f

-- | Converts a pair of exponent and mantissa to an abstract real number
unfreezeFix :: (Type a) => (Data IntN, Data a) -> Fix a
unfreezeFix = uncurry Fix

-- | Converts a fixed point integer with given exponent to an abstract real number
unfreezeFix' :: IntN -> Data a -> Fix a
unfreezeFix' e = Fix (value e)

-- This function cannot be implemented now as we don't have access to range
-- information while building the tree anymore.
{-
significantBits :: forall a . (Type a, Num a, Ord a, Size a ~ Range a, Prelude.Real a) => Data a -> IntN
significantBits x = IntN $ fromInteger $ toInteger $ (Prelude.floor mf)+1
  where
    r :: Range a
    r = dataSize x
    m :: a
    m = Prelude.max (Prelude.abs $ lowerBound r) (Prelude.abs $ upperBound r)
    mf :: Float
    mf = logBase 2 $ fromRational $ toRational m
-}
{-
setSignificantBits :: forall a . (Type a, Num a, Ord a, Size a ~ Range a, Prelude.Real a) => a -> Data a -> Data a
setSignificantBits sb x = resizeData r x
  where
    r :: Range a
    r =  Range 0 sb
-}
wordLength :: forall a . (Integral a, Prelude.Real a) => T a -> IntN
wordLength _ = Prelude.ceiling ( Prelude.logBase 2 $ fromRational $ toRational (maxBound :: a)) + 1

-- | Operations to get and set exponent
class (Splittable t) => Fixable t where
    fix :: Data IntN -> t -> t
    getExp :: t -> Data IntN

instance (Bits a) => Fixable (Fix a) where
    fix e' (Fix e m) = Fix e' $ e' > e ? (m .>>. i2n (e' - e), m .<<. i2n (e - e'))
    getExp = Feldspar.FixedPoint.exponent

instance Fixable (Data Float) where
    fix = const id
    getExp = const $ fromInteger $ toInteger $ Feldspar.exponent (0.0 :: Float)

data T a = T

-- | Operations to split data into dynamic and static parts
class (Syntax (Dynamic t)) => Splittable t where
    type Static t
    type Dynamic t
    store       :: t -> (Static t, Dynamic t)
    retrieve    :: (Static t, Dynamic t) -> t
    patch       :: Static t -> t -> t
    common      :: t -> t -> Static t

instance (Type a) => Splittable (Data a) where
    type Static (Data a) = ()
    type Dynamic (Data a) = Data a
    store x = ((),x)
    retrieve = snd
    patch = const id
    common _ _ = ()

instance (Bits a) => Splittable (Fix a) where
    type Static (Fix a) = Data IntN
    type Dynamic (Fix a) = Data a
    store f = (Feldspar.FixedPoint.exponent f, mantissa f)
    retrieve = uncurry Fix
    patch = fix
    common f g = max (Feldspar.FixedPoint.exponent f) (Feldspar.FixedPoint.exponent g)

-- | A version of vector fold for fixed point algorithms
fixFold :: forall a b . (Splittable a) => (a -> b -> a) -> a -> Vector b -> a
fixFold fun ini vec = retrieve (static, fold fun' ini' vec)
  where
    static = fst $ store ini
    ini' = snd $ store ini
    fun' st el = snd $ store $ patch static $ retrieve (static,st) `fun` el

-- | A version of branching for fixed point algorithms
infix 1 ?!
(?!) :: forall a . (Syntax a, Splittable a) => Data Bool -> (a,a) -> a
cond ?! (x,y) = retrieve (comm, cond ? (x',y'))
  where
    comm = common x y
    x' = snd $ store $ patch comm x
    y' = snd $ store $ patch comm y
