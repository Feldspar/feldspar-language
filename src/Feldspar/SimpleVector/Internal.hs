{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
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

module Feldspar.SimpleVector.Internal where



import qualified Prelude
import Control.Applicative
import Test.QuickCheck

import Feldspar.Core.Reify (Syntactic(..), resugar)
import Feldspar.Core.Tuple
import Feldspar.Core.NestedTuples

import Feldspar.Range (rangeSubSat)
import qualified Feldspar
import Feldspar hiding (sugar,desugar,resugar)

import Data.Tuple.Curry
import Data.Hash (Hashable)


--------------------------------------------------------------------------------
-- * Types
--------------------------------------------------------------------------------

-- | Symbolic vector
data Vector a
    = Empty
    | Indexed
        { segmentLength :: Data Length
        , segmentIndex  :: Data Index -> a
        , continuation  :: Vector a
        }

type instance Elem      (Vector a) = a
type instance CollIndex (Vector a) = Data Index
type instance CollSize  (Vector a) = Data Length

-- | Non-nested vector
type Vector1 a = Vector (Data a)

-- | Two-level nested vector
type Vector2 a = Vector (Vector (Data a))

instance (Syntax a, Hashable (Internal a)) => Syntactic (Vector a)
  where
    type Internal (Vector a) = [Internal a]
    desugar = desugar . freezeVector . map resugar
    sugar   = map resugar . thawVector . sugar

instance (Syntax a, Show (Internal a), Hashable (Internal a)) => Show (Vector a)
  where
    show = show . eval



--------------------------------------------------------------------------------
-- * Construction/conversion
--------------------------------------------------------------------------------

indexed :: Data Length -> (Data Index -> a) -> Vector a
indexed l idxFun = Indexed l idxFun Empty

-- | Breaks up a segmented vector into a list of single-segment vectors.
segments :: Vector a -> [Vector a]
segments Empty                = []
segments (Indexed l ixf cont) = Indexed l ixf Empty : segments cont
  -- Note: Important to use `Indexed` instead of `indexed` since we need to
  --       guarantee that each vector has a single segment.

length :: Vector a -> Data Length
length Empty = 0
length vec   = Prelude.sum $ Prelude.map segmentLength $ segments vec

-- | Converts a segmented vector to a vector with a single segment.
mergeSegments :: Syntax a => Vector a -> Vector a
mergeSegments Empty = Empty
mergeSegments vec = Indexed (length vec) (ixFun (segments vec)) Empty
    -- Note: Important to use `Indexed` instead of `indexed` since we need to
    --       guarantee that the result has a single segment.
  where
    ixFun []                     = const $ err "indexing in empty vector"
    ixFun (Empty : vs)           = ixFun vs
    ixFun (Indexed l ixf _ : vs) = case vs of
      [] -> ixf
      _  -> \i -> (i<l) ? ixf i $ ixFun vs (i-l)

-- | Converts a non-nested vector to a core vector.
freezeVector :: (Type a, Hashable a) => Vector (Data a) -> Data [a]
freezeVector Empty                = value []
freezeVector (Indexed l ixf cont) = parallel l ixf `append` freezeVector cont

-- | Converts a non-nested core array to a vector.
thawVector :: Type a => Data [a] -> Vector (Data a)
thawVector arr = indexed (getLength arr) (getIx arr)

thawVector' :: (Type a, Hashable a) => Length -> Data [a] -> Vector (Data a)
thawVector' len arr = thawVector $ setLength (value len) arr



--------------------------------------------------------------------------------
-- * Operations
--------------------------------------------------------------------------------

instance Syntax a => Indexed (Vector a)
  where
    (!) = segmentIndex . mergeSegments

instance Syntax a => Sized (Vector a)
  where
    collSize    = length
    setCollSize = newLen

instance CollMap (Vector a) (Vector b)
  where
    collMap = map

-- | Change the length of the vector to the supplied value. If the supplied
-- length is greater than the old length, the new elements will have undefined
-- value. The resulting vector has only one segment.
newLen :: Syntax a => Data Length -> Vector a -> Vector a
newLen l vec = (mergeSegments vec) {segmentLength = l}

-- | Change the length of the vector before and after calling the function.
withLen :: (Syntax a, Syntax b)
        => Data Length -> (Vector a -> Vector b) -> Vector a -> Vector b
withLen l f = newLen l . f . newLen l

(++) :: Vector a -> Vector a -> Vector a
Empty              ++ v     = v
v                  ++ Empty = v
Indexed l ixf cont ++ v     = Indexed l ixf (cont ++ v)

infixr 5 ++

nuncurry :: (a -> b -> c) -> (a, (b, d)) -> c
nuncurry f (x, (y,_)) = f x y

take :: Data Length -> Vector a -> Vector a
take _ Empty                = Empty
take n (Indexed l ixf cont) = indexed nHead ixf ++ take nCont cont
  where
    nHead = min l n
    nCont = sizeProp (nuncurry rangeSubSat) (n,l) $ n - min l n

drop :: Data Length -> Vector a -> Vector a
drop _ Empty = Empty
drop n (Indexed l ixf cont) = indexed nHead (ixf . (+n)) ++ drop nCont cont
  where
    nHead = sizeProp (nuncurry rangeSubSat) (l,n) $ l - min l n
    nCont = sizeProp (nuncurry rangeSubSat) (n,l) $ n - min l n

splitAt :: Data Index -> Vector a -> (Vector a, Vector a)
splitAt n vec = (take n vec, drop n vec)

head :: Syntax a => Vector a -> a
head = (!0)

last :: Syntax a => Vector a -> a
last vec = vec ! (length vec - 1)

tail :: Vector a -> Vector a
tail = drop 1

init :: Vector a -> Vector a
init vec = take (length vec - 1) vec

tails :: Vector a -> Vector (Vector a)
tails vec = indexed (length vec + 1) (`drop` vec)

inits :: Vector a -> Vector (Vector a)
inits vec = indexed (length vec + 1) (`take` vec)

inits1 :: Vector a -> Vector (Vector a)
inits1 = tail . inits

-- | Permute a single-segment vector
permute' :: (Data Length -> Data Index -> Data Index) -> (Vector a -> Vector a)
permute' _    Empty                 = Empty
permute' perm (Indexed l ixf Empty) = indexed l (ixf . perm l)

-- | Permute a vector
permute :: Syntax a =>
    (Data Length -> Data Index -> Data Index) -> (Vector a -> Vector a)
permute perm = permute' perm . mergeSegments

reverse :: Syntax a => Vector a -> Vector a
reverse = permute $ \l i -> l-1-i
  -- TODO Can be optimized (reversing each segment separately, and then
  --      reversing the segment order)

rotateVecL :: Syntax a => Data Index -> Vector a -> Vector a
rotateVecL ix = permute $ \l i -> (i + ix) `rem` l

rotateVecR :: Syntax a => Data Index -> Vector a -> Vector a
rotateVecR ix = reverse . rotateVecL ix . reverse

replicate :: Data Length -> a -> Vector a
replicate n a = Indexed n (const a) Empty

-- | @enumFromTo m n@: Enumerate the integers from @m@ to @n@
--
enumFromTo :: forall a. (Integral a)
           => Data a -> Data a -> Vector (Data a)
enumFromTo 1 n
    | IntType U _ <- typeRep :: TypeRep a
    = indexed (i2n n) ((+1) . i2n)
enumFromTo m n = indexed (i2n l) ((+m) . i2n)
  where
    l = (n<m) ? 0 $ (n-m+1)
  -- TODO The first case avoids the comparison when `m` is 1. However, it
  --      cover the case when `m` is a complicated expression that is later
  --      optimized to the literal 1. The same holds for other such
  --      optimizations in this module.
  --
  --      Perhaps we need a language construct that lets the user supply a
  --      custom optimization rule (similar to `sizeProp`)? `sizeProp` could
  --      probably be expressed in terms of this more general construct.

-- | @enumFrom m@: Enumerate the indexes from @m@ to 'maxBound'
enumFrom :: (Integral a, Hashable a) => Data a -> Vector (Data a)
enumFrom = flip enumFromTo (value maxBound)

-- | See 'enumFromTo'
(...) :: (Integral a) => Data a -> Data a -> Vector (Data a)
(...) = enumFromTo

-- | 'map' @f v@ is the 'Vector' obtained by applying f to each element of
-- @f@.
map :: (a -> b) -> Vector a -> Vector b
map _ Empty = Empty
map f (Indexed l ixf cont) = Indexed l (f . ixf) $ map f cont

-- | Zipping two 'Vector's
zip :: (Syntax a, Syntax b) => Vector a -> Vector b -> Vector (a,b)
zip v1 v2 = go (mergeSegments v1) (mergeSegments v2)
  where
    go Empty _ = Empty
    go _ Empty = Empty
    go (Indexed l1 ixf1 Empty) (Indexed l2 ixf2 Empty) =
      indexed (min l1 l2) ((,) <$> ixf1 <*> ixf2)

-- | Zipping three 'Vector's
zip3 :: (Syntax a, Syntax b, Syntax c)
     => Vector a -> Vector b -> Vector c -> Vector (a,b,c)
zip3 v1 v2 v3 = go (mergeSegments v1) (mergeSegments v2) (mergeSegments v3)
  where
    go Empty _ _ = Empty
    go _ Empty _ = Empty
    go _ _ Empty = Empty
    go (Indexed l1 ixf1 Empty) (Indexed l2 ixf2 Empty) (Indexed l3 ixf3 Empty) =
      indexed (Prelude.foldr1 min [l1,l2,l3]) ((,,) <$> ixf1 <*> ixf2 <*> ixf3)

-- | Zipping four 'Vector's
zip4 :: (Syntax a, Syntax b, Syntax c, Syntax d)
     => Vector a -> Vector b -> Vector c -> Vector d -> Vector (a,b,c,d)
zip4 v1 v2 v3 v4 = go (mergeSegments v1) (mergeSegments v2) (mergeSegments v3) (mergeSegments v4)
  where
    go Empty _ _ _ = Empty
    go _ Empty _ _ = Empty
    go _ _ Empty _ = Empty
    go _ _ _ Empty = Empty
    go (Indexed l1 ixf1 Empty) (Indexed l2 ixf2 Empty) (Indexed l3 ixf3 Empty) (Indexed l4 ixf4 Empty) =
      indexed (Prelude.foldr1 min [l1,l2,l3,l4]) ((,,,) <$> ixf1 <*> ixf2 <*> ixf3 <*> ixf4)

-- | Zipping five 'Vector's
zip5 :: (Syntax a, Syntax b, Syntax c, Syntax d, Syntax e)
     => Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector (a,b,c,d,e)
zip5 v1 v2 v3 v4 v5 = go (mergeSegments v1) (mergeSegments v2) (mergeSegments v3) (mergeSegments v4) (mergeSegments v5)
  where
    go Empty _ _ _ _ = Empty
    go _ Empty _ _ _ = Empty
    go _ _ Empty _ _ = Empty
    go _ _ _ Empty _ = Empty
    go _ _ _ _ Empty = Empty
    go (Indexed l1 ixf1 Empty) (Indexed l2 ixf2 Empty) (Indexed l3 ixf3 Empty) (Indexed l4 ixf4 Empty) (Indexed l5 ixf5 Empty) =
      indexed (Prelude.foldr1 min [l1,l2,l3,l4,l5]) ((,,,,) <$> ixf1 <*> ixf2 <*> ixf3 <*> ixf4 <*> ixf5)

-- | Unzip to two 'Vector's
unzip :: Vector (a,b) -> (Vector a, Vector b)
unzip v = (map sel1 v, map sel2 v)

-- | Unzip to three 'Vector's
unzip3 :: Vector (a,b,c) -> (Vector a, Vector b, Vector c)
unzip3 v = (map sel1 v, map sel2 v, map sel3 v)

-- | Unzip to four 'Vector's
unzip4 :: Vector (a,b,c,d) -> (Vector a, Vector b, Vector c, Vector d)
unzip4 v = (map sel1 v, map sel2 v, map sel3 v, map sel4 v)

-- | Unzip to five 'Vector's
unzip5 :: Vector (a,b,c,d,e) -> (Vector a, Vector b, Vector c, Vector d, Vector e)
unzip5 v = (map sel1 v, map sel2 v, map sel3 v, map sel4 v, map sel5 v)

-- | Generalization of 'zip' using the supplied function instead of tupling
-- to combine the elements
zipWith :: (Syntax a, Syntax b) =>
    (a -> b -> c) -> Vector a -> Vector b -> Vector c
zipWith f a b = map (uncurryN f) $ zip a b

-- | Generalization of 'zip3' using the supplied function instead of tupling
-- to combine the elements
zipWith3 :: (Syntax a, Syntax b, Syntax c) =>
    (a -> b -> c -> d) -> Vector a -> Vector b -> Vector c -> Vector d
zipWith3 f a b c = map (uncurryN f) $ zip3 a b c

-- | Generalization of 'zip4' using the supplied function instead of tupling
-- to combine the elements
zipWith4 :: (Syntax a, Syntax b, Syntax c, Syntax d) =>
    (a -> b -> c -> d -> e) -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
zipWith4 f a b c d = map (uncurryN f) $ zip4 a b c d

-- | Generalization of 'zip5' using the supplied function instead of tupling
-- to combine the elements
zipWith5 :: (Syntax a, Syntax b, Syntax c, Syntax d, Syntax e) =>
    (a -> b -> c -> d -> e -> f) -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector f
zipWith5 f a b c d e = map (uncurryN f) $ zip5 a b c d e

-- | Corresponds to the standard 'foldl'.
fold :: (Syntax a) => (a -> b -> a) -> a -> Vector b -> a
fold _ x Empty = x
fold f x (Indexed l ixf cont) =
    fold f (forLoop l x $ \ix s -> f s (ixf ix)) cont

-- | Corresponds to the standard 'foldl1'.
fold1 :: Syntax a => (a -> a -> a) -> Vector a -> a
fold1 f a = fold f (head a) (tail a)

sum :: (Syntax a, Num a) => Vector a -> a
sum = fold (+) 0

maximum :: Ord a => Vector (Data a) -> Data a
maximum = fold1 max

minimum :: Ord a => Vector (Data a) -> Data a
minimum = fold1 min

or :: Vector (Data Bool) -> Data Bool
or = fold (||) false
  -- TODO Should be lazy

and :: Vector (Data Bool) -> Data Bool
and = fold (&&) true
  -- TODO Should be lazy

any :: (a -> Data Bool) -> Vector a -> Data Bool
any p = or . map p

all :: (a -> Data Bool) -> Vector a -> Data Bool
all p = and . map p

eqVector :: Eq a => Vector (Data a) -> Vector (Data a) -> Data Bool
eqVector a b = (length a == length b) && and (zipWith (==) a b)

-- | Scalar product of two vectors
scalarProd :: (Syntax a, Num a) => Vector a -> Vector a -> a
scalarProd a b = sum (zipWith (*) a b)

scan :: (Syntax a, Syntax b, Hashable (Internal a)) => (a -> b -> a) -> a -> Vector b -> Vector a
scan f init bs = Feldspar.sugar $ sequential (length bs) (Feldspar.desugar init) $ \i s ->
    let s' = Feldspar.desugar $ f (Feldspar.sugar s) (bs!i)
    in  (s',s')
  -- Note: This function should not be exported by the `Vector` module, since it doesn't fuse with
  --       other operations.
  -- TODO Ideally, the `Stream` library should make this function superfluous.



--------------------------------------------------------------------------------
-- Misc.
--------------------------------------------------------------------------------

tVec :: Patch a a -> Patch (Vector a) (Vector a)
tVec _ = id

tVec1 :: Patch a a -> Patch (Vector (Data a)) (Vector (Data a))
tVec1 _ = id

tVec2 :: Patch a a -> Patch (Vector (Vector (Data a))) (Vector (Vector (Data a)))
tVec2 _ = id

instance (Arbitrary (Internal a), Syntax a, Hashable (Internal a)) => Arbitrary (Vector a)
  where
    arbitrary = fmap value arbitrary
