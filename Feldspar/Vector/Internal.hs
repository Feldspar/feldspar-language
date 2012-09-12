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

module Feldspar.Vector.Internal where



import qualified Prelude
import Control.Arrow ((&&&))
import qualified Data.TypeLevel as TL
import Test.QuickCheck

import QuickAnnotate

import Language.Syntactic

import Feldspar.Range (rangeSubSat)
import Feldspar hiding (sugar,desugar,resugar)
import Feldspar.Wrap



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
type DVector a = Vector (Data a)
{-# DEPRECATED DVector "Please use `Vector1` instead." #-}

-- | Non-nested vector
type Vector1 a = Vector (Data a)

-- | Two-level nested vector
type Vector2 a = Vector (Vector (Data a))

instance Syntax a => Syntactic (Vector a) FeldDomainAll
  where
    type Internal (Vector a) = [Internal a]
    desugar = desugar . freezeVector . map resugar
    sugar   = map resugar . thawVector . sugar

instance Syntax a => Syntax (Vector a)

instance (Syntax a, Show (Internal a)) => Show (Vector a)
  where
    show = show . eval



--------------------------------------------------------------------------------
-- * Construction/conversion
--------------------------------------------------------------------------------

indexed :: Data Length -> (Data Index -> a) -> Vector a
indexed 0 _      = Empty
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
mergeSegments vec = Indexed (length vec) (ixFun (segments vec)) Empty
    -- Note: Important to use `Indexed` instead of `indexed` since we need to
    --       guarantee that the result has a single segment.
  where
    ixFun []                     = const $ err "indexing in empty vector"
    ixFun (Empty : vs)           = ixFun vs
    ixFun (Indexed l ixf _ : vs) = case vs of
      [] -> ixf
      _  -> \i -> (i<l) ? (ixf i, ixFun vs (i-l))

-- | Converts a non-nested vector to a core vector.
freezeVector :: Type a => Vector (Data a) -> Data [a]
freezeVector Empty                = value []
freezeVector (Indexed l ixf cont) = parallel l ixf `append` freezeVector cont

-- | Converts a non-nested core array to a vector.
thawVector :: Type a => Data [a] -> Vector (Data a)
thawVector arr = indexed (getLength arr) (getIx arr)

thawVector' :: Type a => Length -> Data [a] -> Vector (Data a)
thawVector' len arr = thawVector $ setLength (value len) arr

unfreezeVector :: Type a => Data [a] -> Vector (Data a)
unfreezeVector = thawVector
{-# DEPRECATED unfreezeVector "Please use `thawVector` instead." #-}

unfreezeVector' :: Type a => Length -> Data [a] -> Vector (Data a)
unfreezeVector' = thawVector'
{-# DEPRECATED unfreezeVector' "Please use `thawVector'` instead." #-}



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

(++) :: Vector a -> Vector a -> Vector a
Empty              ++ v     = v
v                  ++ Empty = v
Indexed l ixf cont ++ v     = Indexed l ixf (cont ++ v)

infixr 5 ++

take :: Data Length -> Vector a -> Vector a
take _ Empty                = Empty
take n (Indexed l ixf cont) = indexed nHead ixf ++ take nCont cont
  where
    nHead = min l n
    nCont = sizeProp (uncurry rangeSubSat) (n,l) $ n - min l n

drop :: Data Length -> Vector a -> Vector a
drop _ Empty = Empty
drop n (Indexed l ixf cont) = indexed nHead (ixf . (+n)) ++ drop nCont cont
  where
    nHead = sizeProp (uncurry rangeSubSat) (l,n) $ l - min l n
    nCont = sizeProp (uncurry rangeSubSat) (n,l) $ n - min l n

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

-- | @enumFromTo m n@: Enumerate the indexes from @m@ to @n@
--
-- In order to enumerate a different type, use 'i2n', e.g:
--
-- > map i2n (10...20) :: Vector1 Word8
enumFromTo :: Data Index -> Data Index -> Vector (Data Index)
enumFromTo 1 n = indexed n (+1)
enumFromTo m n = indexed l (+m)
  where
    l = (n<m) ? (0, n-m+1)
  -- TODO The first case avoids the comparison when `m` is 1. However, it
  --      cover the case when `m` is a complicated expression that is later
  --      optimized to the literal 1. The same holds for other such
  --      optimizations in this module.
  --
  --      Perhaps we need a language construct that lets the user supply a
  --      custom optimization rule (similar to `sizeProp`)? `sizeProp` could
  --      probably be expressed in terms of this more general construct.

-- | @enumFrom m@: Enumerate the indexes from @m@ to 'maxBound'
enumFrom :: Data Index -> Vector (Data Index)
enumFrom = flip enumFromTo (value maxBound)

-- | See 'enumFromTo'
(...) :: Data Index -> Data Index -> Vector (Data Index)
(...) = enumFromTo

map :: (a -> b) -> Vector a -> Vector b
map _ Empty = Empty
map f (Indexed l ixf cont) = Indexed l (f . ixf) $ map f cont

-- | Zipping a single-segment vector
zip1 :: Vector a -> Vector b -> Vector (a,b)
zip1 Empty _ = Empty
zip1 _ Empty = Empty
zip1 (Indexed l1 ixf1 Empty) (Indexed l2 ixf2 Empty) =
    indexed (min l1 l2) (ixf1 &&& ixf2)

zip :: (Syntax a, Syntax b) => Vector a -> Vector b -> Vector (a,b)
zip vec1 vec2 = zip1 (mergeSegments vec1) (mergeSegments vec2)

unzip :: Vector (a,b) -> (Vector a, Vector b)
unzip Empty = (Empty, Empty)
unzip (Indexed l ixf cont) =
    (Indexed l (fst.ixf) cont1, Indexed l (snd.ixf) cont2)
  where
    (cont1,cont2) = unzip cont

zipWith :: (Syntax a, Syntax b) =>
    (a -> b -> c) -> Vector a -> Vector b -> Vector c
zipWith f aVec bVec = map (uncurry f) $ zip aVec bVec

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

-- | Scalar product of two vectors
scalarProd :: (Syntax a, Num a) => Vector a -> Vector a -> a
scalarProd a b = sum (zipWith (*) a b)



--------------------------------------------------------------------------------
-- Misc.
--------------------------------------------------------------------------------

tVec :: Patch a a -> Patch (Vector a) (Vector a)
tVec _ = id

tVec1 :: Patch a a -> Patch (Vector (Data a)) (Vector (Data a))
tVec1 _ = id

tVec2 :: Patch a a -> Patch (Vector (Vector (Data a))) (Vector (Vector (Data a)))
tVec2 _ = id

instance (Arbitrary (Internal a), Syntax a) => Arbitrary (Vector a)
  where
    arbitrary = fmap value arbitrary

instance (Type a) => Wrap (Vector (Data a)) (Data [a]) where
    wrap = freezeVector

instance (Wrap t u, Type a, TL.Nat s) => Wrap (DVector a -> t) (Data' s [a] -> u) where
    wrap f = \(Data' d) -> wrap $ f $ thawVector $ setLength s' d where
        s' = fromInteger $ toInteger $ TL.toInt (undefined :: s)

instance Annotatable a => Annotatable (Vector a)
  where
    annotate _    Empty                  = Empty
    annotate info (Indexed len ixf cont) = Indexed
        (annotate (info Prelude.++ " (vector length)") len)
        (annotate (info Prelude.++ " (vector element)") . ixf)
        (annotate info cont)

