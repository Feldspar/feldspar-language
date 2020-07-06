{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

module Feldspar.Repa where

import qualified Prelude as P

import Feldspar.Core.Reify (Syntactic(..), resugar)
import Feldspar hiding (desugar,sugar,resugar)
import Feldspar.Core.NestedTuples

-- | * Shapes

infixl 3 :.
data Z = Z
data tail :. head = tail :. head

type DIM0 = Z
type DIM1 = DIM0 :. Data Length
type DIM2 = DIM1 :. Data Length
type DIM3 = DIM2 :. Data Length

class Shape sh where
  -- | Get the number of dimensions in a shape
  dim          :: sh -> Int
  -- | The shape of an array of size zero, with a particular dimension
  zeroDim      :: sh
  -- | The shape of an array with size one, with a particular dimension
  unitDim      :: sh
  -- | Get the total number of elements in an array with this shape.
  size         :: sh -> Data Length
  -- | Index into flat, linear, row-major representation
  toIndex      :: sh -> sh -> Data Index
  -- | Inverse of `toIndex`.
  fromIndex    :: sh -> Data Index -> sh
  -- | The intersection of two dimensions.
  intersectDim :: sh -> sh -> sh
  -- | Check whether an index is within a given shape.
  -- @inRange l u i@ checks that 'i' fits between 'l' and 'u'.
  inRange      :: sh -> sh -> sh -> Data Bool
  -- | Turn a shape into a list. Used in the 'Syntactic' instance.
  toList       :: sh -> [Data Length]
  -- | Reconstruct a shape. Used in the 'Syntactic' instance.
  toShape      :: Int -> Data [Length] -> sh

instance Shape Z where
  dim Z            = 0
  zeroDim          = Z
  unitDim          = Z
  size Z           = 1
  toIndex _ _      = 0
  fromIndex _ _    = Z
  intersectDim _ _ = Z
  inRange Z Z Z    = true
  toList _         = []
  toShape _ _      = Z

instance Shape sh => Shape (sh :. Data Length) where
  dim (sh :. _)                       = dim sh + 1
  zeroDim                             = zeroDim :. 0
  unitDim                             = unitDim :. 1
  size (sh :. i)                      = size sh * i
  toIndex (sh1 :. sh2) (sh1' :. sh2') = toIndex sh1 sh1' * sh2 + sh2'
  fromIndex (ds :. d) ix
      = fromIndex ds (ix `quot` d) :. (ix `rem` d)
  intersectDim (sh1 :. n1) (sh2 :. n2)
      = (intersectDim sh1 sh2 :. (min n1 n2))
  inRange (shL :. l) (shU :. u) (sh :. i)
      = l <= i && i < u && inRange shL shU sh
  toList (sh :. i)                    = i : toList sh
  toShape i arr
      = toShape (i+1) arr :. (arr ! (P.fromIntegral i))

-- | * Slices

data All = All
data Any sh = Any

type family FullShape ss
type instance FullShape Z                   = Z
type instance FullShape (Any sh)            = sh
type instance FullShape (sl :. Data Length) = FullShape sl :. Data Length
type instance FullShape (sl :. All)         = FullShape sl :. Data Length

type family SliceShape ss
type instance SliceShape Z                   = Z
type instance SliceShape (Any sh)            = sh
type instance SliceShape (sl :. Data Length) = SliceShape sl
type instance SliceShape (sl :. All)         = SliceShape sl :. Data Length

class Slice ss where
  sliceOfFull :: ss -> FullShape ss -> SliceShape ss
  fullOfSlice :: ss -> SliceShape ss -> FullShape ss

instance Slice Z where
  sliceOfFull Z Z = Z
  fullOfSlice Z Z = Z

instance Slice (Any sh) where
  sliceOfFull Any sh = sh
  fullOfSlice Any sh = sh

instance Slice sl => Slice (sl :. Data Length) where
  sliceOfFull (fsl :. _) (ssl :. _) = sliceOfFull fsl ssl
  fullOfSlice (fsl :. n) ssl        = fullOfSlice fsl ssl :. n

instance Slice sl => Slice (sl :. All) where
  sliceOfFull (fsl :. All) (ssl :. s)
   = sliceOfFull fsl ssl :. s
  fullOfSlice (fsl :. All) (ssl :. s)
   = fullOfSlice fsl ssl :. s



-- | * Vectors

data Vector sh a = Vector sh (sh -> a)
type DVector sh a = Vector sh (Data a)

instance (Shape sh, Syntax a) => Syntactic (Vector sh a)
  where
    type Internal (Vector sh a) = NPair [Length] [Internal a]
    desugar = desugar . freezeVector . map resugar
    sugar   = map resugar . thawVector . sugar

type instance Elem      (Vector sh a) = a
type instance CollIndex (Vector sh a) = sh
type instance CollSize  (Vector sh a) = sh

instance Syntax a => Indexed (Vector sh a)
  where
    (Vector _ ixf) ! i = ixf i

instance (Syntax a, Shape sh) => Sized (Vector sh a)
  where
    collSize    = extent
    setCollSize = newExtent

instance CollMap (Vector sh a) (Vector sh a)
  where
    collMap = map

-- | * Fuctions

-- | Store a vector in an array.
fromVector :: (Shape sh, Type a) => DVector sh a -> Data [a]
fromVector vec = parallel (size ext) (\ix -> vec !: fromIndex ext ix)
  where ext = extent vec

-- | Restore a vector from an array
toVector :: (Shape sh, Type a) => sh -> Data [a] -> DVector sh a
toVector sh arr = Vector sh (\ix -> arr ! toIndex ix sh)

freezeVector :: (Shape sh, Type a) => DVector sh a -> NPair (Data [Length]) (Data [a])
freezeVector v   = twotup shapeArr $ fromVector v
  where shapeArr = fromList (toList $ extent v)

fromList :: Type a => [Data a] -> Data [a]
fromList ls = loop 1 (parallel (value len) (const (P.head ls)))
  where loop i arr
            | i P.< len = loop (i+1) (setIx arr (value i) (ls P.!! (P.fromIntegral i)))
            | otherwise = arr
        len  = P.fromIntegral $ P.length ls

thawVector :: (Shape sh, Type a) => (Data [Length], Data [a]) -> DVector sh a
thawVector (l,arr) = toVector (toShape 0 l) arr

-- | Store a vector in memory. Use this function instead of 'force' if
--   possible as it is both much more safe and faster.
memorize :: (Shape sh, Type a) => DVector sh a -> DVector sh a
memorize vec = toVector (extent vec) (fromVector vec)

-- | The shape and size of the vector
extent :: Vector sh a -> sh
extent (Vector sh _) = sh

-- | Change the extent of the vector to the supplied value. If the supplied
-- extent will contain more elements than the old extent, the new elements
-- will have undefined value.
newExtent :: sh -> Vector sh a -> Vector sh a
newExtent sh (Vector _ ixf) = Vector sh ixf

-- | Change shape and transform elements of a vector. This function is the
--   most general way of manipulating a vector.
traverse :: (Shape sh, Shape sh') =>
            Vector sh  a -> (sh -> sh') -> ((sh -> a) -> sh' -> a')
         -> Vector sh' a'
traverse (Vector sh ixf) shf elemf
  = Vector (shf sh) (elemf ixf)

-- | Duplicates part of a vector along a new dimension.
replicate :: (Slice sl, Shape (FullShape sl)
             ,Shape (SliceShape sl))
            => sl -> Vector (SliceShape sl) a
                  -> Vector (FullShape  sl) a
replicate sl vec
 = backpermute (fullOfSlice sl (extent vec))
               (sliceOfFull sl) vec

-- | Extracts a slice from a vector.
slice :: (Slice sl
         ,Shape (FullShape sl)
         ,Shape (SliceShape sl))
        => Vector (FullShape sl) a
            -> sl -> Vector (SliceShape sl) a
slice vec sl
 = backpermute (sliceOfFull sl (extent vec))
               (fullOfSlice sl) vec

-- | Change the shape of a vector. This function is potentially unsafe, the
--   new shape need to have fewer or equal number of elements compared to
--   the old shape.
reshape :: (Shape sh, Shape sh') => sh -> Vector sh' a -> Vector sh a
reshape sh' (Vector sh ixf)
 = Vector sh' (ixf . fromIndex sh . toIndex sh')

-- | A scalar (zero dimensional) vector
unit :: a -> Vector Z a
unit a = Vector Z (const a)

-- | Index into a vector
(!:) :: (Shape sh) => Vector sh a -> sh -> a
(Vector _ ixf) !: ix = ixf ix

-- | Extract the diagonal of a two dimensional vector
diagonal :: Vector DIM2 a -> Vector DIM1 a
diagonal vec = backpermute (Z :. width) (\ (_ :. x) -> Z :. x :. x) vec
  where Z :. _ :. width = extent vec

-- | Change the shape of a vector.
backpermute :: (Shape sh, Shape sh') =>
               sh' -> (sh' -> sh) -> Vector sh a -> Vector sh' a
backpermute sh perm vec = traverse vec (const sh) (. perm)

-- | Map a function on all the elements of a vector
map :: (a -> b) -> Vector sh a -> Vector sh b
map f (Vector sh ixf) = Vector sh (f . ixf)

-- | Combines the elements of two vectors. The size of the resulting vector
--   will be the intersection of the two argument vectors.
zip :: (Shape sh) => Vector sh a -> Vector sh b -> Vector sh (a,b)
zip = zipWith (\a b -> (a,b))

-- | Combines the elements of two vectors pointwise using a function.
--   The size of the resulting vector will be the intersection of the
--   two argument vectors.
zipWith :: (Shape sh) =>
           (a -> b -> c) -> Vector sh a -> Vector sh b -> Vector sh c
zipWith f arr1 arr2 = Vector (intersectDim (extent arr1) (extent arr2))
                      (\ix -> f (arr1 !: ix) (arr2 !: ix))

-- | Reduce a vector along its last dimension
fold :: (Shape sh, Syntax a) =>
        (a -> a -> a)
     -> a
     -> Vector (sh :. Data Length) a
     -> Vector sh a
fold f x vec = Vector sh ixf
    where sh :. n = extent vec
          ixf i = forLoop n x (\ix s -> f s (vec !: (i :. ix)))

-- Here's another version of fold which has a little bit more freedom
-- when it comes to choosing the initial element when folding

-- | A generalization of 'fold' which allows for different initial
--   values when starting to fold.
fold' :: (Shape sh, Syntax a) =>
        (a -> a -> a)
     -> Vector sh a
     -> Vector (sh :. Data Length) a
     -> Vector sh a
fold' f x vec = Vector sh ixf
    where sh :. n = extent vec
          ixf i = forLoop n (x!:i) (\ix s -> f s (vec !: (i :. ix)))

-- | Summing a vector along its last dimension
sum :: (Shape sh, Type a, Numeric a) =>
       DVector (sh :. Data Length) a -> DVector sh a
sum = fold (+) 0

-- | Enumerating a vector
(...) :: Data Index -> Data Index -> DVector DIM1 Index
from ... to = Vector (Z :. (to - from + 1)) (\(Z :. ix) -> ix + from)

-- This one should generalize to arbitrary shapes



-- Laplace

stencil :: DVector DIM2 Float -> DVector DIM2 Float
stencil vec
  = traverse vec id update
  where
    _ :. height :. width = extent vec

    update get d@(sh :. i :. j)
      = isBoundary i j
        ? get d
        $ ( get (sh :. (i-1) :. j)
          + get (sh :. i     :. (j-1))
          + get (sh :. (i+1) :. j)
          + get (sh :. i     :. (j+1))) / 4

    isBoundary i j
      =  (i == 0) || (i >= width  - 1)
      || (j == 0) || (j >= height - 1)

laplace :: Data Length -> DVector DIM2 Float -> DVector DIM2 Float
laplace steps vec = toVector (extent vec) $
                    forLoop steps (fromVector vec) $
                        const $ fromVector . stencil . toVector (extent vec)


-- Matrix Multiplication

transpose2D :: Vector DIM2 e -> Vector DIM2 e
transpose2D vec
  = backpermute new_extent swp vec
  where swp (Z :. i :. j) = Z :. j :. i
        new_extent        = swp (extent vec)

-- | Matrix multiplication
mmMult :: (Type e, Numeric e) =>
          DVector DIM2 e -> DVector DIM2 e
       -> DVector DIM2 e
mmMult vA vB
  = sum (zipWith (*) vaRepl vbRepl)
  where
    vaRepl = replicate (Z :. All   :. colsB :. All) vA
    vbRepl = replicate (Z :. rowsA :. All   :. All) vB
    (Z :. _     :. rowsA) = extent vA
    (Z :. colsB :. _    ) = extent vB

-- One dimensional vectors, meant to help transitioning from
-- the old vector library

mapDIM1 :: (Data Index -> Data Index) -> DIM1 -> DIM1
mapDIM1 ixmap (Z :. i) = (Z :. ixmap i)

indexed :: Data Length -> (Data Index -> a) -> Vector DIM1 a
indexed l idxFun = Vector (Z :. l) (\ (Z :. i) -> idxFun i)

length :: Vector DIM1 a -> Data Length
length (Vector (Z :. l) _) = l

--------------------------------------------------------------------------------
-- * Operations on one dimensional vectors
--------------------------------------------------------------------------------

-- | Change the length of the vector to the supplied value. If the supplied
-- length is greater than the old length, the new elements will have undefined
-- value. The resulting vector has only one segment.
newLen :: Syntax a => Data Length -> Vector DIM1 a -> Vector DIM1 a
newLen l (Vector (Z :. _) ixf) = Vector (Z :. l) ixf

(++) :: Syntax a => Vector DIM1 a -> Vector DIM1 a -> Vector DIM1 a
Vector (Z :. l1) ixf1 ++ Vector (Z :. l2) ixf2
    = Vector (Z :. l1 + l2) (\ (Z :. i) -> i < l1 ? ixf1 (Z :. i)
                                                  $ ixf2 (Z :. (i + l1)))

infixr 5 ++

take :: Data Length -> Vector DIM1 a -> Vector DIM1 a
take n (Vector (Z :. l) ixf) = Vector (Z :. (min n l)) ixf

drop :: Data Length -> Vector DIM1 a -> Vector DIM1 a
drop n (Vector (Z :. l) ixf) = Vector (Z :. (l - min l n)) (ixf . mapDIM1 (+ n))

splitAt :: Data Index -> Vector DIM1 a -> (Vector DIM1 a, Vector DIM1 a)
splitAt n vec = (take n vec, drop n vec)

head :: Syntax a => Vector DIM1 a -> a
head = (! (Z :. 0))

last :: Syntax a => Vector DIM1 a -> a
last vec = vec ! (Z :. (length vec - 1))

tail :: Vector DIM1 a -> Vector DIM1 a
tail = drop 1

init :: Vector DIM1 a -> Vector DIM1 a
init vec = take (length vec - 1) vec

tails :: Vector DIM1 a -> Vector DIM1 (Vector DIM1 a)
tails vec = indexed (length vec + 1) (\n -> drop n vec)

inits :: Vector DIM1 a -> Vector DIM1 (Vector DIM1 a)
inits vec = indexed (length vec + 1) (\n -> take n vec)

inits1 :: Vector DIM1 a -> Vector DIM1 (Vector DIM1 a)
inits1 = tail . inits

-- | Permute a vector
permute :: (Data Length -> Data Index -> Data Index) -> (Vector DIM1 a -> Vector DIM1 a)
permute perm (Vector s@(Z :. l) ixf) = Vector s (ixf . mapDIM1 (perm l))

reverse :: Syntax a => Vector DIM1 a -> Vector DIM1 a
reverse = permute $ \l i -> l-1-i

rotateVecL :: Syntax a => Data Index -> Vector DIM1 a -> Vector DIM1 a
rotateVecL ix = permute $ \l i -> (i + ix) `rem` l

rotateVecR :: Syntax a => Data Index -> Vector DIM1 a -> Vector DIM1 a
rotateVecR ix = reverse . rotateVecL ix . reverse

replicate1 :: Data Length -> a -> Vector DIM1 a
replicate1 n a = Vector (Z :. n) (const a)

-- | @enumFromTo m n@: Enumerate the indexes from @m@ to @n@
--
-- In order to enumerate a different type, use 'i2n', e.g:
--
-- > map i2n (10...20) :: Vector1 Word8
enumFromTo :: Data Index -> Data Index -> Vector DIM1 (Data Index)
enumFromTo 1 n = indexed n (+1)
enumFromTo m n = indexed l (+m)
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
enumFrom :: Data Index -> Vector DIM1 (Data Index)
enumFrom = flip enumFromTo (value maxBound)

unzip :: Vector DIM1 (a,b) -> (Vector DIM1 a, Vector DIM1 b)
unzip (Vector l ixf) = (Vector l (fst.ixf), Vector l (snd.ixf))

-- | Corresponds to the standard 'foldl'.
foldl :: (Syntax a) => (a -> b -> a) -> a -> Vector DIM1 b -> a
foldl f x (Vector (Z :. l) ixf) = forLoop l x $ \ix s -> f s (ixf (Z :. ix))

-- | Corresponds to the standard 'foldl1'.
fold1 :: Syntax a => (a -> a -> a) -> Vector DIM1 a -> a
fold1 f a = foldl f (head a) (tail a)

sum1 :: (Syntax a, Num a) => Vector DIM1 a -> a
sum1  = foldl (+) 0

maximum :: Ord a => Vector DIM1 (Data a) -> Data a
maximum = fold1 max

minimum :: Ord a => Vector DIM1 (Data a) -> Data a
minimum = fold1 min

-- | Scalar product of two vectors
scalarProd :: (Syntax a, Num a) => Vector DIM1 a -> Vector DIM1 a -> a
scalarProd a b = sum1 (zipWith (*) a b)



--------------------------------------------------------------------------------
-- Misc.
--------------------------------------------------------------------------------

tVec :: Patch a a -> Patch (Vector DIM1 a) (Vector DIM1 a)
tVec _ = id

tVec1 :: Patch a a -> Patch (Vector DIM1 (Data a)) (Vector DIM1 (Data a))
tVec1 _ = id

tVec2 :: Patch a a -> Patch (Vector DIM2 (Data a)) (Vector DIM2 (Data a))
tVec2 _ = id
