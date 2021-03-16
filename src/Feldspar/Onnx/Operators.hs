{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -Wall #-}

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

-- | Library implementing ONNX operators

module Feldspar.Onnx.Operators where

import qualified Prelude as P

import Feldspar
import Feldspar.Vector hiding (splitShape)
import qualified Feldspar.Vector as V
import Feldspar.Range (singletonRange)
import Feldspar.Lattice (universal)

import Data.Hash

-- | A list of attributes (key-value pair)
type Attrs = [(AttrName, AttrArg)]

-- | The name of an attribute
type AttrName = String

-- | Values of node attributes.
data AttrArg = AAInt     {aaInt     :: Integer}
             | AAFloat   {aaFloat   :: Float}
             | AADouble  {aaDouble  :: Double}
             | AABool    {aaBool    :: Bool}
             | AAInts    {aaInts    :: [Integer]}
             | AAFloats  {aaFloats  :: [Float]}
             | AADoubles {aaDoubles :: [Double]}
             deriving (P.Eq, P.Show)

-- | Get the value of an attribute from a list of attributes and a default value.
getAttr :: Attrs -> (AttrArg -> a) -> a -> AttrName -> a
getAttr attrs unpack def n = P.maybe def unpack $ P.lookup n attrs

-- | Get the Maybe value of an attribute from a list of attributes.
getAttrM :: Attrs -> (AttrArg -> a) -> AttrName -> Maybe a
getAttrM attrs unpack n = P.fmap unpack $ P.lookup n attrs

{-
   The broadcast functionality here is slightly more general than in ONNX
   since it will accept broadcasting from dimensions whose size is not 1.

   Broadcasting is a generalization of scalar promotion, as when a scalar
   is multiplied with a matrix with the intention of scaling every element
   of the matrix. With broadcasting, tensors of different dimensionality
   can be combined using pointwise operations by extending one or both
   operands.
-}

type family UnionShape sh1 sh2 where
  UnionShape Z                    sh                   = sh
  UnionShape sh                   Z                    = sh
  UnionShape (sh1 :. Data Length) (sh2 :. Data Length) = UnionShape sh1 sh2 :. Data Length

-- | Map an index in a broadcasted shape to an index into the shape before broadcasting.
--   First argument is target extent which must not have less dimensions than source extent.
--   Second argument is source extent.
--   Third argument is index which must have as many dimensions as target extent.
bcIx :: Shape sh1 -> Shape sh2 -> Shape sh1 -> Shape sh2
bcIx _           Z            _         = Z
bcIx (ext1 :. _) (ext2 :. n2) (ix :. i) = bcIx ext1 ext2 ix :. (n2 == 1 ? 0 $ i)
bcIx _           _            _         = P.error "Operators.bcIx: target extent narrower than source extent"

-- | Unidirectional broadcast
uniBCast :: Shape sh1 -> Pull sh2 a -> Pull sh1 a
uniBCast extN (Pull ixf extO) = Pull (ixf . bcIx extN extO) extN

-- | Compute the union of two extents with the max size in each dimension
--   and the maximum number of dimensions.
unionExt :: Shape sh1 -> Shape sh2 -> Shape (UnionShape sh1 sh2)
unionExt Z           sh          = sh
unionExt sh          Z           = sh
unionExt (sh1 :. n1) (sh2 :. n2) = unionExt sh1 sh2 :. max n1 n2

{- |
  Slices
  A slice is a (possibly lower dimensional) part of a multidimensional vector.
  For instance, a range of a one dimensional vector is a slice, a row or
  column of a (two-dimensional) matrix is a one-dimensional slice as is the first
  five elements of a row or column. A sub block of a matrix is a two-dimensional
  slice.

  A slice of an n-dimensional vector is given by an n-dimensional slice index
  where each dimension is either an offset or an offset and a length. The resulting
  slice has one dimension for each dimension where the slice index has a dimension
  and a length.

  Note that 'xs !# (ZZ :! 3)' means the same as 'xs ! (Z :. 3)' namely the fourth 
  element of the one dimensional vector xs, whereas 'xs !# (ZZ :.. (3,1))' is a 
  one dimensional vector of length one containing the same value as its only element.
-}

{- The slices in Feldspar.Vector corresponds to the SliceIndex GADT defined in this
   module as follows:
     SZ  corresponds to ZZ
     ::.  coresponds to :!
     ::: corresponds to :.., but :.. uses an explicit range in (start, length) format
                             wheras ::: always includes all elements
     SAny has no counterpart in this module

   Further, the type families mapping slices to shapes correspond as follows:
     FullShape  corresponds to ToArg
     SliceShape corresponds to ToRes

   The other slice functions correspond as follows:
     sliceOfFull  corresponds to  sliceExt
     fullOfSlize  corresponds to  sliceIdx

   The two slice implementations could be unified by generalizing the implementation
   in Feldspar.Vector by adding functionality corresponding to the :.. constructor.
   This can be done in at least two ways:
   - Add the :.. constructor to the Slice GADT and extend type families and slice functions
   - Add a second constructor to the data type All, e.g. 
       data All = All
                | Part (Data Length) (Data Length)
     and extend slice functions
-}

data (:..) a b

data SliceIndex ix where
  ZZ :: SliceIndex Z
  (:!) :: SliceIndex ix -> Data Length -> SliceIndex (ix :. Data Length)
  (:..) :: SliceIndex ix -> (Data Length, Data Length) -> SliceIndex (ix :.. Data Length)

type family ToArg a where
  ToArg Z                    = Z
  ToArg (sh :.  Data Length) = ToArg sh :. Data Length
  ToArg (sh :.. Data Length) = ToArg sh :. Data Length

type family ToRes a where
  ToRes Z                    = Z
  ToRes (sh :.  Data Length) = ToRes sh
  ToRes (sh :.. Data Length) = ToRes sh :. Data Length

-- | Apply a slice index to a vector, producing a slice
(!#) :: Pull (ToArg sl) a -> SliceIndex sl -> Pull (ToRes sl) a
Pull ixf ext !# sl = Pull (ixf . sliceIx sl) (sliceExt sl ext)

-- | Map an index into the slice to an index into the original vector
sliceIx :: SliceIndex sl -> Shape (ToRes sl) -> Shape (ToArg sl)
sliceIx ZZ             Z         = Z
sliceIx (sl :! i)      sh        = sliceIx sl sh :. i
sliceIx (sl :.. (i,_)) (sh :. j) = sliceIx sl sh :. (i+j)

-- | Map the extent of he original vector to the extent of the slice
sliceExt :: SliceIndex sl -> Shape (ToArg sl) -> Shape (ToRes sl)
sliceExt ZZ             Z         = Z
sliceExt (sl :! _)      (sh :. _) = sliceExt sl sh
sliceExt (sl :.. (_,m)) (sh :. _) = sliceExt sl sh :. m

type ShapelyU sh1 sh2 = Shapely (UnionShape sh1 sh2) 

-- | zipWith with broadcasting
bcZipWith :: ShapelyU sh1 sh2
          => (a -> b -> c) -> Pull sh1 a -> Pull sh2 b -> Pull (UnionShape sh1 sh2) c
bcZipWith f xs ys = zipWith f (uniBCast ext xs) (uniBCast ext ys)
  where ext = unionExt (extent xs) (extent ys) 

-- | Implementation of ONNX tensor addition
onnxAdd :: (Pully vec1, VecShape vec1 ~ sh1, Pully vec2, VecShape vec2 ~ sh2, Num a, ShapelyU sh1 sh2)
        => Attrs -> vec1 a -> vec2 a -> Pull (UnionShape sh1 sh2) a
onnxAdd _ xs ys = bcAdd (toPull xs) (toPull ys)

-- | Implementation of ONNX tensor subtraction
onnxSub :: (Pully vec1, VecShape vec1 ~ sh1, Pully vec2, VecShape vec2 ~ sh2, Num a, ShapelyU sh1 sh2)
        => Attrs -> vec1 a -> vec2 a -> Pull (UnionShape sh1 sh2) a
onnxSub _ xs ys = bcSub (toPull xs) (toPull ys)

-- | Implementation of ONNX tensor multiplication
onnxMul :: (Pully vec1, VecShape vec1 ~ sh1, Pully vec2, VecShape vec2 ~ sh2, Num a, ShapelyU sh1 sh2)
        => Attrs -> vec1 a -> vec2 a -> Pull (UnionShape sh1 sh2) a
onnxMul _ xs ys = bcMul (toPull xs) (toPull ys)

-- | Implementation of ONNX tensor fractional division
onnxDivF :: (Pully vec1, VecShape vec1 ~ sh1, Pully vec2, VecShape vec2 ~ sh2, Fractional a, ShapelyU sh1 sh2)
         => Attrs -> vec1 a -> vec2 a -> Pull (UnionShape sh1 sh2) a
onnxDivF _ xs ys = bcDivF (toPull xs) (toPull ys)

-- | Implementation of ONNX tensor integral division
onnxDivI :: (Pully vec1, VecShape vec1 ~ sh1, Pully vec2, VecShape vec2 ~ sh2, Integral a, ShapelyU sh1 sh2)
         => Attrs -> vec1 (Data a) -> vec2 (Data a) -> DPull (UnionShape sh1 sh2) a
onnxDivI _ xs ys = bcDivI (toPull xs) (toPull ys)

-- | Elementwise add with broadcasting
bcAdd :: (Num a, ShapelyU sh1 sh2)
      => Pull sh1 a -> Pull sh2 a -> Pull (UnionShape sh1 sh2) a
bcAdd = bcZipWith (+)

-- | Elementwise sub with broadcasting
bcSub :: (Num a, ShapelyU sh1 sh2)
      => Pull sh1 a -> Pull sh2 a -> Pull (UnionShape sh1 sh2) a
bcSub = bcZipWith (-)

-- | Elementwise mul with broadcasting
bcMul :: (Num a, ShapelyU sh1 sh2)
      => Pull sh1 a -> Pull sh2 a -> Pull (UnionShape sh1 sh2) a
bcMul = bcZipWith (*)

-- | Elementwise fractional division with broadcasting
bcDivF :: (Fractional a, ShapelyU sh1 sh2)
       => Pull sh1 a -> Pull sh2 a -> Pull (UnionShape sh1 sh2) a
bcDivF = bcZipWith (/)

-- | Elementwise integral division with broadcasting
bcDivI :: (Integral a, ShapelyU sh1 sh2) 
           => DPull sh1 a -> DPull sh2 a -> DPull (UnionShape sh1 sh2) a
bcDivI = bcZipWith div -- Or quot???

-- | Implementation of ONNX batch normalization
onnxBatchNormalization :: Floating a
                       => Attrs  -- ^ attributes (including epsilon)
                       -> DPull DIM4 a -- ^ data
                       -> DPull DIM1 a -- ^ gamma
                       -> DPull DIM1 a -- ^ beta
                       -> DPull DIM1 a -- ^ mean
                       -> DPull DIM1 a -- ^ var
                       -> DPull DIM4 a
onnxBatchNormalization attrs xs gamma beta mean var = ys
  where invDev = map (\ v -> 1.0 / sqrt (v + epsilon)) var <! 1 <! 1
        xsHat = bcMul invDev $ bcSub xs $ mean <! 1 <! 1
        ys = bcAdd (bcMul xsHat $ gamma <! 1 <! 1) $ beta <! 1 <! 1
        epsilon = value $ P.realToFrac $ getAttr attrs aaFloat 1e-5 "epsilon"

-- | Flatten a tensor to a matrix
--   We use Manifest vectors for flattening even though fusion is lost since flattening based on
--   Pull vectors leads to index expressions containing division and modulus that have a greater
--   performance impact than the loss of fusion.
onnxFlatten :: (Storable vec, Syntax a) => Attrs -> vec a -> Manifest DIM2 a
onnxFlatten attrs xs = flatMan d $ store xs
  where d = P.fromIntegral $ getAttr attrs aaInt 1 "axis"

-- | Flatten a Manifest vector to two dimensions by changing its extent
flatMan :: Int -> Manifest sh a -> Manifest DIM2 a
flatMan d (Manifest arr sh) = Manifest arr sh'
  where sh' = Z :. P.product ls :. P.product rs
        (ls,rs) = takeDropShape d sh

-- | Split a shape
takeDropShape :: Int -> Shape sh -> ([Data Length], [Data Length])
takeDropShape i sh = P.splitAt j es
  where es = P.reverse $ toList sh     -- Leftmost index first
        j = if i P.< 0 then i + P.length es else i

-- | Matrix multiplication of two dimensional temsors
onnxGemm :: (RealFloat a, Numeric a, Pully vec, VecShape vec ~ DIM2, Pully vec2,
             UnionShape DIM2 (VecShape vec2) ~ DIM2, Storable vec, Pully vec', Storable vec', VecShape vec' ~ DIM2)
         => Attrs -> vec (Data a) -> vec' (Data a) -> vec2 (Data a) -> DPull DIM2 a
onnxGemm attrs vA vB vC = bcZipWith (+) (mmT vAT vBnT) $ toPull vC
  where vA' = if alpha P.== 1.0 then toPull $ store vA else map (* value alpha) $ toPull $ store vA
        vAT = if transA P.== 1 then transpose vA' else vA'
        vB' = toPull $ store vB
        vBnT = if transA P.== transB then transpose vB' else vB' -- The mmT routine wants its second argument transposed
        alpha = P.realToFrac $ getAttr attrs aaFloat 1.0 "alpha"
        transA = getAttr attrs aaInt 0 "transA"
        transB = getAttr attrs aaInt 0 "transB"

-- | Matrix multiplication that transposes its second argument
mmT :: (Syntax a, Num a) => Pull DIM2 a -> Pull DIM2 a -> Pull DIM2 a
mmT vecA vecBT = Pull ixf $ Z :. iL :. jL
  where ixf (Z :. i :. j) = fromZero $ sum $ zipWith (*) (vecA !# (ZZ :! i :.. (0,kL))) (vecBT !# (ZZ :! j :.. (0,kL)))
        [iL, kL] = P.reverse $ toList $ extent vecA
        [jL, _]  = P.reverse $ toList $ extent vecBT

-- | Summing over the three rightmost dimensions
sum3D :: (Num a, Syntax a, Shapely sh)
      => Pull (sh :. Data Length :. Data Length :. Data Length) a -> Pull sh a
sum3D = sum . sum . sum

-- | Slide a Pull vector
slidePull :: Shape sh -> Pull sh a -> Pull sh a
slidePull d (Pull ixf ext) = Pull (ixf . zipShape (+) d) $ zipShape (-) ext d

infixl 5 <!

-- | Replicate a pull vector along a new rightmost dimension
(<!) :: Pull sh a -> Data Length -> Pull (sh :. Data Length) a
Pull ixf ext <! n = Pull (\ (ix :. _) -> ixf ix) (ext :. n)

-- | Implementation of ONNX convolution operator for 2 inputs
onnxConv_2 :: (Pully vec1, Pushy vec1, VecShape vec1 ~ DIM4, Pully vec2, VecShape vec2 ~ DIM4, Num a, Syntax a)
           => Attrs -> vec1 a -> vec2 a -> Pull DIM4 a
onnxConv_2 attrs xs ws = onnxConv_3 attrs xs ws bs
  where bs = constant (Z :. m) 0
        Z :. m :. _ :. _ :. _ = extent ws

-- | Implementation of ONNX convolution operator for 3 inputs
onnxConv_3 :: (Pully vec1, Pushy vec1, VecShape vec1 ~ DIM4, Pully vec2, VecShape vec2 ~ DIM4,
               Pully vec3, VecShape vec3 ~ DIM1, Num a, Syntax a)
           => Attrs -> vec1 a -> vec2 a -> vec3 a -> Pull DIM4 a
onnxConv_3 attrs xs = if doPad then onnxConvNP (value $ map fromInteger strides) $ store $ pad dPads 0 xs
                               else onnxConvNP (value $ map fromInteger strides) xs
  where -- dilations    = getAttr  attrs aaInts [1, 1]    "dilations" -- Currently unused
        -- group        = getAttr  attrs aaInt  1         "group"     -- Currently unused
        -- kernel_shape = getAttrM attrs aaInts           "kernel_shape" -- Currently unused
        -- FIXME: Extend the implementation to take the remaining attributes into account
        pads         = getAttr  attrs aaInts [0,0,0,0] "pads"
        strides      = getAttr  attrs aaInts [1,1]     "strides"
        doPad = P.any (P./= 0) pads
        dPads = value $ map fromInteger pads :: Data [Length]

-- | Convolution with no padding
onnxConvNP :: (Pully vec1, VecShape vec1 ~ DIM4, Pully vec2, VecShape vec2 ~ DIM4,
               Pully vec3, VecShape vec3 ~ DIM1, Num a, Syntax a)
           => Data [Length] -> vec1 a -> vec2 a -> vec3 a -> Pull DIM4 a
onnxConvNP ss xs ws bs = Pull ixf (Z :. nLen :. mLen :. h1 :. w1) `bcAdd` (toPull bs <! 1 <! 1)
  where ixf (Z :. n :. m :. y :. x) 
            = fromZero $ sum3D $ zipWith (*) (xsP !# (ZZ :! n :.. (0,c) :.. (y*sY, kH) :.. (x*sX, kW)))
                                             (wsP !# (ZZ :! m :.. (0,c) :.. (0,kH) :.. (0,kH)))
        [nLen, c,  h,  w] = P.reverse $ toList $ extent xs
        [mLen, _, kH, kW] = P.reverse $ toList $ extent ws
        (sY,sX) = (ss!0, ss!1)
        h1 = (h - kH) `div` sY + 1
        w1 = (w - kW) `div` sX + 1
        xsP = toPull xs
        wsP = toPull ws

-- | Implementation of ONNX global average pooling
onnxGlobalAveragePool :: (Pully vec, VecShape vec ~ DIM4, Fraction a) => Attrs -> vec (Data a) -> DPull DIM4 a
onnxGlobalAveragePool _ = globalAveragePool . toPull

-- | Global average pooling of a Pull vector
globalAveragePool :: Fraction a => DPull DIM4 a -> DPull DIM4 a
globalAveragePool vec@(Pull _ (Z :. n :. c :. h :. w)) = Pull ixf' (Z :. n :. c :. 1 :. 1)
  where ixf' (Z :. nx :. cx :. _ :. _) = avgF $ vec !# (ZZ :! nx :! cx :.. (0,h) :.. (0,w))
        avgF xs = (fromZero $ sum $ sum xs) / (i2n $ size $ extent xs)

-- | Implementation of ONNX Relu
onnxRelu :: (Pully vec, Numeric a, Ord a) => Attrs -> vec (Data a) -> DPull (VecShape vec) a
onnxRelu _ = fmap (max 0) . toPull

-- | Implementation of ONNX MaxPool
onnxMaxPool :: (Pully vec, VecShape vec ~ DIM4, Numeric a, Ord a, OnnxBounded a) => Attrs -> vec (Data a) -> DPull DIM4 a
onnxMaxPool attrs xs = pool2d max onnxMinBound kernel_shape strides
                     $ toPull $ store
                     $ pad (value $ map fromInteger pads) onnxMinBound
                     $ toPull xs
  where pads         = getAttr  attrs aaInts [0,0,0,0] "pads"
        strides      = getAttr  attrs aaInts [1,1]     "strides"
        kernel_shape = getAttr  attrs aaInts undefined "kernel_shape"

-- | Two dimensional pooling combinator
pool2d :: Type a => (Data a -> Data a -> Data a) -> Data a -> [Integer] -> [Integer] -> DPull DIM4 a -> DPull DIM4 a
pool2d f z ks ss xs = Pull ixf (Z :. nLen :. mLen :. h1 :. w1)
  where ixf (Z :. m :. n :. y :. x) = fromZero $ f2d $ xs !# (ZZ :! m :! n :.. (y*sY, kH) :.. (x*sX, kW))
        [kH,kW] = map (value . fromInteger) ks :: [Data Length]
        [sY,sX] = map (value . fromInteger) ss :: [Data Length]
        f2d = fold f z . fold f z
        [nLen, mLen, h, w] = P.reverse $ toList $ extent xs
        h1 = (h - kH) `div` sY + 1
        w1 = (w - kW) `div` sX + 1

-- | Implementation of ONNX Identity operator
onnxIdentity :: Attrs -> a -> a
onnxIdentity _ x = x

-- | Implementation of 3d variant of ONNX Reshape operator
onnxReshape_d3 :: (Pully vec1, Pully vec2, VecShape vec2 ~ DIM1, Type a)
               => Attrs -> vec1 (Data a) -> vec2 (Data Int64) -> DPull DIM3 a
onnxReshape_d3 _ xs shape = reshape_d3 (toPull xs) (toPull shape)

-- | 3d reshape of Pull vectors
reshape_d3 :: Type a => DPull sh a -> DPull DIM1 Int64 -> DPull DIM3 a
reshape_d3 (Pull ixf ext) shape = Pull ixf1 ext1
  where lext = P.reverse $ toList ext
        ixf1 = ixf . fromIndex ext . toIndex ext1
        ext1 = Z :. dsize 0 :. dsize 1 :. dsize 2
        dsize i = case P.drop (P.fromIntegral i) lext of
                    [] -> m
                    n : _ -> m == 0 ? n $ m
                  where m = i2n $ shape ! (Z :. (value i :: Data Length))

-- | Implementaion of ONNX MatMul operator
onnxMatMul :: (Pully vec1, VecShape vec1 ~ DIM2, Pully vec2, VecShape vec2 ~ DIM2, RealFloat a, Numeric a)
           => Attrs -> vec1 (Data a) -> vec2 (Data a) -> DPull DIM2 a
onnxMatMul _ a b = mmT (toPull a) (transpose $ toPull b)

-- | Padding a multi dimensional vector
pad :: forall a vec sh . (Syntax a, Num a, Pushy vec,
                          VecShape vec ~ (sh :. Data Length :. Data Length))
    => Data [Length] -> a -> vec a -> Push (sh :. Data Length :. Data Length) a
pad ps x vec
  = bpadding `vconc` (lpadding `hconc` pvec `hconc` rpadding) `vconc` tpadding
    where pvec = toPush vec
          (ext,m,n) = case extent pvec of sh :. i :. j -> (sh, i, j)
          bpadding = toPush $ constant (ext :. pB :. n1) x
          tpadding = toPush $ constant (ext :. pT :. n1) x
          lpadding = toPush $ constant (ext :. m :. pL)  x
          rpadding = toPush $ constant (ext :. m :. pR)  x
          (pB,pT,pL,pR) = (ps!0, ps!1, ps!2, ps!3)
          n1 = n + pL + pR
          vconc = V.conc (V.NotThis V.This)
          hconc = V.conc V.This

class SplitShape b c where
  type Peel b c
  splitShape :: Shape c -> (Shape (Peel b c), Shape b)
  appShape :: Shape (Peel b c) -> Shape b -> Shape c

instance SplitShape Z c where
  type Peel Z c = c
  splitShape sh = (sh, Z)
  appShape sh Z = sh

instance SplitShape b c => SplitShape (b :. Data Length) (c :. Data Length) where
  type Peel (b :. Data Length) (c :. Data Length) = Peel b c
  splitShape (sh :. n) = (sh1, sh2 :. n)
    where (sh1,sh2) = splitShape sh
  appShape sh1 (sh2 :. n) = appShape sh1 sh2 :. n

dim0 :: Shape DIM0
dim0 = Z

dim1 :: Shape DIM1
dim1 = dim0 :. 0

dim2 :: Shape DIM2
dim2 = dim1 :. 0

class PrefShape sh1 sh2 where
  type RestShape sh1 sh2
  spShape :: Shape sh1 -> Shape sh2 -> (Shape sh1, Shape (RestShape sh1 sh2))

instance PrefShape Z sh where
  type RestShape Z sh = sh
  spShape Z sh = (Z, sh)

instance PrefShape sh1 sh2 => PrefShape (sh1 :. Data Length) (sh2 :. Data Length) where
  type RestShape (sh1 :. Data Length) (sh2 :. Data Length) = RestShape sh1 sh2
  spShape (sh1 :. _) (sh2 :. n) = (sh1' :. n, sh2')
    where (sh1',sh2') = spShape sh1 sh2

-- | Map an n-dimensional function over the n rightmost dimensions of a multi dimensional vector
vvmap :: (PrefShape sh (AppendShape sh sh2), PrefShape sh (AppendShape sh sh3),
          sh2 ~ RestShape sh (AppendShape sh sh2),
          sh3 ~ RestShape sh (AppendShape sh sh3))
      => Shape sh -> (Pull sh2 a -> Pull sh3 b) -> Pull (AppendShape sh sh2) a -> Pull (AppendShape sh sh3) b
vvmap d f vec = Pull ixfN (appendShape ext1 $ extent $ f $ fromExt ext2)
  where Pull ixf ext = toPull vec
        (ext1,ext2) = spShape d ext
        ixfN ix = f (Pull (ixf . appendShape ix1) ext2) ! ix2
          where (ix1,ix2) = spShape d ix

-- | Map av n-dimensional function
vmap :: (Pully vec, VecShape vec ~ sh', SplitShape sh1 sh',
         SplitShape sh2 sh'', Peel sh1 sh' ~ Peel sh2 sh'')
     => (Pull sh1 a -> Pull sh2 b) -> vec a -> Pull sh'' b
vmap f vec = Pull ixfN (appShape ext1 $ extent $ f $ fromExt ext2)
  where Pull ixf ext = toPull vec
        (ext1,ext2) = splitShape ext
        ixfN ix = f (Pull (ixf . appShape ix1) ext2) ! ix2
          where (ix1,ix2) = splitShape ix

-- | zipWith an n-dimensional function
vZipWith :: (Pully vec1, VecShape vec1 ~ sh1', SplitShape sh1 sh1',
             Peel sh1 sh1' ~ Peel sh sh',
             Pully vec2, VecShape vec2 ~ sh2', SplitShape sh2 sh2',
             Peel sh2 sh2' ~ Peel sh sh', SplitShape sh sh')
         => (Pull sh1 a1 -> Pull sh2 a2 -> Pull sh a) -> vec1 a1 -> vec2 a2 -> Pull sh' a
vZipWith f vec1 vec2
  = Pull ixfN (appShape ext1' $ extent $ f (fromExt ext1'') (fromExt ext2''))
    where Pull ixf1 ext1  = toPull vec1
          Pull ixf2 ext2  = toPull vec2
          (ext1', ext1'') = splitShape ext1 -- ext1' and ext2' should be equal
          (_, ext2'')     = splitShape ext2
          ixfN ix = f (Pull (ixf1 . appShape ixL) ext1'')
                      (Pull (ixf2 . appShape ixL) ext2'')
                  ! ixR
            where (ixL, ixR) = splitShape ix

-- | Construct a Pull vector with an undefined index function and a given extent
fromExt :: Shape sh -> Pull sh a
fromExt = Pull (P.error "Extent of result depends on index function of argument.")

{- The rationale for the definitions below is mainly max and min pooling in combination
   with padding. ONNX specifies that the padding should not contribute to the max or min
   value within a pooling window near the edge.
-}

class OnnxBounded a where
  onnxMinBound :: a
  onnxMaxBound :: a

-- | OnnxBounded instances for Float
instance OnnxBounded Float where
  onnxMinBound = -(1/0)
  onnxMaxBound =  (1/0)

-- | OnnxBounded instance for Double
instance OnnxBounded Double where
  onnxMinBound = -(1/0)
  onnxMaxBound =  (1/0)

-- | OnnxBounded instance for Data
instance (Hashable a, Type a, OnnxBounded a) => OnnxBounded (Data a) where
  onnxMinBound = value onnxMinBound
  onnxMaxBound = value onnxMaxBound

-- | Add range info to one dimensional Pull vector
setSizePull1 :: Length -> Pull DIM1 a -> Pull DIM1 a
setSizePull1 s1 (Pull ixf (Z :. e1)) = Pull ixf (Z :. cap (singletonRange s1) e1)

-- | Add range info to two dimensional Pull vector
setSizePull2 :: Length -> Length -> Pull DIM2 a -> Pull DIM2 a
setSizePull2 s1 s2 (Pull ixf (Z :. e1 :. e2))
  = Pull ixf (Z :. cap (singletonRange s1) e1 :. cap (singletonRange s2) e2)

-- | Add range info to three dimensional Pull vector
setSizePull3 :: Length -> Length -> Length -> Pull DIM3 a -> Pull DIM3 a
setSizePull3 s1 s2 s3 (Pull ixf (Z :. e1 :. e2 :. e3))
  = Pull ixf (Z :. cap (singletonRange s1) e1 :. cap (singletonRange s2) e2 :. cap (singletonRange s3) e3)

-- | Add range info to three dimensional Pull vector
setSizePull4 :: Length -> Length -> Length -> Length -> Pull DIM4 a -> Pull DIM4 a
setSizePull4 s1 s2 s3 s4 (Pull ixf (Z :. e1 :. e2 :. e3 :. e4))
  = Pull ixf (Z :. cap (singletonRange s1) e1 :. cap (singletonRange s2) e2 :. cap (singletonRange s3) e3
                :. cap (singletonRange s4) e4)

-- | Add range info to one dimensinal Manifest vector
setSizeManifest1 :: Length -> Manifest DIM1 a -> Manifest DIM1 a
setSizeManifest1 s1 (Manifest arr (Z :. e1))
  = Manifest (cap (singletonRange s1 :> universal) arr)
             (Z :. cap (singletonRange s1) e1)

-- | Add range info to two dimensinal Manifest vector
setSizeManifest2 :: Length -> Length -> Manifest DIM2 a -> Manifest DIM2 a
setSizeManifest2 s1 s2 (Manifest arr (Z :. e1 :. e2))
  = Manifest (cap (singletonRange (s1*s2) :> universal) arr)
             (Z :. cap (singletonRange s1) e1 :. cap (singletonRange s2) e2)

-- | Add range info to three dimensinal Manifest vector
setSizeManifest3 :: Length -> Length -> Length -> Manifest DIM3 a -> Manifest DIM3 a
setSizeManifest3 s1 s2 s3 (Manifest arr (Z :. e1 :. e2 :. e3))
  = Manifest (cap (singletonRange (s1*s2*s3) :> universal) arr)
             (Z :. cap (singletonRange s1) e1 :. cap (singletonRange s2) e2 :. cap (singletonRange s3) e3)

-- | Add range info to four dimensinal Manifest vector
setSizeManifest4 :: Length -> Length -> Length -> Length -> Manifest DIM4 a -> Manifest DIM4 a
setSizeManifest4 s1 s2 s3 s4 (Manifest arr (Z :. e1 :. e2 :. e3 :. e4))
  = Manifest (cap (singletonRange (s1*s2*s3*s4) :> universal) arr)
             (Z :. cap (singletonRange s1) e1 :. cap (singletonRange s2) e2 :. cap (singletonRange s3) e3
                :. cap (singletonRange s4) e4)
