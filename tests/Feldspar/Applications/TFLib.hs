{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

--
-- Copyright (c) 2020, ERICSSON AB
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

module Feldspar.Applications.TFLib where

import Feldspar as F
import Feldspar.Vector as V
import qualified Prelude as P

-- Convenience functions for two dimensional tensors, represented as multidim arrays

-- Map a function taking a vector argument over the rows of a matrix
mapOverRows :: (Pull DIM1 a -> b) -> Pull DIM2 a -> Pull DIM1 b
mapOverRows rowFun (Pull ixf (Z :. nRows :. rowLen)) = Pull ixf2 (Z :. nRows)
  where ixf2 (Z :. i) = rowFun (Pull ixf3 (Z :. rowLen))
             where ixf3 (Z :. j) = ixf (Z :. i :. j)

-- Map a function taking a vector argument to a vector result over the rows of a matrix and flatten to a 2D result
mapOverRowsF :: (Pull DIM1 a -> Pull DIM1 b) -> Pull DIM2 a -> Pull DIM2 b
mapOverRowsF rowFun (Pull ixf (Z :. nRows :. rowLen)) = Pull ixf2 (Z :. nRows :. rowLen)
  where ixf2 (Z :. i :. k) = rowFun (Pull ixf3 (Z :. rowLen)) ! (Z :. k)
             where ixf3 (Z :. j) = ixf (Z :. i :. j)

-- Map a function taking a vector argument over the columns of a matrix
mapOverCols :: (Pull DIM1 a -> b) -> Pull DIM2 a -> Pull DIM1 b
mapOverCols colFun (Pull ixf (Z :. nRows :. rowLen)) = Pull ixf2 (Z :. rowLen)
  where ixf2 (Z :. j) = colFun (Pull ixf3 (Z :. nRows))
             where ixf3 (Z :. i) = ixf (Z :. i :. j)

-- Concatenate two matrices horizontally
concatH :: Syntax a => Pull DIM2 a -> Pull DIM2 a -> Pull DIM2 a
concatH (Pull ixf1 (Z :. nRows1 :. rowLen1)) (Pull ixf2 (Z :. nRows2 :. rowLen2)) 
  = Pull (ixf ixf1 ixf2) (Z :. F.min nRows1 nRows2 :. (+) rowLen1 rowLen2)
  where ixf ixf1 ixf2 (Z :. i :. j) 
          = (j F.< rowLen1 ? ixf1 (Z :. i :. j)) $ ixf2 (Z :. i :. (F.-) j rowLen1 :: Shape DIM2)

-- Concatenate two matrices vertically
concatV :: Syntax a => Pull DIM2 a -> Pull DIM2 a -> Pull DIM2 a
concatV (Pull ixf1 (Z :. nRows1 :. rowLen1)) (Pull ixf2 (Z :. nRows2 :. rowLen2)) 
  = Pull (ixf ixf1 ixf2) (Z :. (+) nRows1 nRows2 :. F.min rowLen1 rowLen2)
  where ixf ixf1 ixf2 (Z :. i :. j) = i F.< nRows1 ? ixf1 (Z :. i :. j) $ ixf2 (Z :. (F.-) i nRows1 :. j)

tfExpandDimsDIM2 :: Syntax a => Pull DIM1 a -> Data Int32 -> Pull DIM2 a
tfExpandDimsDIM2 (Pull ixf (Z :. n)) d = Pull ixfn (Z :. sf 1 n :. sf n 1)
  where ixfn (Z :. i :. j) = ixf (Z :. sf j i)
        sf :: Syntax a => a -> a -> a
        sf x y = d F.== (-2) ? x $
                 d F.== (-1) ? y $
                 d F.== 0    ? x $
                               y

dim2shape :: Pull DIM2 a -> Pull DIM1 (Data Int32)
dim2shape (Pull _ (Z :. m :. n)) = Pull (\ (Z :. i) -> F.i2n $ i F.== 0 ? m $ n) (Z :. 2)

-- Construct a matrix containing the value 'x'
tfFillDIM2 :: Pull DIM1 (Data Int32) -> a -> Pull DIM2 a
tfFillDIM2 sh x = Pull (const x) (Z :. (F.i2n $ sh ! (Z :. 0)) :. (F.i2n $ sh ! (Z :. 1)))

-- Make a Pull vector manifest
memoizePull :: Shapely sh => Pull sh (Data Float) -> Pull sh (Data Float)
memoizePull vec = arrToPull ext $ fromPull vec
  where ext = extent vec

-- A zipWith analogue that supports the Tensorflow (and numpy) broadcast (array promotion) rules
-- First some type hackery
data MaxShape sh1 sh2 where
type family BroadcastShape sh1 sh2
type instance BroadcastShape                    Z                    Z = Z
type instance BroadcastShape                    Z (sh  :. Data Length) = sh :. Data Length
type instance BroadcastShape (sh  :. Data Length)                    Z = sh :. Data Length
type instance BroadcastShape (sh1 :. Data Length) (sh2 :. Data Length) = BroadcastShape sh1 sh2 :. Data Length

-- Convert an index into the broadcast structure to an index into one of the components
bcAdj :: Shape sh1 -> Shape sh2 -> Shape (BroadcastShape sh1 sh2) -> Shape sh2
bcAdj (sh1 :. _) (sh2 :. n) (sh3 :. i) = bcAdj sh1 sh2 sh3 :. (n F.== 1 ? 0 $ i)
bcAdj          Z (sh2 :. n) (sh3 :. i) = sh3 :. i
bcAdj         sh          Z        sh3 = Z

-- Compute the shape of the result after broadcasting
bcShape :: Shape sh1 -> Shape sh2 -> Shape (BroadcastShape sh1 sh2)
bcShape (sh1 :. n1) (sh2 :. n2) = bcShape sh1 sh2 :. F.max n1 n2
bcShape Z           (sh  :. n)  = sh :. n
bcShape (sh  :. n)  Z           = sh :. n
bcShape Z           Z           = Z

-- And finally, the bcZipWith proper
bcZipWith f (Pull ixf1 sh1) (Pull ixf2 sh2) = Pull ixf sh
  where ixf ix = f (ixf1 $ bcAdj sh2 sh1 ix) (ixf2 $ bcAdj sh1 sh2 ix)
        sh = bcShape sh1 sh2

-- Special case for 1D vectors and no masks
tfStridedSliceDIM1 :: Pull DIM1 a -> Pull DIM1 (Data Int32) -> Pull DIM1 (Data Int32) -> Pull DIM1 (Data Int32) 
                   -> Pull DIM1 a
tfStridedSliceDIM1 (Pull df (Z :. dn)) (Pull bf bs) (Pull ef es) (Pull sf ss) = Pull rf rs
  where rf (Z :. i) = df (Z :. (b + i*s))
        b = F.i2n $ bf (Z :. 0)
        e = F.i2n $ ef (Z :. 0)
        s = F.i2n $ sf (Z :. 0)
        rs = Z :. ((e-b) `F.div` s) -- This is probably not correct for strides other than 1

-- Special case for a 2D result, implements the special meaning of -1
tfReshapeDIM2 :: Pull sh a -> Pull DIM1 (Data Int32) -> Pull DIM2 a
tfReshapeDIM2 (Pull ixf sh) ds = Pull nixf nsh
  where s = size sh
        m = F.i2n $ ds ! (Z :. 0)
        n = F.i2n $ ds ! (Z :. 1)
        m1 = m F.== (-1) ? s `F.div` n $ m
        n1 = n F.== (-1) ? s `F.div` m $ n
        nsh = Z :. m1 :. n1
        nixf ix = ixf $ V.fromIndex sh $ V.toIndex nsh ix

-- Functions for computing cross entropy
softmaxRow :: Pull DIM1 (Data Float) -> Pull DIM1 (Data Float)
softmaxRow xs = V.map (F./ sumx) xexp
  where xmax = V.maximum xs
        xexp = V.map F.exp $ V.map (F.- xmax) xs
        sumx = V.fromZero $ V.sum xexp

softmaxDIM2 :: Pull DIM2 (Data Float) -> Pull DIM2 (Data Float)
softmaxDIM2 xs = {- V.flatten $ -} mapOverRowsF softmaxRow xs

crossEntropyDIM2 :: Pull DIM2 (Data Float) -> Pull DIM1 (Data Int32) -> Pull DIM1 (Data Float)
crossEntropyDIM2 xs ls = V.zipWith f (mapOverRows (\x -> x) normxs) ls
  where normxs = softmaxDIM2 xs
        f ys l = - F.log (ys ! (Z :. F.i2n l))

-- Squeeze the last dimension from an array
squeezeLast :: Pull (sh :. Data Length) a -> Pull sh a
squeezeLast (Pull ixf (sh :. n)) = Pull (\ ix -> ixf (ix :. 0)) sh

-- Find index of maximum value in vector
maxIdx :: Pull DIM1 (Data Float) -> Data Int32
maxIdx (Pull ixf (Z :. n)) = F.i2n $ forLoop (n-1) 0 $ \ i midx -> ixf (Z :. (i+1)) F.> ixf (Z :. midx) ? i+1 $ midx

-- 2D matrix transpose
transposeDIM2 :: Pull DIM2 a -> Pull DIM2 a
transposeDIM2 (Pull ixf sh) = Pull (ixf . swap) (swap sh)
  where swap :: Shape DIM2 -> Shape DIM2
        swap (Z :. m :. n) = Z :. n :. m

rowDIM2 :: Pull DIM2 a -> Data Index -> Pull DIM1 a
rowDIM2 xs i = indexed (Z :. cols) (\ (Z :. j) -> xs ! (Z :. i :. j))
  where (rows, cols) = case extent xs of Z :. r :. c -> (r,c)

colDIM2 :: Pull DIM2 a -> Data Index -> Pull DIM1 a
colDIM2 xs j = indexed (Z :. rows) (\ (Z :. i) -> xs ! (Z :. i :. j))
  where (rows, cols) = case extent xs of Z :. r :. c -> (r,c)

-- Matrix multiplication
mm :: Pull DIM2 (Data Float) -> Pull DIM2 (Data Float) -> Pull DIM2 (Data Float)
mm xs ys = indexed2 rowsX colsY ixf
  where (rowsX, colsX) = case extent xs of Z :. r :. c -> (r,c)
        (rowsY, colsY) = case extent ys of Z :. r :. c -> (r,c)
        ixf i j = V.fromZero $ V.sum $ V.zipWith (*) (rowDIM2 xs i) (colDIM2 ys j)

-- Matrix multiplication based on the 'column major' function in MultiDim
tfMatMul :: Pull DIM2 (Data Float) -> Pull DIM2 (Data Float) -> Pull DIM2 (Data Float)
tfMatMul xs ys = mm xs ys

-- We make our own zipWith
-- tfZipWithDIM1 f (Pull ixf1 (Z:. n1)) (Pull ixf2 (Z :. n2)) = Pull (\ ix -> ixf1 ix `f` ixf2 ix) (Z :. F.min n1 n2)

-- Now for something completely different

tfArgMax__LPull_DIM1_Int32R___LPull_DIM2_FloatR__Int32 :: Pull DIM2 (Data Float) -> Data Int32 -> Pull DIM1 (Data Int32)
tfArgMax__LPull_DIM1_Int32R___LPull_DIM2_FloatR__Int32 xs sel = sel == 0 ? mapOverCols maxIdx xs $ mapOverRows maxIdx xs

tfAssignAdd_Float_Float_Float :: Data Float -> Data Float -> Data Float
tfAssignAdd_Float_Float_Float = (+)

tfBiasAdd__LPull_DIM2_FloatR___LPull_DIM2_FloatR___LPull_DIM1_FloatR_ :: Pull DIM2 (Data Float) -> Pull DIM1 (Data Float) -> Pull DIM2 (Data Float)
tfBiasAdd__LPull_DIM2_FloatR___LPull_DIM2_FloatR___LPull_DIM1_FloatR_ xss bs = mapOverRowsF (zipWith (+) bs) xss

-- Type conversion functions
tfCast_Float_Int32 :: Data Int32 -> Data Float
tfCast_Float_Int32 i = F.i2n i -- We need more conversions in the simulator


tfCast__LPull_DIM2_FloatR___LPull_DIM2_BoolR_ :: Pull sh (Data Bool) -> Pull sh (Data Float)
tfCast__LPull_DIM2_FloatR___LPull_DIM2_BoolR_ bs = V.map (\ b -> b ? 1.0 $ 0.0) bs

tfCast__LPull_DIM2_Int32R___LPull_DIM2_Int32R_ xs = xs -- Works if we avoid using Int32 in practice, defining it as Int

tfConcatV2__LPull_DIM2_FloatR___LPull_DIM2_FloatR___LPull_DIM2_FloatR___LPull_DIM2_FloatR___LPull_DIM2_FloatR__Int32
   :: Pull DIM2 (Data Float) -> Pull DIM2 (Data Float) -> Pull DIM2 (Data Float) -> Pull DIM2 (Data Float) -> Data Int32 -> Pull DIM2 (Data Float)
tfConcatV2__LPull_DIM2_FloatR___LPull_DIM2_FloatR___LPull_DIM2_FloatR___LPull_DIM2_FloatR___LPull_DIM2_FloatR__Int32 xs ys zs ws d
   = d == 0 ? xs `concatV` ys `concatV` zs `concatV` ws $ xs `concatH` ys `concatH` zs `concatH` ws


tfConst_Float :: Float -> Data Float
tfConst_Float x = value x
tfConst_Int32 :: Int32 -> Data Int32
tfConst_Int32 n = value n

tfConst__LPull_DIM1_Int32R_ :: [Int32] -> Pull DIM1 (Data Int32)
tfConst__LPull_DIM1_Int32R_ ys = Pull (\ (Z :. ix) -> xs!ix) (Z :. getLength xs)
  where xs = value ys

tfEqual__LPull_DIM2_BoolR___LPull_DIM2_Int32R___LPull_DIM2_Int32R_ :: Pull DIM2 (Data Int32) -> Pull DIM2 (Data Int32) -> Pull DIM2 (Data Bool)
tfEqual__LPull_DIM2_BoolR___LPull_DIM2_Int32R___LPull_DIM2_Int32R_ xs ys = bcZipWith (==) xs ys

tfExpandDims__LPull_DIM2_FloatR___LPull_DIM1_FloatR__Int32 :: Pull DIM1 (Data Float) -> Data Int32 -> Pull DIM2 (Data Float)
tfExpandDims__LPull_DIM2_FloatR___LPull_DIM1_FloatR__Int32 xs d = tfExpandDimsDIM2 xs d
tfExpandDims__LPull_DIM2_Int32R___LPull_DIM1_Int32R__Int32 :: Pull DIM1 (Data Int32) -> Data Int32 -> Pull DIM2 (Data Int32)
tfExpandDims__LPull_DIM2_Int32R___LPull_DIM1_Int32R__Int32 xs d = tfExpandDimsDIM2 xs d
-- tfExpandDims__LPull_DIM2_Int32R___LPull_DIM1_Int32R__Int32 :: Pull DIM1 (Data Int32) -> Data Int32 -> Pull DIM2 (Data Int32)
-- tfExpandDims__LPull_DIM2_Int32R___LPull_DIM1_Int32R__Int32 xs d = tfExpandDimsDIM2 xs d  WAS: Int64

tfFill__LPull_DIM2_FloatR___LPull_DIM1_Int32R__Float :: Pull DIM1 (Data Int32) -> Data Float -> Pull DIM2 (Data Float)
tfFill__LPull_DIM2_FloatR___LPull_DIM1_Int32R__Float sh x = tfFillDIM2 sh x


tfGreater_Bool_Float_Float :: Data Float -> Data Float -> Data Bool
tfGreater_Bool_Float_Float x y = x F.> y

tfIdentity__LPull_DIM1_FloatR___LPull_DIM1_FloatR_ :: Pull DIM1 (Data Float) -> Pull DIM1 (Data Float)
tfIdentity__LPull_DIM1_FloatR___LPull_DIM1_FloatR_ xs = xs
tfIdentity__LPull_DIM2_FloatR___LPull_DIM2_FloatR_ :: Pull DIM2 (Data Float) -> Pull DIM2 (Data Float)
tfIdentity__LPull_DIM2_FloatR___LPull_DIM2_FloatR_ xs = xs
tfIdentity__LPull_DIM2_Int32R___LPull_DIM2_Int32R_ :: Pull DIM2 (Data Int32) -> Pull DIM2 (Data Int32)
tfIdentity__LPull_DIM2_Int32R___LPull_DIM2_Int32R_ xs = xs

tfMatMul__LPull_DIM2_FloatR___LPull_DIM2_FloatR___LPull_DIM2_FloatR_ xs ys = tfMatMul xs ys

tfMul__LPull_DIM1_FloatR___LPull_DIM1_FloatR__Float :: Pull DIM1 (Data Float) -> Data Float -> Pull DIM1 (Data Float)
tfMul__LPull_DIM1_FloatR___LPull_DIM1_FloatR__Float xs x = V.map (F.* x) xs
tfMul__LPull_DIM2_FloatR__Float__LPull_DIM2_FloatR_ :: Data Float -> Pull DIM2 (Data Float) -> Pull DIM2 (Data Float)
tfMul__LPull_DIM2_FloatR__Float__LPull_DIM2_FloatR_ x xs = V.map (F.* x) xs
tfMul__LPull_DIM2_FloatR___LPull_DIM2_FloatR__Float :: Pull DIM2 (Data Float) -> Data Float -> Pull DIM2 (Data Float)
tfMul__LPull_DIM2_FloatR___LPull_DIM2_FloatR__Float xs x = V.map (F.* x) xs
tfMul__LPull_DIM2_FloatR___LPull_DIM2_FloatR___LPull_DIM2_FloatR_ :: Pull DIM2 (Data Float) -> Pull DIM2 (Data Float) -> Pull DIM2 (Data Float)
tfMul__LPull_DIM2_FloatR___LPull_DIM2_FloatR___LPull_DIM2_FloatR_ xs ys = bcZipWith (F.*) xs ys

tfNoOp_ :: Data ()
tfNoOp_ = value ()

-- tfOneShotIterator_FIXME:_resource

tfPack__LPull_DIM1_Int32R__Int32_Int32 :: Data Int32 -> Data Int32 -> Pull DIM1 (Data Int32)
tfPack__LPull_DIM1_Int32R__Int32_Int32 x y = V.indexed1 2 (\ i -> i F.== 0 ? x $ y)

tfRealDiv_Float_Float_Float :: Data Float -> Data Float -> Data Float
tfRealDiv_Float_Float_Float x y = x/y

tfRelu__LPull_DIM2_FloatR___LPull_DIM2_FloatR_ :: Pull DIM2 (Data Float) -> Pull DIM2 (Data Float)
tfRelu__LPull_DIM2_FloatR___LPull_DIM2_FloatR_ xs = V.map (\ x -> F.max x 0.0) xs

tfReshape__LPull_DIM2_FloatR___LPull_DIM2_FloatR___LPull_DIM1_Int32R_ :: Pull DIM2 (Data Float) -> Pull DIM1 (Data Int32) -> Pull DIM2 (Data Float)
tfReshape__LPull_DIM2_FloatR___LPull_DIM2_FloatR___LPull_DIM1_Int32R_ xs ds = tfReshapeDIM2 xs ds

tfSelect_Float_Bool_Float_Float :: Data Bool -> Data Float -> Data Float -> Data Float
tfSelect_Float_Bool_Float_Float c t f = c ? t $ f

tfShape__LPull_DIM1_Int32R___LPull_DIM2_FloatR_ :: Pull DIM2 (Data Float) -> Pull DIM1 (Data Int32)
tfShape__LPull_DIM1_Int32R___LPull_DIM2_FloatR_ xs = dim2shape xs

tfSparseSoftmaxCrossEntropyWithLogits__LPull_DIM1_FloatR___LPull_DIM2_FloatR___LPull_DIM1_Int32R_ 
       :: Pull DIM2 (Data Float) -> Pull DIM1 (Data Int32) -> Pull DIM1 (Data Float)
tfSparseSoftmaxCrossEntropyWithLogits__LPull_DIM1_FloatR___LPull_DIM2_FloatR___LPull_DIM1_Int32R_ logits labels
       = crossEntropyDIM2 logits labels

tfSqueeze__LPull_DIM1_Int32R___LPull_DIM2_Int32R_ :: Pull DIM2 (Data Int32) -> Pull DIM1 (Data Int32)
tfSqueeze__LPull_DIM1_Int32R___LPull_DIM2_Int32R_ xs = squeezeLast xs

tfStridedSlice_Int32__LPull_DIM1_Int32R___LPull_DIM1_Int32R___LPull_DIM1_Int32R___LPull_DIM1_Int32R_
       :: Pull DIM1 (Data Int32) -> Pull DIM1 (Data Int32) -> Pull DIM1 (Data Int32) -> Pull DIM1 (Data Int32) -> Data Int32
tfStridedSlice_Int32__LPull_DIM1_Int32R___LPull_DIM1_Int32R___LPull_DIM1_Int32R___LPull_DIM1_Int32R_ d b e s = d ! (Z :. F.i2n (b ! (Z :. 0)))


tfSum_Float_Float__LPull_DIM1_Int32R_ :: Data Float -> Pull DIM1 (Data Int32) -> Data Float
tfSum_Float_Float__LPull_DIM1_Int32R_ x ds = x
tfSum_Float__LPull_DIM2_FloatR___LPull_DIM1_Int32R_ :: Pull DIM2 (Data Float) -> Pull DIM1 (Data Int32) -> Data Float
tfSum_Float__LPull_DIM2_FloatR___LPull_DIM1_Int32R_ xs ds = V.fromZero $ V.sum $ mapOverRows (V.fromZero . V.sum) xs

tfVariableV2_Float :: Float -> (Data Float)
tfVariableV2_Float x = value x

tfVariableV2__LPull_DIM1_FloatR_ :: [Float] -> Pull DIM1 (Data Float)
tfVariableV2__LPull_DIM1_FloatR_ xs = V.arrToPull (Z :. getLength arr) arr
  where arr = value xs

tfVariableV2__LPull_DIM2_FloatR_ :: [[Float]] -> Pull DIM2 (Data Float)
tfVariableV2__LPull_DIM2_FloatR_ xs = V.arrToPull sh $ value $ P.concat xs -- V.fromList $ P.concat xs
  where sh = Z :. m :. n
        m :: Data Length
        m = getLength arr -- value $ P.length xs
        n :: Data Length
        n = m F.== 0 ? 0 $ getLength (arr!0) -- F.i2n $ value $ if P.null xs then 0 else (P.length $ P.head xs)
        arr = value xs

-- | Memoize a Pull vector
memoize :: (Type a, Shapely sh) => Pull sh (Data a) -> Pull sh (Data a)
memoize vec = Pull ixf ext
  where ext = extent vec
        ixf ix = arr ! toIndex ext ix
        arr = fromPull vec
