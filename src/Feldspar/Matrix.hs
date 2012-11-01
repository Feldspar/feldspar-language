{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
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

-- | Operations on matrices (doubly-nested parallel vectors). All operations in
-- this module assume rectangular matrices.

module Feldspar.Matrix where

import qualified Prelude as P

import Feldspar.Prelude
import Feldspar.Core
import Feldspar.Vector.Internal



type Matrix a = Vector2 a

tMat :: Patch a a -> Patch (Matrix a) (Matrix a)
tMat = tVec2



-- | Converts a matrix to a core array.
freezeMatrix :: Type a => Matrix a -> Data [[a]]
freezeMatrix = freezeVector . map freezeVector

-- | Converts a core array to a matrix.
thawMatrix :: Type a => Data [[a]] -> Matrix a
thawMatrix = map thawVector . thawVector

-- | Converts a core array to a matrix. The first length argument is the number
-- of rows (outer vector), and the second argument is the number of columns
-- (inner vector).
thawMatrix' :: Type a => Length -> Length -> Data [[a]] -> Matrix a
thawMatrix' y x = map (thawVector' x) . thawVector' y

-- | Constructs a matrix. The elements are stored in a core array.
matrix :: Type a => [[a]] -> Matrix a
matrix = value

-- | Constructing a matrix from an index function.
--
-- @indexedMat m n ixf@:
--
--   * @m@ is the number of rows.
--
--   * @n@ is the number of columns.
--
--   * @ifx@ is a function mapping indexes to elements (first argument is row
--     index; second argument is column index).
indexedMat
    :: Data Length
    -> Data Length
    -> (Data Index -> Data Index -> Data a)
    -> Matrix a
indexedMat m n idx = indexed m $ \k -> indexed n $ \l -> idx k l

-- | Transpose of a matrix. Assumes that the number of rows is > 0.
transpose :: Type a => Matrix a -> Matrix a
transpose a = indexedMat (length $ head a) (length a) $ \y x -> a ! x ! y
  -- TODO This assumes that (head a) can be used even if a is empty.

-- | Concatenates the rows of a matrix.
flatten :: Type a => Matrix a -> Vector (Data a)
flatten matr = Indexed (m*n) ixf Empty
  where
    m = length matr
    n = (m==0) ? (0, length (head matr))

    ixf i = matr ! y ! x
      where
        y = i `div` n
        x = i `mod` n
  -- TODO Should use linear indexing

-- | The diagonal vector of a square matrix. It happens to work if the number of
-- rows is less than the number of columns, but not the other way around (this
-- would require some overhead).
diagonal :: Type a => Matrix a -> Vector (Data a)
diagonal m = zipWith (!) m (0 ... (length m - 1))

distributeL :: (a -> b -> c) -> a -> Vector b -> Vector c
distributeL f = map . f

distributeR :: (a -> b -> c) -> Vector a -> b -> Vector c
distributeR = flip . distributeL . flip



class Mul a b
  where
    type Prod a b

    -- | General multiplication operator
    (***) :: a -> b -> Prod a b

instance Numeric a => Mul (Data a) (Data a)
  where
    type Prod (Data a) (Data a) = Data a
    (***) = (*)

instance Numeric a => Mul (Data a) (Vector1 a)
  where
    type Prod (Data a) (Vector1 a) = Vector1 a
    (***) = distributeL (***)

instance Numeric a => Mul (Vector1 a) (Data a)
  where
    type Prod (Vector1 a) (Data a) = Vector1 a
    (***) = distributeR (***)

instance Numeric a => Mul (Data a) (Matrix a)
  where
    type Prod (Data a) (Matrix a) = Matrix a
    (***) = distributeL (***)

instance Numeric a => Mul (Matrix a) (Data a)
  where
    type Prod (Matrix a) (Data a) = Matrix a
    (***) = distributeR (***)

instance Numeric a => Mul (Vector1 a) (Vector1 a)
  where
    type Prod (Vector1 a) (Vector1 a) = Data a
    (***) = scalarProd

instance Numeric a => Mul (Vector1 a) (Matrix a)
  where
    type Prod (Vector1 a) (Matrix a) = (Vector1 a)
    vec *** mat = distributeL (***) vec (transpose mat)

instance Numeric a => Mul (Matrix a) (Vector1 a)
  where
    type Prod (Matrix a) (Vector1 a) = (Vector1 a)
    (***) = distributeR (***)

instance Numeric a => Mul (Matrix a) (Matrix a)
  where
    type Prod (Matrix a) (Matrix a) = (Matrix a)
    (***) = distributeR (***)



-- | Matrix multiplication
mulMat :: Numeric a => Matrix a -> Matrix a -> Matrix a
mulMat = (***)



class Syntax a => ElemWise a
  where
    type Scalar a

    -- | Operator for general element-wise multiplication
    elemWise :: (Scalar a -> Scalar a -> Scalar a) -> a -> a -> a

instance Type a => ElemWise (Data a)
  where
    type Scalar (Data a) = Data a
    elemWise = id

instance (ElemWise a, Syntax (Vector a)) => ElemWise (Vector a)
  where
    type Scalar (Vector a) = Scalar a
    elemWise = zipWith . elemWise

(.+) :: (ElemWise a, Num (Scalar a)) => a -> a -> a
(.+) = elemWise (+)

(.-) :: (ElemWise a, Num (Scalar a)) => a -> a -> a
(.-) = elemWise (-)

(.*) :: (ElemWise a, Num (Scalar a)) => a -> a -> a
(.*) = elemWise (*)

