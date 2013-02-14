{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE FlexibleContexts #-}
module Feldspar.Vector.Unbox where

import qualified Prelude as P

import Feldspar hiding ((!),M,P)
import qualified Feldspar.Vector.Pull as Pl
import qualified Feldspar.Vector.MultiDim as M
import Feldspar.Vector.MultiDim (Slice(..),Any,All)
import Feldspar.Vector.Shape

class Elm a where
  data Vector a
  type Repr a
  (!) :: Vector a -> Data Index -> a
  indexed :: (Data Index -> a) -> Data Length -> Vector a
  length :: Vector a -> Data Index
  freezeVector :: Vector a -> Data (Repr a)

instance Type a => Elm (Data a) where
  data Vector (Data a) = Pull (Pl.PullVector (Data a))
  type Repr   (Data a) = [a]
  Pull v ! i = Pl.index v i
  indexed ixf l = Pull (Pl.indexed l ixf)
  length (Pull v) = Pl.length v
  freezeVector (Pull v) = Pl.freezePull v

instance (Elm a, Elm b, Type (Repr a), Type (Repr b)) => Elm (a,b) where
  data Vector (a,b) = Pair { fstP :: Vector a, sndP :: Vector b }
  type Repr (a,b) = (Repr a, Repr b)
  Pair va vb ! i = (va!i,vb!i)
  indexed ixf l = Pair (indexed (fst . ixf) l) (indexed (snd . ixf) l)
  length vec = length (fstP vec)
  freezeVector (Pair v1 v2) = desugar (freezeVector v1, freezeVector v2)

instance (Syntax a, Shapely sh) => Elm (M.Vector sh a) where
  data Vector (M.Vector sh a) = M (M.Vector (sh :. Data Length) a)
  type Repr (M.Vector sh a) = [Internal a]
  (M vec) ! i = M.slice vec (SAny ::. i)
  indexed ixf l = M $ M.flatten $ M.indexed (\(Z :. ix) -> ixf ix) (Z :. l)
  length (M (M.Vector (_ :. l) _)) = l
  freezeVector (M vec) = snd (M.freezeVector (fmap desugar vec))

instance Syntax a => Elm (Pl.PullVector a) where
  data Vector (Pl.PullVector a) = P (M.Vector DIM2 a)
  type Repr (Pl.PullVector a) = [Internal a]
  (P vec) ! i = dim1ToPull $ M.slice vec (SAny ::. i)
  indexed ixf l = P $ M.flatten $ M.indexed (\(Z :. ix) -> pullToDIM1 $ ixf ix) (Z :. l)
  length (P (M.Vector (_ :. l) _)) = l
  freezeVector (P vec) = snd (M.freezeVector (fmap desugar vec))

dim1ToPull :: M.Vector DIM1 a -> Pl.PullVector a
dim1ToPull (M.Vector (Z :. l) ixf) = Pl.Pull (\i -> ixf (Z :. i)) l

pullToDIM1 :: Pl.PullVector a -> M.Vector DIM1 a
pullToDIM1 (Pl.Pull ixf l) = M.Vector (Z :. l) (\(Z :. ix) -> ixf ix)

map :: (Elm a, Elm b) => (a -> b) -> Vector a -> Vector b
map f vec = indexed (\ix -> f (vec ! ix)) (length vec)

take :: (Elm a) => Data Length -> Vector a -> Vector a
take n vec = indexed (\ix -> vec ! ix) (min (length vec) n)

drop :: (Elm a) => Data Length -> Vector a -> Vector a
drop n vec = indexed (\ix -> vec ! (ix+n)) (length vec - n)

permute :: (Elm a) =>
           (Data Length -> Data Index -> Data Index) ->
           Vector a -> Vector a
permute perm vec = indexed (\ix -> vec ! (perm (length vec) ix)) (length vec)

reverse :: Elm a => Vector a -> Vector a
reverse = permute (\l i -> l - i - 1)

rotateVecL :: Elm a => Data Index -> Vector a -> Vector a
rotateVecL ix = permute (\l i -> (i + ix) `rem` l)

rotateVecR :: Elm a => Data Index -> Vector a -> Vector a
rotateVecR ix = reverse . rotateVecL ix . reverse

replicate :: Elm a => Data Length -> a -> Vector a
replicate n a = indexed (const a) n
