{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleInstances #-}
module Feldspar.Vector.Shape where

import qualified Prelude as P

import Feldspar

infixl 3 :.
data Z
data tail :. head

type DIM0 = Z
type DIM1 = DIM0 :. Data Length
type DIM2 = DIM1 :. Data Length
type DIM3 = DIM2 :. Data Length

data Shape sh where
  Z :: Shape Z
  (:.) :: Shape tail -> Data Length -> Shape (tail :. Data Length)

dim :: Shape sh -> Int
dim Z = 0
dim (sh :. _) = 1 + dim sh

size :: Shape sh -> Data Length
size Z = 1
size (sh :. i) = size sh * i

toIndex :: Shape sh -> Shape sh -> Data Index
toIndex Z Z = 0
toIndex (sh1 :. sh2) (sh1' :. sh2') = toIndex sh1 sh1' * sh2 + sh2'

fromIndex :: Shape sh -> Data Index -> Shape sh
fromIndex Z _ = Z
fromIndex sh@(_sh :. l) ix = fromIndexOne sh ix

fromIndexOne :: Shape (sh :. Data Index) -> Data Index ->
                Shape (sh :. Data Index)
fromIndexOne (Z :. _) ix = Z :. ix
fromIndexOne (ds@(_ :. _) :. d) ix
  = fromIndexOne ds (ix `quot` d) :. (ix `rem` d)

intersectDim :: Shape sh -> Shape sh -> Shape sh
intersectDim Z Z = Z
intersectDim (sh1 :. n1) (sh2 :. n2)
  = (intersectDim sh1 sh2 :. (min n1 n2))

inRange :: Shape sh -> Shape sh -> Shape sh -> Data Bool
inRange Z Z Z    = true
inRange (shL :. l) (shU :. u) (sh :. i)
  = l <= i && i < u && inRange shL shU sh

forShape :: Shape sh -> (Shape sh -> M ()) -> M ()
forShape Z k = k Z
forShape (sh :. l) k = forM l (\i -> forShape sh (\sh -> k (sh :. i)))

toList :: Shape sh -> [Data Index]
toList Z         = []
toList (sh :. i) = i : toList sh

uncons :: Shape (sh :. Data Length) -> (Shape sh, Data Length)
uncons (sh :. i) = (sh,i)

class Shapely sh where
  zeroDim   :: Shape sh
  unitDim   :: Shape sh
  fakeShape :: Shape sh
  toShape   :: Int -> Data [Length] -> Shape sh

instance Shapely Z where
  zeroDim   = Z
  unitDim   = Z
  fakeShape = Z
  toShape _ _ = Z

instance Shapely sh => Shapely (sh :. Data Length) where
  zeroDim   = zeroDim   :. 0
  unitDim   = unitDim   :. 1
  fakeShape = fakeShape :. (P.error "You shall not inspect the syntax tree!")
  toShape i arr = toShape (i+1) arr :. (arr ! (P.fromIntegral i))

