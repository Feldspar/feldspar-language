module Feldspar.Vector.PullPush where

import qualified Prelude as P

import Feldspar
import qualified Feldspar.Vector.Pull as Pl
import qualified Feldspar.Vector.Push as Ph

data Vector a where
  Pull :: Pl.PullVector a -> Vector a
  Push :: Ph.PushVector a -> Vector a

-- Internal

toPush :: Vector a -> Ph.PushVector a
toPush (Pull pull) = Ph.toPush pull
toPush (Push push) = push

-- allocates memory
pushToPull :: Ph.PushVector a -> Pl.PullVector a
pushToPull push = (Pl.thawPull (Ph.freezePush push))

-- May allocate memory
toPull :: Vector a -> Pl.PullVector a
toPull (Pull pull) = pull
toPull (Push push) = pushToPull push

-- API

enumFromTo :: Data Index -> Data Index -> Vector (Data Index)
enumFromTo from to = Pull (Ph.enumFromTo from to)

index :: Vector a -> Data Index -> a
index (Pull pull) ix = pull ! ix
index (Push push) ix = getIx (Ph.freezePush push) ix

(++) :: Vector a -> Vector a -> Vector a
vec1 ++ vec2 = toPush vec1 Ph.++ toPush vec2

take :: Data Length -> Vector a -> Vector a
take n (Pull vec) = Pl.take n vec
take n (Push vec) = Pl.take n (Pl.thawPull (Ph.freezePush vec))

drop :: Data Length -> Vector a -> Vector a
drop n (Pull vec) = Pl.drop n vec
drop n (Push vec) = Pl.drop n (Pl.thawPull (Ph.freezePush vec))

splitAt :: Data Index -> Vector a -> (Vector a, Vector a)
splitAt n vec = (take n vec, drop n vec)

permute :: (Data Length -> Data Index -> Data Index) ->
           Vector a -> Vector a
permute perm (Pull vec) = Pl.permute perm vec
permute perm (Push vec) = Ph.permute perm vec

reverse :: Syntax a => Vector a -> Vector a
reverse = permute $ \l i -> l-1-i

rotateVecL :: Syntax a => Data Index -> Vector a -> Vector a
rotateVecL ix = permute $ \l i -> (i + ix) `rem` l

rotateVecR :: Syntax a => Data Index -> Vector a -> Vector a
rotateVecR ix = reverse . rotateVecL ix . reverse

replicate :: Data Length -> a -> Vector a
replicate n a = Pull (Pl.replicate n a)

zip :: Vector a -> Vector b -> Vector (a,b)
zip v1 v2 = Pl.zip (toPull v1) (toPull v2)

