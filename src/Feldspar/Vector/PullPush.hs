{-# LANGUAGE GADTs #-}
module Feldspar.Vector.PullPush where

import qualified Prelude as P

import Feldspar
import qualified Feldspar.Vector.Pull as Pl
import qualified Feldspar.Vector.Push as Ph

data Vector a where
  Pull :: Pl.PullVector a -> Vector a
  Push :: Ph.PushVector a -> Vector a

-- Internal

toPush :: Syntax a => Vector a -> Ph.PushVector a
toPush (Pull pull) = Ph.toPush pull
toPush (Push push) = push

-- allocates memory
pushToPull :: Syntax a => Ph.PushVector a -> Pl.PullVector a
pushToPull push = (Pl.thawPull (Ph.freezePush push))

-- May allocate memory
toPull :: Syntax a => Vector a -> Pl.PullVector a
toPull (Pull pull) = pull
toPull (Push push) = pushToPull push

-- API

enumFromTo :: Data Index -> Data Index -> Vector (Data Index)
enumFromTo from to = Pull (Pl.enumFromTo from to)

index :: Type a => Vector (Data a) -> Data Index -> Data a
index (Pull pull) ix = pull ! ix
index (Push push) ix = getIx (Ph.freezePush push) ix

(++) :: Syntax a => Vector a -> Vector a -> Vector a
vec1 ++ vec2 = Push $ toPush vec1 Ph.++ toPush vec2

take :: Syntax a => Data Length -> Vector a -> Vector a
take n (Pull vec) = Pull $ Pl.take n vec
take n (Push vec) = Pull $ Pl.take n (Pl.thawPull (Ph.freezePush vec))

drop :: Syntax a => Data Length -> Vector a -> Vector a
drop n (Pull vec) = Pull $ Pl.drop n vec
drop n (Push vec) = Pull $ Pl.drop n (Pl.thawPull (Ph.freezePush vec))

splitAt :: Syntax a => Data Index -> Vector a -> (Vector a, Vector a)
splitAt n vec = (take n vec, drop n vec)

permute :: (Data Length -> Data Index -> Data Index) ->
           Vector a -> Vector a
permute perm (Pull vec) = Pull $ Pl.permute perm vec
permute perm (Push vec) = Push $ Ph.permute perm vec

reverse :: Syntax a => Vector a -> Vector a
reverse = permute $ \l i -> l-1-i

rotateVecL :: Syntax a => Data Index -> Vector a -> Vector a
rotateVecL ix = permute $ \l i -> (i + ix) `rem` l

rotateVecR :: Syntax a => Data Index -> Vector a -> Vector a
rotateVecR ix = reverse . rotateVecL ix . reverse

replicate :: Data Length -> a -> Vector a
replicate n a = Pull (Pl.replicate n a)

zip :: (Syntax a,Syntax b) => Vector a -> Vector b -> Vector (a,b)
zip v1 v2 = Pull $ Pl.zip (toPull v1) (toPull v2)

