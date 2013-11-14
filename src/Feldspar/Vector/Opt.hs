{-# LANGUAGE GADTs #-}
module Feldspar.Vector.Opt where

import qualified Prelude as P

import Feldspar
import Feldspar.Vector.PullPush hiding (enumFromTo,zip,reverse,permute,
                                        take,drop,rotateVecL,toPush,(++))
import qualified Feldspar.Vector.PullPush as PP

data Vector a where
  PullV :: Pull a -> Vector a
  PushV :: Push a -> Vector a

-- Internal

toPush :: Syntax a => Vector a -> Push a
toPush (PullV pull) = PP.toPush pull
toPush (PushV push) = push

-- allocates memory
pushToPull :: Syntax a => Push a -> Pull a
pushToPull push = (thawPull (freezePush push))

-- May allocate memory
toPull :: Syntax a => Vector a -> Pull a
toPull (PullV pull) = pull
toPull (PushV push) = pushToPull push

-- API

enumFromTo :: Data Index -> Data Index -> Vector (Data Index)
enumFromTo from to = PullV (PP.enumFromTo from to)

index :: Type a => Vector (Data a) -> Data Index -> Data a
index (PullV pull) ix = pull ! ix
index (PushV push) ix = getIx (freezePush push) ix

(++) :: Syntax a => Vector a -> Vector a -> Vector a
vec1 ++ vec2 = PushV $ toPush vec1 PP.++ toPush vec2

take :: Syntax a => Data Length -> Vector a -> Vector a
take n (PullV vec) = PullV $ PP.take n vec
take n (PushV vec) = PullV $ PP.take n (thawPull (freezePush vec))

drop :: Syntax a => Data Length -> Vector a -> Vector a
drop n (PullV vec) = PullV $ PP.drop n vec
drop n (PushV vec) = PullV $ PP.drop n (thawPull (freezePush vec))

splitAt :: Syntax a => Data Index -> Vector a -> (Vector a, Vector a)
splitAt n vec = (take n vec, drop n vec)

permute :: (Data Length -> Data Index -> Data Index) ->
           Vector a -> Vector a
permute perm (PullV vec) = PullV $ PP.permute perm vec
permute perm (PushV vec) = PushV $ PP.permute perm vec

reverse :: Syntax a => Vector a -> Vector a
reverse = permute $ \l i -> l-1-i

rotateVecL :: Syntax a => Data Index -> Vector a -> Vector a
rotateVecL ix = permute $ \l i -> (i + ix) `rem` l

rotateVecR :: Syntax a => Data Index -> Vector a -> Vector a
rotateVecR ix = reverse . rotateVecL ix . reverse

replicate :: Data Length -> a -> Vector a
replicate n a = PullV (PP.replicate n a)

zip :: (Syntax a,Syntax b) => Vector a -> Vector b -> Vector (a,b)
zip v1 v2 = PullV $ PP.zip (toPull v1) (toPull v2)

