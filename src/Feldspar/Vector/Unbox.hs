{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module Feldspar.Vector.Unbox where

import qualified Prelude as P

import Feldspar hiding ((!),P)
import qualified Feldspar.Vector.PullPush as Pl
import qualified Feldspar.Vector.MultiDim as M
import Feldspar.Vector.MultiDim (Slice(..),Any,All)
import Feldspar.Vector.Shape

import Language.Syntactic hiding (fold,desugar,sugar,P)

import Data.Proxy

-- | Non-nested vector
type Vector1 a = Vector (Data a)

-- | Two-level nested vector
type Vector2 a = Vector (Vector (Data a))

class Elm a where
  data Vector a
  type Repr a
  (!) :: Vector a -> Data Index -> a
  indexed :: (Data Index -> a) -> Data Length -> Vector a
  length :: Vector a -> Data Index
  freezeVector :: Vector a -> Data (Repr a)

instance Type a => Elm (Data a) where
  data Vector (Data a) = Pull (Pl.Pull (Data a))
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

instance (Elm a, Elm b, Elm c, Type (Repr a), Type (Repr b), Type (Repr c)) => Elm (a,b,c) where
  data Vector (a,b,c) = Triple { fstT :: Vector a, sndT :: Vector b, trdT :: Vector c }
  type Repr (a,b,c) = (Repr a, Repr b, Repr c)
  Triple va vb vc ! i = (va!i,vb!i,vc!i)
  indexed ixf l = Triple (indexed (fst3 . ixf) l) (indexed (snd3 . ixf) l) (indexed (trd3 . ixf) l)
  length vec = length (fstT vec)
  freezeVector (Triple v1 v2 v3) = desugar (freezeVector v1, freezeVector v2, freezeVector v3)

instance (Type (Repr a), Elm a) => Elm (Vector a) where
  data Vector (Vector a) = Nest (Pl.Pull (Vector a))
  type Repr   (Vector a) = [Repr a]
  Nest v ! i = Pl.index v i
  indexed ixf l = Nest (Pl.indexed l ixf)
  length (Nest v) = Pl.length v
  freezeVector (Nest vec) = Pl.freezePull (fmap freezeVector vec)

instance (Syntax a, Shapely sh) => Elm (M.Pull sh a) where
  data Vector (M.Pull sh a) = MP (M.Pull (sh :. Data Length) a)
  type Repr (M.Pull sh a) = [Internal a]
  (MP vec) ! i = M.slice vec (SAny ::. i)
  indexed ixf l = MP $ M.flatten $ M.indexed (\(Z :. ix) -> ixf ix) (Z :. l)
  length (MP (M.Pull _ (_ :. l))) = l
  freezeVector (MP vec) = snd (M.freezePull (fmap desugar vec))

instance Syntax a => Elm (Pl.Pull a) where
  data Vector (Pl.Pull a) = P (M.Pull DIM2 a)
  type Repr (Pl.Pull a) = [Internal a]
  (P vec) ! i = dim1ToPull $ M.slice vec (SAny ::. i)
  indexed ixf l = P $ M.flatten $ M.indexed (\(Z :. ix) -> pullToDIM1 $ ixf ix) (Z :. l)
  length (P (M.Pull _ (_ :. l))) = l
  freezeVector (P vec) = snd (M.freezePull (fmap desugar vec))

dim1ToPull :: M.Pull DIM1 a -> Pl.Pull a
dim1ToPull (M.Pull ixf (Z :. l)) = Pl.Pull (\i -> ixf (Z :. i)) l

pullToDIM1 :: Pl.Pull a -> M.Pull DIM1 a
pullToDIM1 (Pl.Pull ixf l) = M.Pull (\(Z :. ix) -> ixf ix) (Z :. l)

-- | @enumFromTo m n@: Enumerate the integers from @m@ to @n@
--
enumFromTo :: (Type a, Integral a) => Data a -> Data a -> Vector (Data a)
enumFromTo 1 n
    = indexed ((+1) . i2n) (i2n n)
enumFromTo m n = indexed ((+m) . i2n) (i2n l)
  where
    l = (n<m) ? 0 $ (n-m+1)

(...) :: (Type a, Integral a) => Data a -> Data a -> Vector (Data a)
(...) = enumFromTo

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

zip :: (Elm a, Elm b, Type (Repr a), Type (Repr b)) =>
       Vector a -> Vector b -> Vector (a,b)
zip = zipWith (\a b -> (a, b))

zip3 :: (Elm a, Elm b, Elm c, Type (Repr a), Type (Repr b), Type (Repr c)) =>
        Vector a -> Vector b -> Vector c -> Vector (a,b,c)
zip3 = zipWith3 (\a b c -> (a,b,c))

zipWith :: (Elm a, Elm b, Elm c) =>
           (a -> b -> c) -> Vector a -> Vector b -> Vector c
zipWith f vA vB = indexed (\ix -> f (vA ! ix) (vB ! ix))
                  (min (length vA) (length vB))

zipWith3 :: (Elm a, Elm b, Elm c, Elm d) =>
           (a -> b -> c -> d) -> Vector a -> Vector b -> Vector c -> Vector d
zipWith3 f vA vB vC = indexed (\ix -> f (vA ! ix) (vB ! ix) (vC ! ix))
                      (min3 (length vA) (length vB) (length vC))

-- | Corresponds to the standard 'foldl'.
fold :: (Elm b) => (a -> b -> a) -> a -> Vector b -> a
fold f x vec = P.error "fold: not implemented"

-- | Corresponds to the standard 'foldl1'.
fold1 :: (Elm a) => (a -> a -> a) -> Vector a -> a
fold1 f a = fold f (head a) (tail a)

maximum :: (Ord a, Elm (Data a)) => Vector (Data a) -> Data a
maximum = fold1 max

splitAt :: Elm a => Data Index -> Vector a -> (Vector a, Vector a)
splitAt n vec = (take n vec, drop n vec)

head :: Elm a => Vector a -> a
head = (!0)

last :: Elm a => Vector a -> a
last vec = vec ! (length vec - 1)

tail :: Elm a => Vector a -> Vector a
tail = drop 1

scan :: (Elm a, Elm b) => (a -> b -> a) -> a -> Vector b -> Vector a
scan f init bs = P.error "scan not implemented"
-- scan f init bs = sugar $ sequential (length bs) (desugar init) $ \i s ->
--    let s' = desugar $ f (sugar s) (bs!i)
--    in  (s',s')

-- TODO: Remove.
min3 a b c = min a (min b c)
min4 a b c d = min (min a b) (min c d)
fst3 (a,_,_) = a
snd3 (_,b,_) = b
trd3 (_,_,c) = c

-- Push vectors

class ElmP a where
  type RepP a
  type Arr  a
  allocArray  :: Proxy a -> Data Length -> M (Arr a)
  writeArray  :: Arr a -> ((Data Index -> a -> M ()) -> M ()) -> M ()
  freezePush  :: Proxy a -> Arr a -> M (RepP a)

data PushP a = ElmP a => PushP ((Data Index -> a -> M ()) -> M ()) (Data Length)

store :: forall a. ElmP a =>
         ((Data Index -> a -> M ()) -> M ()) ->
         Data Length ->
         M (RepP a)
store f l = do arr <- allocArray (Proxy :: Proxy a) l
               writeArray arr f
               freezePush (Proxy :: Proxy a) arr

storePush :: PushP a -> M (RepP a)
storePush (PushP f l) = store f l

instance Type a => ElmP (Data a) where
  type RepP (Data a) = Data [a]
  type Arr  (Data a) = Data (MArr a)
  allocArray _ = newArr_
  writeArray marr f = f (\i a -> setArr marr i a)
  freezePush _ arr = freezeArray arr

instance (ElmP a, ElmP b) => ElmP (a,b) where
  type RepP (a,b) = (RepP a, RepP b)
  type Arr  (a,b) = (Arr  a, Arr  b)
  allocArray (p :: Proxy (a,b)) l = do
    a1 <- allocArray (Proxy :: Proxy a) l
    a2 <- allocArray (Proxy :: Proxy b) l
    return (a1,a2)
  writeArray (arr1,arr2) f =
    f (\i (a,b) -> writeArray arr1 (\k -> k i a) >>
                   writeArray arr2 (\k -> k i b)
      )
  freezePush (p :: Proxy (a,b)) (arr1,arr2) = do
    a1 <- freezePush (Proxy :: Proxy a) arr1
    a2 <- freezePush (Proxy :: Proxy b) arr2
    return (a1,a2)

mapP :: ElmP b => (a -> b) -> PushP a -> PushP b
mapP f (PushP g l) = PushP (\k -> g (\i a -> k i (f a))) l

(++) :: PushP a -> PushP a -> PushP a
PushP f l ++ PushP g m = PushP (\k -> f k >> g k) (l + m)

data Push a = Push ((Data Index -> a -> M ()) -> M ()) (Data Length)

storeFlat :: forall a . (ElmP a, Syntax (RepP a)) => Push a -> RepP a
storeFlat (Push f l) = runMutable $ do
  arr <- allocArray (Proxy :: Proxy a) l
  writeArray arr f
  freezePush (Proxy :: Proxy a) arr
