{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances      #-}
module Feldspar.Vector.PullPush where

import qualified Prelude as P

import Feldspar hiding (sugar,desugar)
import qualified Feldspar as F

import Language.Syntactic (Syntactic(..))
import Data.Tuple.Select
import Data.Tuple.Curry

import Test.QuickCheck

-- * Pull Vectors

data Pull a where
  Pull :: (Data Index -> a) -> Data Length -> Pull a

type Pull1 a = Pull (Data a)

instance Syntax a => Syntactic (Pull a)
  where
    type Domain (Pull a)   = FeldDomain
    type Internal (Pull a) = [Internal a]
    desugar = desugar . freezePull
    sugar   = thawPull . sugar

-- | Store pull vectors in memory.
freezePull :: Syntax a => Pull a -> Data [Internal a]
freezePull (Pull ixf l) = parallel l (F.desugar. ixf)

-- | Create a pull vector from an array stored in memory.
thawPull :: Syntax a => Data [Internal a] -> Pull a
thawPull arr = Pull (\i -> F.sugar (getIx arr i)) (getLength arr)

instance Functor Pull where
  fmap f (Pull ixf l) = Pull (f . ixf) l

-- | Create a pull vector from an index function
indexed :: Data Length -> (Data Index -> a) -> Pull a
indexed l ixf = Pull ixf l

-- | Index into a pull vector
index :: Pull a -> Data Index -> a
index (Pull ixf _) i = ixf i

type instance Elem      (Pull a) = a
type instance CollIndex (Pull a) = Data Index
type instance CollSize  (Pull a) = Data Length

instance Indexed (Pull a) where
  Pull ixf l ! i = ixf i

instance Sized (Pull a) where
  collSize    = length
  setCollSize = newLen

instance CollMap (Pull a) (Pull b) where
  collMap = map

-- | Change the length of the vector to the supplied value. If the supplied
-- length is greater than the old length, the new elements will have undefined
-- value.
newLen :: Data Length -> Pull a -> Pull a
newLen l (Pull ixf _) = Pull ixf l

-- | `take n vec` takes the first `n` elements of `vec`
take :: Data Length -> Pull a -> Pull a
take n (Pull ixf l) = Pull ixf (min n l)

-- | `drop n vec` removes the first `n` elements of `vec`
drop :: Data Length -> Pull a -> Pull a
drop n (Pull ixf l) = Pull (ixf . (+n)) (l-n)

-- | Splits a pull vector in two at a particular index
splitAt :: Data Index -> Pull a -> (Pull a, Pull a)
splitAt n vec = (take n vec, drop n vec)

-- | Take the first element of a pull vector
head :: Pull a -> a
head (Pull ixf _) = ixf 0

-- | Take the last element of a pull vector
last :: Pull a -> a
last (Pull ixf l) = ixf (l-1)

-- | Remove the first element of a pull vector
tail :: Pull a -> Pull a
tail = drop 1

-- | Remove the last element of a pull vector
init :: Pull a -> Pull a
init vec = take (length vec - 1) vec

-- | Create a vector containing all the suffixes of the input vector
tails :: Pull a -> Pull (Pull a)
tails vec = indexed (length vec + 1) (`drop` vec)

-- | Create a vector containing all the prefixes of the input vector
inits :: Pull a -> Pull (Pull a)
inits vec = indexed (length vec + 1) (`take` vec)

-- | Similar to `inits` but without the empty vector
inits1 :: Pull a -> Pull (Pull a)
inits1 = tail . inits

-- | `roteateVecL n vec` rotates the elements of `vec` `n` steps to the left 
rotateVecL :: Data Index -> Pull a -> Pull a
rotateVecL ix = permute (\l i -> (i + ix) `rem` l)

-- | `roteateVecR n vec` rotates the elements of `vec` `n` steps to the right 
rotateVecR :: Data Index -> Pull a -> Pull a
rotateVecR ix = reverse . rotateVecL ix . reverse

-- | `replicate n a` creates a pull array containing `n` copies of `a`
replicate :: Data Length -> a -> Pull a
replicate n a = Pull (const a) n

-- | A vector which enumerates numbers consecutively
enumFromTo :: forall a. (Type a, Integral a)
           => Data a -> Data a -> Pull (Data a)
enumFromTo 1 n
    | IntType U _ <- typeRep :: TypeRep a
    = indexed (i2n n) ((+1) . i2n)
enumFromTo m n = indexed (i2n l) ((+m) . i2n)
  where
    l = (n<m) ? 0 $ (n-m+1)

(...) = enumFromTo

map = fmap

-- | Zipping pull vectors
zip :: Pull a -> Pull b -> Pull (a,b)
zip (Pull ixf1 l1) (Pull ixf2 l2) = Pull (\i -> (ixf1 i, ixf2 i)) (min l1 l2)

-- | Zipping three pull vectors
zip3 :: Pull a -> Pull b -> Pull c ->
        Pull (a,b,c)
zip3 (Pull ixf1 l1) (Pull ixf2 l2) (Pull ixf3 l3)
  = Pull (\i -> (ixf1 i, ixf2 i, ixf3 i)) (min (min l1 l2) l3)

-- | Zipping four pull vectors
zip4 (Pull ixf1 l1) (Pull ixf2 l2) (Pull ixf3 l3) (Pull ixf4 l4)
  = Pull (\i -> (ixf1 i, ixf2 i, ixf3 i, ixf4 i)) (min (min l1 l2) (min l3 l4))

zip5 (Pull ixf1 l1) (Pull ixf2 l2) (Pull ixf3 l3) (Pull ixf4 l4) (Pull ixf5 l5)
  = Pull (\i -> (ixf1 i, ixf2 i, ixf3 i, ixf4 i, ixf5 i)) (min (min (min l1 l2) (min l3 l4)) l5)

-- | Unzipping two pull vectors
unzip :: Pull (a,b) -> (Pull a, Pull b)
unzip v = (map sel1 v, map sel2 v)

-- | Unzipping three pull vectors
unzip3 :: Pull (a,b,c) -> (Pull a, Pull b, Pull c)
unzip3 v = (map sel1 v, map sel2 v, map sel3 v)

-- | Unzipping four pull vectors
unzip4 :: Pull (a,b,c,d) -> (Pull a, Pull b, Pull c, Pull d)
unzip4 v = (map sel1 v, map sel2 v, map sel3 v, map sel4 v)

-- | Unzipping five pull vectors
unzip5 :: Pull (a,b,c,d,e) -> (Pull a, Pull b, Pull c, Pull d, Pull e)
unzip5 v = (map sel1 v, map sel2 v, map sel3 v, map sel4 v, map sel5 v)

zipWith :: (a -> b -> c) -> Pull a -> Pull b ->
           Pull c
zipWith f a b = map (uncurryN f) $ zip a b

zipWith3 f a b c = map (uncurryN f) $ zip3 a b c

zipWith4 f a b c d = map (uncurryN f) $ zip4 a b c d

zipWith5 f a b c d e = map (uncurryN f) $ zip5 a b c d e

fold :: Syntax a => (a -> b -> a) -> a -> Pull b -> a
fold f x (Pull ixf l) = forLoop l x $ \ix s ->
                          f s (ixf ix)

fold1 :: Syntax a => (a -> a -> a) -> Pull a -> a
fold1 f a = fold f (head a) (tail a)

-- | Summing the elements of an array
sum :: (Syntax a, Num a) => Pull a -> a
sum = fold (+) 0

maximum, minimum :: Ord a => Pull (Data a) -> (Data a)
maximum = fold1 max

minimum = fold1 min

or, and :: Pull (Data Bool) -> Data Bool
or (Pull ixf l) = snd (whileLoop (0,false) (\(i,b) -> not b && i < l) body)
  where body (i,b) = (i+1,ixf i)

and (Pull ixf l) = snd (whileLoop (0,true) (\(i,b) -> b && i < l) body)
  where body (i,b) = (i+1,ixf i)

any, all :: (a -> Data Bool) -> Pull a -> Data Bool
any p = or . map p

all p = and . map p

eqVector :: Eq a => Pull (Data a) -> Pull (Data a) -> Data Bool
eqVector a b = length a == length b && and (zipWith (==) a b)

-- | Compute the scalar product of two pull vectors
scalarProd :: (Syntax a, Num a) => Pull a -> Pull a -> a
scalarProd a b = sum (zipWith (*) a b)

-- * Push Vector

data Push a where
  Push :: ((Data Index -> a -> M ()) -> M ()) -> Data Length -> Push a

type Push1 a = Push (Data a)

instance Syntax a => Syntactic (Push a)
  where
    type Domain (Push a)   = FeldDomain
    type Internal (Push a) = [Internal a]
    desugar = desugar . freezePush
    sugar   = thawPush . sugar

-- | Store push vectors in memory.
freezePush :: Syntax a => Push a -> Data [Internal a]
freezePush (Push _ 0) = parallel 0 (\i -> err "freezePush: indexing empty array")
freezePush (Push k l) = runMutableArray $ do
                          arr <- newArr_ l
                          k (\i a -> setArr arr i (resugar a))
                          return arr

-- | Freeze a vector and return a new Pull vector
freezeToPull :: (Pushy arr, Syntax a) => arr a -> Pull a
freezeToPull = thawPull . freezePush . toPush

-- | Create a push vector from an array stored in memory.
thawPush :: Syntax a => Data [Internal a] -> Push a
thawPush arr = Push f (getLength arr)
  where f k = forM (getLength arr) $ \ix ->
                k ix (resugar (arr ! ix))

-- | Any kind of vector, push or pull, can cheaply be converted to a push vector
class Pushy arr where
  toPush :: Syntax a => arr a -> Push a

instance Pushy Push where
  toPush = id

instance Pushy Pull where
  toPush vec = Push (\k -> forM (length vec) (\i -> k i (vec!i))) (length vec)

instance Functor Push where
  fmap f (Push g l) = Push (\k -> g (\i a -> k i (f a))) l

-- | Concatenating two arrays.
(++) :: (Pushy arr1, Pushy arr2, Syntax a) => arr1 a -> arr2 a -> Push a
v1 ++ v2 = Push (\func -> f func >>
                          g (\i a -> func (l1 + i) a))
                (l1 + l2)
  where
    Push f l1 = toPush v1
    Push g l2 = toPush v2

-- | Given an array of pairs, flatten the array so that the elements of the
--   pairs end up next to each other in the resulting vector.
unpair :: (Pushy arr, Syntax a) => arr (a,a) -> Push a
unpair arr = Push (\k -> f (everyOther k)) (2 * l)
  where
    Push f l = toPush arr

unpairWith :: (Pushy arr, Syntax a)
           => ((Data Index -> a -> M ()) -> Data Index -> (a,a) -> M ())
           -> arr (a,a) -> Push a
unpairWith spread arr = Push (f . spread) (2*l)
  where
    Push f l = toPush arr

everyOther :: (Data Index -> a -> M b)
           -> Data Index -> (a,a) -> M b
everyOther f = \ix (a1,a2) -> f (ix * 2) a1 >> f (ix * 2 + 1) a2

-- | Interleaves the elements of two vectors.
zipUnpair :: Syntax a => Pull a -> Pull a -> Push a
zipUnpair v1 v2 = unpair (zip v1 v2)

-- | Split a pull vector in half.
--
--   If the input vector has an odd length the second result vector
--   will be one element longer than the first.
halve :: Syntax a => Pull a -> (Pull a, Pull a)
halve v = (indexed (l `div` 2) ixf
          ,indexed ((l+1) `div` 2) (\i -> ixf (i + (l `div` 2))))
  where l   = length v
        ixf = (v!)

-- | Split a vector in half and interleave the two two halves.
riffle :: Syntax a => Pull a -> Push a
riffle = unpair . uncurry zip . halve

-- | This function can distribute array computations on chunks of a large
--   pull vector. A call `chunk l f g v` will split the vector `v` into chunks
--   of size `l` and apply `f` to these chunks. In case the length of `v` is
--   not a multiple of `l` then the rest of `v` will be processed by `g`.
chunk :: (Pushy arr1, Pushy arr2, Syntax b)
      => Data Length            -- ^ Size of the chunks
      -> (Pull a -> arr1 b) -- ^ Applied to every chunk
      -> (Pull a -> arr2 b) -- ^ Applied to the rest of the vector
      -> Pull a
      -> Push b
chunk c f g v = Push loop (noc * c)
             ++ toPush (g (drop (noc * c) v))
  where l = length v
        noc = l `div` c
        loop func = forM noc $ \i ->
                      do let (Push k _) = toPush $ f (take c (drop (c*i) v))
                         k (\j a -> func (c*i + j) a)

-- | `scanl` is similar to `fold`, but returns a `Push` of successive
-- reduced values from the left.
scanl :: (Syntax a, Syntax b)
      => (a -> b -> a) -> a -> Pull b -> Push a
scanl f init v = Push g l
  where
    l   = length v
    g k = do s <- newRef init
             forM l $ \ix -> do
               modifyRef s (`f` (v ! ix))
               getRef s >>= k ix

-- | The empty push vector.
empty :: Push a
empty = Push (const (return ())) 0

-- | Flattens a pull vector containing push vectors into an unnested push vector
--
--   Note that there are no restrictions on the lengths of the push vectors
--   inside the pull vector.
flatten :: Syntax a => Pull (Push a) -> Push a
flatten v = Push f len
  where len = sum (fmap length v)
        f k = do l <- newRef 0
                 forM (length v) $ \i ->
                   do let (Push g m) = v ! i
                      n <- getRef l
                      g (\j a -> k (n + j) a)
                      setRef l (n+m)

-- * Common functions

-- | An overloaded function for reordering elements of a vector.
class Permute arr where
  permute :: (Data Length -> Data Index -> Data Index) -> arr a -> arr a

instance Permute Pull where
  permute perm (Pull ixf l) = Pull (ixf . perm l) l

instance Permute Push where
  permute perm (Push f l) = Push (\k -> f (\i a -> k (perm l i) a)) l

-- | Permute the indexes of a vector
ixmap :: Permute arr =>
         (Data Index -> Data Index) -> arr a -> arr a
ixmap perm  = permute (const perm)

-- | Reverse a vector.
reverse :: (Permute arr, Len arr) =>
           arr a -> arr a
reverse arr = ixmap (\ix -> length arr - ix - 1) arr

-- | A class for overloading `length` for both pull and push vectors
class Len arr where
  length :: arr a -> Data Length

instance Len Pull where
  length (Pull _ l) = l

instance Len Push where
  length (Push _ l) = l

--------------------------------------------------------------------------------
-- Misc.
--------------------------------------------------------------------------------

tPull :: Patch a a -> Patch (Pull a) (Pull a)
tPull _ = id

tPull1 :: Patch a a -> Patch (Pull (Data a)) (Pull (Data a))
tPull1 _ = id

tPull2 :: Patch a a -> Patch (Pull (Pull (Data a))) (Pull (Pull (Data a)))
tPull2 _ = id

instance (Arbitrary (Internal a), Syntax a) => Arbitrary (Pull a)
  where
    arbitrary = fmap value arbitrary

instance Annotatable a => Annotatable (Pull a)
  where
    annotate info (Pull ixf len) = Pull
        (annotate (info P.++ " (vector element)") . ixf)
        (annotate (info P.++ " (vector length)") len)
