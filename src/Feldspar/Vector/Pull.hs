{-# LANGUAGE GADTs #-}
module Feldspar.Vector.Pull where

import qualified Prelude

import Feldspar hiding (sugar,desugar)

import Lanugage.Syntactic (Syntactic(..))

data PullVector a where
  Pull :: (Data Index -> a) -> Data Length -> Pull a

type PullVector1 a = PullVector (Data a)

instance Syntax a => Syntactic (PullVector a)
  where
    type Domain (PullVector a)   = FeldDomain
    type Internal (PullVector a) = [Internal a]
    desugar = desugar . freezePull
    sugar   = thawPull . sugar

-- | Store pull vectors in memory.
freezePull :: Syntax a => PullVector a -> Data [Internal a]
freezePull (Pull ixf l) = parallel l (desugar. ixf)

-- | Create a push vector from an array stored in memory.
thawPull :: Syntax a => Data [Internal a] -> PullVector a
thawPull arr = Pull (\i -> getIx arr i) (getLength arr)

instance Functor PullVector where
  fmap f (Pull ixf l) = Pull (f . ixf) l

indexed :: Data Length -> (Data Index -> a) -> PullVector a
indexed l ixf = Pull ixf l

length :: PullVector a -> Data Length
length (Pull _ l) = l

instance Indexed (PullVector a) where
  Pull ixf l ! i = ixf i

instance Sized (PullVector a) where
  collSize    = length
  setCollSize = newLen

instance CollMap (PullVector a) (PullVector b) where
  collMap = map

-- | Change the length of the vector to the supplied value. If the supplied
-- length is greater than the old length, the new elements will have undefined
-- value.
newLen :: Data Length -> PullVector a -> PullVector a
newLen l (Pull ixf _) = Pull ixf l

take :: Data Length -> PullVector a -> PullVector a
take n (Pull ixf l) = Pull ixf (min n l)

drop :: Data Length -> PullVector a -> PullVector a
drop n (Pull ixf l) = Pull (ixf . (+n)) (l-n)

splitAt :: Data Index -> PullVector a -> (PullVector a, PullVector a)
splitAt n vec = (take n vec, drop n vec)

head :: PullVector a -> a
head (Pull ixf _) = ixf 0

last :: PullVector a -> a
last (Pull ixf l) = ixf (l-1)

tail :: PullVector a -> PullVector a
tail = drop 1

init :: PullVector a -> PullVector a
init vec = take (length vec - 1) vec

tails :: PullVector a -> PullVector (PullVector a)
tails vec = indexed (length vec + 1) (`drop` vec)

inits :: PullVector a -> PullVector (PullVector a)
inits vec = indexed (length vec + 1) (`take` vec)

inits1 :: PullVector a -> PullVector (PullVector a)
inits1 = tail . inits

permute :: (Data Length -> Data Index -> Data Index) -> PullVector a -> PullVector a
permute perm (Pull ixf l) = Pull (ixf . perm l) l

reverse :: PullVector a -> PullVector a
reverse = permute (\l i -> l - i - 1)

rotateVecL :: Data Index -> PullVector a -> PullVector a
rotateVecL ix = permute (\l i -> (i + ix) `rem` l)

rotateVecR :: Data Index -> PullVector a -> PullVector a
rotateVecR ix = reverse . rotateVecL ix . reverse

replicate :: Data Length -> a -> PullVector a
replicate n a = Pull (const a) n

enumFromTo = error "FIXME"

(...) = enumFromTo

map = fmap

zip :: PullVector a -> PullVector b -> PullVector (a,b)
zip (Pull ixf1 l1) (Pull ixf2 l2) = Pull (\i -> (ixf1 i, ixf2 i)) (min l1 l2)

zip3 :: PullVector a -> PullVector b -> PullVector c ->
        PullVector (a,b,c)
zip3 (Pull ixf1 l1) (Pull ixf2 l2) (Pull ixf3 l3)
  = Pull (\i -> (ixf1 i, ixf2 i, ixf3 i)) (min (min l1 l2) l3)

zip4 = error "FIXME"

zip5 = error "FIXME"

unzip :: PullVector (a,b) -> (PullVector a, PullVector b)
unzip vec = (map fst vec, map snd vec)

unzip3 :: PullVector (a,b,c) -> (PullVector a, PullVector b, PullVector c)
unzip3 vec = (map sel1 vec, map sel2 vec, map sel3 vec)

unzip4 = error "FIXME"

unzip5 = error "FIXME"

zipWith :: (a -> b -> c) -> PullVector a -> PullVector b ->
           PullVector c
zipWith f vecA vecB = map f (zip vecA vecB)

zipWith3 = error "FIXME"

zipWith4 = error "FIXME"

zipWith5 = error "FIXME"

fold :: (a -> b -> a) -> a -> PullVector b -> a
fold f x (Pull ixf l) = forLoop l x $ \ix s ->
                          f s (ixf ix)

fold1 :: (a -> a -> a) -> PullVector a -> a
fold1 f a = fold f (head a) (tail a)

sum = fold (+) 0

maximum = fold1 max

minimum = fold1 min

or = fold (||) false

and = fold (&&) true

any p = or . map p

all p = and . map p

eqVector :: Eq a => PullVector (Data a) -> PullVector (Data 1) -> Data Bool
eqVector a b = length a == length b && and (zipWith (==) a b)

scalarProd :: Num a => PullVector a -> PullVector a -> a
scalarProd a b = sum (zipWith (*) a b)

scan = error "FIXME"


--------------------------------------------------------------------------------
-- Misc.
--------------------------------------------------------------------------------

tVec :: Patch a a -> Patch (PullVector a) (PullVector a)
tVec _ = id

tVec1 :: Patch a a -> Patch (PullVector (Data a)) (PullVector (Data a))
tVec1 _ = id

tVec2 :: Patch a a -> Patch (PullVector (PullVector (Data a))) (PullVector (PullVector (Data a)))
tVec2 _ = id

instance (Arbitrary (Internal a), Syntax a) => Arbitrary (PullVector a)
  where
    arbitrary = fmap value arbitrary

instance Annotatable a => Annotatable (PullVector a)
  where
    annotate info (Pull len ixf = Pull
        (annotate (info Prelude.++ " (vector element)") . ixf)
        (annotate (info Prelude.++ " (vector length)") len)
