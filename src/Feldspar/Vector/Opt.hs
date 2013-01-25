module Feldspar.Vector.Opt where

import qualified Prelude as P

import Feldspar

data Vector a where
  Indexed   :: Data Length -> (Data Index -> a)  -> Vector a
  Stretch   :: Data Length -> Vector a           -> Vector a
  Repeat    :: Data Length -> Vector a           -> Vector a
  Arr       :: Type a => Data [a] -> Data Length -> Vector (Data a)
  Enum      :: Data Index -> Data Index          -> Vector (Data Index)
  Const     :: Data Length -> Data a             -> Vector a

  (:++:)    :: Vector a -> Vector a              -> Vector a
  (:==:)    :: Vector a -> Vector a              -> Vector a
  Concat    :: [Vector a]                        -> Vector a

length :: Vector a -> Data Length
length (Indexed l _) = l
length (Stretch s v) = s * length v
length (Repeat  r v) = r * length v
length (Arr _ l)     = l
length (Enum f t)    = t - f + 1
length (Const l _)   = l
length (v1 :++: v2)  = length v1 + length v2
length (Concat vs)   = P.sum (P.map length vs)

-- TODO: Use monad stuff here. Perhaps translate to push vector
freezeVector :: Type a => Vector (Data a) -> Data [a]
freezeVector (Indexed l ixf) = parallel l ixf
freezeVector _ = error "Unimplemented"

thawVector :: Type a => Data Length -> Data [a] -> Vector (Data a)
thawVector l arr = Arr arr l

instance Syntax a => Indexed (Vector a)
  where
    (!) = index

instance Syntax a => Sized (Vector a)
  where
    collSize    = length
    setCollSize = newLen

instance CollMap (Vector a) (Vector b)
  where
    collMap = map

instance Functor Vector where
  fmap = map

index :: Vector a -> Data Index -> a
index (Indexed _ ixf) i = ixf i
index (Stretch s vec) i = index (i `quot` s) vec
index (Repeat  r vec) i = index (i `rem`  r) vec
index (Arr arr _)     i = getIx arr i
index (Enum from to)  i = from + i
index (Const _ a)     _ = a
index (v1 :++: v2)    i = i < length v1 ? (index v1 i) (index v2 i)
index (v1 :==: v2)    i = i < length v1 ? (index v1 i) (index v2 i)
index (Concat vecs)   i = error "Need to think a litte"

indexed :: Data Length -> (Data Index -> a) -> Vector a
indexed l ixf = Indexed l ixf

map :: (a -> b) -> Vector a -> Vector b
map f (Indexed l ixf) = Indexed l (f . ixf)
map f (Stretch s vec) = Stretch s (map f vec)
map f (Repeat  r vec) = Repeat  r (map f vec)
map f (Const l a)     = Const l (f a)
map f (v1 :++: v2)    = map f v1 :++: map f v2
map f (v1 :==: v2)    = map f v1 :==: map f v2
map f (Concat vecs)   = Concat (P.map (map f) vecs)
map f vec             = indexed (length vec) (\ix -> f (vec ! ix))

enumFromTo :: Data Index -> Data Index -> Vector (Data Index)
enumFromTo from to = Enum from to

(...) :: Data Index -> Data Index -> Vector (Data Index)
(...) = enumFromTo

zipWith :: (Syntax a, Syntax b) =>
           (a -> b -> c) -> Vector a -> Vector b -> Vector c
zipWith f (Const l1 a1) (Const l2 a2) = Const (min l1 l2) (f a1 a2)
zipWith f vA vB = indexed (min (length vA) (length vB))
                  (\ix -> f (vA ! ix) (vB ! ix))

zip :: (Syntax a, Syntax b) => Vector a -> Vector b -> Vector (a,b)
zip = zipWith (\a b -> (a,b))

unzip :: Vector (a,b) -> (Vector a, Vector b)
unzip vec = (map sel1 vec, map sel2 vec)

repeat :: Syntax a => Data Length -> Vector a -> Vector a
repeat n (Repeat m vec) = Repeat (n*m) vec
repeat n (Const l a)    = Const (n*l) a
repeat n vec            = Repeat n vec

stretch :: Syntax a => Data Length -> Vector a -> Vector a
stretch n (Stretch m vec) = Stretch (n*m) vec
stretch n (Const l a)     = Const (n*l) a
stretch n vec             = Stretch n vec

take :: Syntax a => Data Length -> Vector a -> Vector a
take n (Enum from to) = Enum from (min (from + n) to)
take n (Const l a)    = Const (min l n) a
take n vec            = indexed (min n (length vec)) (vec!)

drop :: Syntax a => Data Length -> Vector a -> Vector a
drop n (Enum from to) = Enum (min (from + n) to) to
drop n (Const l a)    = Const (monus l n) a
drop n vec            = indexed (length vec < n ? (0,length vec - n))
                        (\ix -> vec ! (ix - n))

monus a b = b > a ? 0 (a-b)

splitAt :: Syntax a => Data Length -> Vector a -> (Vector a, Vector a)
splitAt n vec = (take n vec, drop n vec)

replicate :: Data Length -> a -> Vector a
replicate n a = Const n a

(++) :: Vector a -> Vector a -> Vector a
(++) = (:++:)

reverse :: Syntax a => Vector a -> Vector a
reverse (Stretch m vec) = Stretch m (reverse vec)
reverse (Repeat  m vec) = Repeat  m (reverse vec)
reverse (Const l a)     = Const l a
reverse (v1 :++: v2)    = reverse v2 :++: reverse v1
reverse (v1 :==: v2)    = reverse v2 :==: reverse v1
reverse (Concat vecs)   = Concat (P.reverse (P.map reverse vecs))
reverse vec             = indexed l (\ix -> vec ! (l - ix - 1))
  where l = length vec

fold :: (a -> b -> a) -> a -> Vector b -> a
fold f a (Indexed l ixf) = forLoop l a $ \ix s -> f s (ixf ix)
{-
  Stretch   :: Data Length -> Vector a           -> Vector a
  Repeat    :: Data Length -> Vector a           -> Vector a
  Arr       :: Type a => Data [a] -> Data Length -> Vector (Data a)
  Enum      :: Data Index -> Data Index          -> Vector (Data Index)
  Const     :: Data Length -> Data a             -> Vector a

  (:++:)    :: Vector a -> Vector a              -> Vector a
  (:==:)    :: Vector a -> Vector a              -> Vector a
  Concat    :: [Vector a]                        -> Vector a
-}

-- This one should be really efficiently implementable
reduce :: (a -> a -> a) -> a -> Vector a -> a
reduce f a (Indexed l ixf) = forLoop l a $ \ix s -> f s (ixf ix)
--reduce f a (Stretch s vec) = forLoop s (reduce f a vec) $ \ix s -> 

sum :: (Syntax a, Num a) => Vector a -> a
sum = fold (+) 0

maximum :: Ord a => Vector (Data a) -> Data a
maximum = fold1 max

minimum :: Ord a => Vector (Data a) -> Data a
minimum = fold1 min

or :: Vector (Data Bool) -> Data Bool
or = fold (||) false
  -- TODO Should be lazy

and :: Vector (Data Bool) -> Data Bool
and = fold (&&) true
  -- TODO Should be lazy

any :: (a -> Data Bool) -> Vector a -> Data Bool
any p = or . map p

all :: (a -> Data Bool) -> Vector a -> Data Bool
all p = and . map p

eqVector :: Eq a => Vector (Data a) -> Vector (Data a) -> Data Bool
eqVector a b = (length a == length b) && and (zipWith (==) a b)

scalarProd :: (Syntax a, Num a) => Vector a -> Vector a -> a
scalarProd a b = sum (zipWith (*) a b)

--------------------------------------------------------------------------------
-- Misc.
--------------------------------------------------------------------------------

tVec :: Patch a a -> Patch (Vector a) (Vector a)
tVec _ = id

tVec1 :: Patch a a -> Patch (Vector (Data a)) (Vector (Data a))
tVec1 _ = id

tVec2 :: Patch a a -> Patch (Vector (Vector (Data a))) (Vector (Vector (Data a)))
tVec2 _ = id

instance (Arbitrary (Internal a), Syntax a) => Arbitrary (Vector a)
  where
    arbitrary = fmap value arbitrary

instance Annotatable a => Annotatable (Vector a) where
  annotate info (Indexed len ixf) = Indexed
        (annotate (info P.++ " (vector length)") len)
        (annotate (info P.++ " (vector element)") . ixf)
  annotate info (Stretch s vec) = Stretch
                                  (annotate (info P.++ " (stretch)") s)
                                  (annotate (info P.++ " (vector stretch") vec)
  annotate info (Repeat r vec) = Repeat
                                 (annotate (info P.++ " (repeat)") r)
                                 (annotate (info P.++ " (vector repeat)") vec)
  annotate info (Arr arr len) = Arr
                                (annotate (info P.++ " (arr vector)") arr)
                                (annotate (info P.++ " (arr length)") len)
  annotate info (Enum from to) = Enum
                                 (annotate (info P.++ " (enum from)") from)
                                 (annotate (info P.++ " (enum to)") to)
  annotate info (Const l a) = Const
                              (annotate (info P.++ " (const length)") l)
                              (annotate (info P.++ " (const elem)") a)
  annotate info (v1 :++: v2) = annotate (info P.++ " (:++: left)")
                               :++:
                               annotate (info P.++ " (:++: right)")
  annotate info (v1 :==: v2) = annotate (info P.++ " (:==: left)")
                               :==:
                               annotate (info P.++ " (:==: right)")
  annotate info (Concat vecs) = zipWith ann vec [0..]
    where ann v i = annotate (info P.++ " (concat " ++ show i ++ ")") v
