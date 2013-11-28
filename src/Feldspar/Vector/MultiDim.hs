{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ConstraintKinds       #-}
module Feldspar.Vector.MultiDim where

import qualified Prelude as P

import Language.Syntactic hiding (fold,size)
import Feldspar hiding (desugar,sugar,resugar)
import qualified Feldspar as F
import Feldspar.Vector.Shape

import Data.Tuple.Select
import Data.Tuple.Curry
import Control.Monad (zipWithM_)

-- | * Slices

data All    = All
data Any sh = Any

data Slice ss where
  SZ    :: Slice Z
  (::.) :: Slice sl -> Data Length -> Slice (sl :. Data Length)
  (:::) :: Slice sl -> All -> Slice (sl :. All)
  SAny  :: Slice (Any sl)


type family FullShape ss
type instance FullShape Z                   = Z
type instance FullShape (Any sh)            = sh
type instance FullShape (sl :. Data Length) = FullShape sl :. Data Length
type instance FullShape (sl :. All)         = FullShape sl :. Data Length

type family SliceShape ss
type instance SliceShape Z                   = Z
type instance SliceShape (Any sh)            = sh
type instance SliceShape (sl :. Data Length) = SliceShape sl
type instance SliceShape (sl :. All)         = SliceShape sl :. Data Length

sliceOfFull :: Slice ss -> Shape (FullShape ss) -> Shape (SliceShape ss)
sliceOfFull SZ Z = Z
sliceOfFull SAny sh = sh
sliceOfFull (fsl ::. _) (ssl :. _) = sliceOfFull fsl ssl
sliceOfFull (fsl ::: All) (ssl :. s) = sliceOfFull fsl ssl :. s

fullOfSlice :: Slice ss -> Shape (SliceShape ss) -> Shape (FullShape ss)
fullOfSlice SZ Z = Z
fullOfSlice SAny sh = sh
fullOfSlice (fsl ::. n) ssl = fullOfSlice fsl ssl :. n
fullOfSlice (fsl ::: All) (ssl :. s) = fullOfSlice fsl ssl :. s

-- | * Pull Vectors

data Pull sh a = Pull (Shape sh -> a) (Shape sh)

type DPull sh a = Pull sh (Data a)

instance Functor (Pull sh)
  where
    fmap f (Pull ixf sh) = Pull (f . ixf) sh


-- | * Functions

-- | Store a vector in an array.
fromPull :: (Type a) => DPull sh a -> Data [a]
fromPull vec = parallel (size ext) (\ix -> vec !: fromIndex ext ix)
  where ext = extent vec


-- | Restore a vector from an array
arrToPull :: (Type a) => Shape sh -> Data [a] -> DPull sh a
arrToPull sh arr = Pull (\ix -> arr ! toIndex sh ix) sh

freezePull :: (Type a) => DPull sh a -> (Data [Length], Data [a])
freezePull v   = (shapeArr, fromPull v) -- TODO should be fromPull' to remove div and mod
  where shapeArr = fromList (toList $ extent v)

fromList :: Type a => [Data a] -> Data [a]
fromList ls = loop 1 (parallel (value len) (const (P.head ls)))
  where loop i arr
            | i P.< len = loop (i+1) (setIx arr (value i) (ls P.!! (P.fromIntegral i)))
            | otherwise = arr
        len  = P.fromIntegral $ P.length ls
{-
thawPull :: (Type a, Shapely sh) => (Data [Length], Data [a]) -> DPull sh a
thawPull (l,arr) = toPull (toShape 0 l) arr
-}
{-
-- | Store a vector in memory. Use this function instead of 'force' if
--   possible as it is both much more safe and faster.
memorize :: (Type a) => DPull sh a -> DPull sh a
memorize vec = toPull (extent vec) (fromPull vec)
-}
-- | A shape-aware version of parallel (though this implementation is
--   sequental).
parShape :: (Type a) => Shape sh -> (Shape sh -> Data a) -> Data [a]
parShape sh ixf = runMutableArray $ do
                   arr <- newArr_ (size sh)
                   forShape sh $ \i -> do
                     setArr arr (toIndex sh i) (ixf i)
                   return arr

-- | An alternative version of `fromVector` which uses `parShape`
fromVector' :: (Type a) => DPull sh a -> Data [a]
fromVector' (Pull ixf sh) = parShape sh ixf

-- | Change the extent of the vector to the supplied value. If the supplied
-- extent will contain more elements than the old extent, the new elements
-- will have undefined value.
newExtent :: Shape sh -> Pull sh a -> Pull sh a
newExtent sh (Pull ixf _) = Pull ixf sh

indexed :: (Shape sh -> a) -> Shape sh -> Pull sh a
indexed ixf l = Pull ixf l

-- | Change shape and transform elements of a vector. This function is the
--   most general way of manipulating a vector.
traverse :: Pully vec =>
            vec sh  a -> (Shape sh -> Shape sh') ->
            ((Shape sh -> a) -> Shape sh' -> a') ->
            Pull sh' a'
traverse vec shf elemf
  = Pull (elemf ixf) (shf sh)
  where Pull ixf sh = toPull vec

-- | Duplicates part of a vector along a new dimension.
replicate :: Pully vec => Slice ss -> vec (SliceShape ss) a -> Pull (FullShape ss) a
replicate sl vec
 = backpermute (fullOfSlice sl (extent vec))
               (sliceOfFull sl) (toPull vec)

-- | Extracts a slice from a vector.
slice :: Pully vec => vec (FullShape ss) a -> Slice ss -> Pull (SliceShape ss) a
slice vec sl
 = backpermute (sliceOfFull sl (extent vec))
               (fullOfSlice sl) vec

-- | Change the shape of a vector. This function is potentially unsafe, the
--   new shape need to have fewer or equal number of elements compared to
--   the old shape.
reshape :: Pully vec => Shape sh -> vec sh' a -> Pull sh a
reshape sh' vec
 = Pull (ixf . fromIndex sh . toIndex sh') sh'
  where Pull ixf sh = toPull vec

-- | A scalar (zero dimensional) vector
unit :: a -> Pull Z a
unit a = Pull (const a) Z

-- | Index into a vector
(!:) :: Pully vec => vec sh a -> Shape sh -> a
vec !: ix = ixf ix
  where Pull ixf _ = toPull vec

-- | Extract the diagonal of a two dimensional vector
diagonal :: Pully vec => vec DIM2 a -> Pull DIM1 a
diagonal vec = backpermute (Z :. width) (\ (_ :. x) -> Z :. x :. x) vec
  where (width : height : _) = toList (extent vec) -- brain explosion hack

-- | Change the shape of a vector.
backpermute :: Pully vec =>
  Shape sh' -> (Shape sh' -> Shape sh) ->
  vec sh a -> Pull sh' a
backpermute sh perm vec = traverse (toPull vec) (const sh) (. perm)

-- | Combines the elements of two vectors. The size of the resulting vector
--   will be the intersection of the two argument vectors.
zip :: (Pully vec1, Pully vec2) => vec1 sh a -> vec2 sh b -> Pull sh (a,b)
zip = zipWith (\a b -> (a,b))

-- | Combines the elements of two vectors pointwise using a function.
--   The size of the resulting vector will be the intersection of the
--   two argument vectors.
zipWith :: (Pully vec1, Pully vec2) =>
           (a -> b -> c) -> vec1 sh a -> vec2 sh b -> Pull sh c
zipWith f arr1 arr2 = Pull (\ix -> f (arr1 !: ix) (arr2 !: ix))
                           (intersectDim (extent arr1) (extent arr2))

zip3 :: (Pully vec1, Pully vec2, Pully vec3) => 
        vec1 sh a -> vec2 sh b -> vec3 sh c ->
        Pull sh (a,b,c)
zip3 vec1 vec2 vec3
  = Pull (\i -> (ixf1 i, ixf2 i, ixf3 i))
         (intersectDim (intersectDim sh1 sh2) sh3)
  where Pull ixf1 sh1 = toPull vec1
        Pull ixf2 sh2 = toPull vec2
        Pull ixf3 sh3 = toPull vec3

-- | Zipping four pull vectors
zip4 :: (Pully vec1, Pully vec2, Pully vec3, Pully vec4) => 
        vec1 sh a -> vec2 sh b -> vec3 sh c -> vec4 sh d ->
        Pull sh (a,b,c,d)
zip4 vec1 vec2 vec3 vec4
  = Pull (\i -> (ixf1 i, ixf2 i, ixf3 i, ixf4 i)) (intersectDim (intersectDim sh1 sh2) (intersectDim sh3 sh4))
  where Pull ixf1 sh1 = toPull vec1
        Pull ixf2 sh2 = toPull vec2
        Pull ixf3 sh3 = toPull vec3
        Pull ixf4 sh4 = toPull vec4

zip5 :: (Pully vec1, Pully vec2, Pully vec3, Pully vec4, Pully vec5) => 
        vec1 sh a -> vec2 sh b -> vec3 sh c -> vec4 sh d -> vec5 sh e ->
        Pull sh (a,b,c,d,e)
zip5 vec1 vec2 vec3 vec4 vec5
  = Pull (\i -> (ixf1 i, ixf2 i, ixf3 i, ixf4 i, ixf5 i))
         (intersectDim (intersectDim (intersectDim sh1 sh2) (intersectDim sh3 sh4)) sh5)
  where Pull ixf1 sh1 = toPull vec1
        Pull ixf2 sh2 = toPull vec2
        Pull ixf3 sh3 = toPull vec3
        Pull ixf4 sh4 = toPull vec4
        Pull ixf5 sh5 = toPull vec5

-- | Unzipping two pull vectors
unzip :: Functor vec => vec (a,b) -> (vec a, vec b)
unzip v = (fmap sel1 v, fmap sel2 v)

-- | Unzipping three pull vectors
unzip3 :: Functor vec => vec (a,b,c) -> (vec a, vec b, vec c)
unzip3 v = (fmap sel1 v, fmap sel2 v, fmap sel3 v)

-- | Unzipping four pull vectors
unzip4 :: Functor vec => vec (a,b,c,d) -> (vec a, vec b, vec c, vec d)
unzip4 v = (fmap sel1 v, fmap sel2 v, fmap sel3 v, fmap sel4 v)

-- | Unzipping five pull vectors
unzip5 :: Functor vec => vec (a,b,c,d,e) -> (vec a, vec b, vec c, vec d, vec e)
unzip5 v = (fmap sel1 v, fmap sel2 v, fmap sel3 v, fmap sel4 v, fmap sel5 v)

-- | Reduce a vector along its last dimension

fold :: (Syntax a, Pully vec) =>
        (a -> a -> a)
     -> a
     -> vec (sh :. Data Length) a
     -> Pull sh a
fold f x vec = Pull ixf sh
    where (sh, n) = uncons (extent vec) -- brain explosion hack
          ixf i = forLoop n x (\ix s -> f s (vec !: (i :. ix)))

-- Here's another version of fold which has a little bit more freedom
-- when it comes to choosing the initial element when folding

-- | A generalization of 'fold' which allows for different initial
--   values when starting to fold.
fold' :: (Syntax a, Pully vec1, Pully vec2)
      => (a -> a -> a)
      -> vec1 sh a
      -> vec2 (sh :. Data Length) a
      -> Pull sh a
fold' f x vec = Pull ixf sh
    where (sh, n) = uncons (extent vec) -- brain explosion hack
          ixf i = forLoop n (x!:i) (\ix s -> f s (vec !: (i :. ix)))

-- | Summing a vector along its last dimension
sum :: (Syntax a, Num a, Pully vec) => vec (sh :. Data Length) a -> Pull sh a
sum = fold (+) 0

-- | Concatenating shapes.
class ShapeConc sh1 sh2 where
  type ShapeConcT sh1 sh2
  shapeConc :: Shape sh1 -> Shape sh2 -> Shape (ShapeConcT sh1 sh2)

  splitIndex :: Shape (ShapeConcT sh1 sh2) -> Shape sh1 -> (Shape sh1,Shape sh2)

instance ShapeConc Z sh2 where
  type ShapeConcT Z sh2 = sh2
  shapeConc Z sh2 = sh2

  splitIndex sh Z = (Z,sh)

instance ShapeConc sh1 sh2 => ShapeConc (sh1 :. Data Length) sh2 where
  type ShapeConcT (sh1 :. Data Length) sh2 = ShapeConcT sh1 sh2 :. Data Length
  shapeConc (sh1 :. l) sh2 = shapeConc sh1 sh2 :. l

  splitIndex (sh :. i) (sh1 :. _) = (i1 :. i,i2)
    where (i1,i2) = splitIndex sh sh1

-- | Flatten nested vectors.
flatten :: forall a sh1 sh2.
           Shapely (ShapeConcT sh1 sh2) =>
          ShapeConc sh1 sh2 => Pull sh1 (Pull sh2 a)
       -> Pull (ShapeConcT sh1 sh2) a
flatten (Pull ixf1 sh1) = Pull ixf sh
  where ixf i = let (i1,i2) = splitIndex i sh1
  	       	    (Pull ixf2 _) = ixf1 i1
  	       	in ixf2 i2
        sh = let (i1,_ :: Shape sh2) = splitIndex fakeShape sh1
	         (Pull _ sh2) = ixf1 i1
	     in shapeConc sh1 sh2

-- Laplace

stencil :: Pully vec => vec DIM2 (Data Float) -> DPull DIM2 Float
stencil vec
  = traverse vec id update
  where
    (width : height : _) = toList (extent vec) -- brain explosion hack

    update get d@(sh :. i :. j)
      = isBoundary i j ?
        get d
        $ (get (sh :. (i-1) :. j)
         + get (sh :. i     :. (j-1))
         + get (sh :. (i+1) :. j)
         + get (sh :. i     :. (j+1))) / 4

    isBoundary i j
      =  (i == 0) || (i >= width  - 1)
      || (j == 0) || (j >= height - 1)

laplace :: Storable vec => Data Length -> vec DIM2 (Data Float) -> Manifest DIM2 (Data Float)
laplace steps vec = forLoop steps (store vec) (\ix ->
                       store . stencil
                    )


-- Matrix Multiplication

transposeL :: forall sh e vec. Pully vec =>
             vec  (sh :. Data Length :. Data Length) e
          -> Pull (sh :. Data Length :. Data Length) e
transposeL vec
  = backpermute new_extent swap vec
  where swap ((tail :: Shape sh) :. i :. j) = tail :. j :. i
        new_extent         = swap (extent vec)

transpose2D :: Pully vec => vec DIM2 e -> Pull DIM2 e
transpose2D = transposeL

-- | Matrix multiplication
mmMult :: (Syntax e, Num e, Pully vec1, Pully vec2) =>
          vec2 DIM2 e -> vec2 DIM2 e -> Pull DIM2 e
mmMult vA vB
  = sum (zipWith (*) vaRepl vbRepl)
  where
    tmp = transpose2D vB
    vaRepl = replicate (SZ ::: All   ::. colsB ::: All) vA
    vbRepl = replicate (SZ ::. rowsA ::: All   ::: All)  vB
    [rowsA, colsA] = toList (extent vA) -- brain explosion hack
    [rowsB, colsB] = toList (extent vB)


-- KFFs combinators

expandL :: Data Length -> Pull (sh :. Data Length) a -> Pull (sh :. Data Length :. Data Length) a
expandL n (Pull ixf ext) = Pull ixf' (insLeft n $ insLeft p $ ext')
  where (m, ext') = peelLeft ext
        p = m `div` n
        ixf' ix = let (i,ix') = peelLeft ix; (j,ix'') = peelLeft ix' in ixf $ insLeft (i*p + j) ix''

contractL :: Pull (sh :. Data Length :. Data Length) a -> Pull (sh :. Data Length) a
contractL (Pull ixf ext) = Pull ixf' (insLeft (m*n) ext')
  where (m, n, ext') = peelLeft2 ext
        ixf' ix = let (i,ix') = peelLeft ix in ixf $ insLeft (i `div` n) $ insLeft (i `mod` n) $ ix'

transL :: Pull (sh :. Data Length :. Data Length) a -> Pull (sh :. Data Length :. Data Length) a
transL (Pull ixf ext) = Pull ixf' (insLeft n $ insLeft m $ ext')
  where (m, n, ext') = peelLeft2 ext
        ixf' ix = let (i, j, ix') = peelLeft2 ix in ixf $ insLeft j $ insLeft i $ ix'


-- Note: curry is unsafe in that it produces an index function that does not check that its leftmost argument is in range
curryL :: Pull (sh :. Data Length) a -> (Data Length, Data Length -> Pull sh a)
curryL (Pull ixf sh) = (n, \ i -> Pull (\ ix -> ixf $ insLeft i ix) sh')
  where (n, sh') = peelLeft sh

uncurryL :: Data Length -> (Data Length -> Pull sh a) -> Pull (sh :. Data Length) a
uncurryL m f = Pull ixf (insLeft m ext)
  where Pull _ ext = f (undefined :: Data Length)
        ixf ix = let (i, ix') = peelLeft ix; Pull ixf' _ = f i in ixf' ix'

dmapL :: (Pull sh1 a1 -> Pull sh2 a2) -> Pull (sh1 :. Data Length) a1 -> Pull (sh2 :. Data Length) a2
dmapL f a = uncurryL n $ f . g
  where (n,g) = curryL a

dzipWithL :: (Pull sh1 a1 -> Pull sh2 a2 -> Pull sh3 a3) -> Pull (sh1 :. Data Length) a1 -> Pull (sh2 :. Data Length) a2
          -> Pull (sh3 :. Data Length) a3
dzipWithL f a1 a2 = uncurryL (min m n) $ \ i -> f (g i) (h i)
  where (m,g) = curryL a1
        (n,h) = curryL a2

-- Convenience functions that maybe should not be in the lib

expandLT :: Data Length -> Pull (sh :. Data Length) a -> Pull (sh :. Data Length :. Data Length) a
expandLT n a = transL $ expandL n $ a

contractLT :: Pull (sh :. Data Length :. Data Length) a -> Pull (sh :. Data Length) a
contractLT a = contractL $ transL $ a



{-

Here is some functions that use both pull and push vectors (pull in, push out). Hence they do not build in the current module structure,
so I include them in the form of comments. Should be uncommentable in a joint module, modulo renaming ;-)

Only difference to the pure pull variants is the use of uncurryS instead of uncurryL.
-}
dmapS :: (Pull sh1 a1 -> Push sh2 a2) -> Pull (sh1 :. Data Length) a1 -> Push (sh2 :. Data Length) a2
dmapS f a = uncurryS n $ f . g
  where (n,g) = curryL a

dzipWithS :: (Pull sh1 a1 -> Pull sh2 a2 -> Push sh3 a3) -> Pull (sh1 :. Data Length) a1 -> Pull (sh2 :. Data Length) a2
          -> Push (sh3 :. Data Length) a3
dzipWithS f a1 a2 = uncurryS (min m n) $ \ i -> f (g i) (h i)
  where (m,g) = curryL a1
        (n,h) = curryL a2


-- | * Functions on one dimensional Pull vectors

indexed1 :: Data Length -> (Data Index -> a) -> Pull DIM1 a
indexed1 l ixf = Pull (\(Z :. i) -> ixf i) (Z :. l)

length :: Shaped vec => vec DIM1 a -> Data Length
length vec = l
  where (Z,l) = uncons (extent vec)

take :: Pully vec => Data Length -> vec DIM1 a -> Pull DIM1 a
take n vec = Pull ixf (Z :. (min n l))
  where Pull ixf sh = toPull vec
        (Z,l)       = uncons sh

-- | `drop n vec` removes the first `n` elements of `vec`
drop :: Pully vec => Data Length -> vec DIM1 a -> Pull DIM1 a
drop n vec = Pull (\(Z :. i) -> ixf (Z :. i+n)) (Z :. (l-n))
  where Pull ixf sh = toPull vec
        (Z,l)       = uncons sh

-- | Splits a pull vector in two at a particular index
splitAt :: Pully vec => Data Index -> vec DIM1 a -> (Pull DIM1 a, Pull DIM1 a)
splitAt n vec = (take n vec, drop n vec)

-- | Take the first element of a pull vector
head :: Pully vec => vec DIM1 a -> a
head vec = ixf (Z :. 0)
  where Pull ixf _ = toPull vec

-- | Take the last element of a pull vector
last :: Pully vec => vec DIM1 a -> a
last vec = ixf (Z :. (l-1))
  where Pull ixf sh = toPull vec
        (Z,l)       = uncons sh

-- | Remove the first element of a pull vector
tail :: Pully vec => vec DIM1 a -> Pull DIM1 a
tail = drop 1

-- | Remove the last element of a pull vector
init :: Pully vec => vec DIM1 a -> Pull DIM1 a
init vec = take (length vec - 1) vec

-- | Create a vector containing all the suffixes of the input vector
tails :: Pully vec => vec DIM1 a -> Pull DIM1 (Pull DIM1 a)
tails vec = indexed1 (length vec + 1) (`drop` vec)

-- | Create a vector containing all the prefixes of the input vector
inits :: Pully vec => vec DIM1 a -> Pull DIM1 (Pull DIM1 a)
inits vec = indexed1 (length vec + 1) (`take` vec)

-- | Similar to `inits` but without the empty vector
inits1 :: Pully vec => vec DIM1 a -> Pull DIM1 (Pull DIM1 a)
inits1 = tail . inits

-- | `roteateVecL n vec` rotates the elements of `vec` `n` steps to the left 
rotateVecL :: Data Index -> Pull DIM1 a -> Pull DIM1 a
rotateVecL ix = permute1 (\l i -> (i + ix) `rem` l)

-- | `roteateVecR n vec` rotates the elements of `vec` `n` steps to the right 
rotateVecR :: Data Index -> Pull DIM1 a -> Pull DIM1 a
rotateVecR ix = reverse . rotateVecL ix . reverse

-- | `replicate1 n a` creates a one dimensional pull array 
--    containing `n` copies of `a`
replicate1 :: Data Length -> a -> Pull DIM1 a
replicate1 n a = Pull (const a) (Z :. n)

-- | A vector which enumerates numbers consecutively
enumFromTo :: forall a. (Type a, Integral a)
           => Data a -> Data a -> Pull DIM1 (Data a)
enumFromTo 1 n
    | IntType U _ <- typeRep :: TypeRep a
    = indexed1 (i2n n) ((+1) . i2n)
enumFromTo m n = indexed1 (i2n l) ((+m) . i2n)
  where
    l = (n<m) ? 0 $ (n-m+1)

(...) :: forall a. (Type a, Integral a)
      => Data a -> Data a -> Pull DIM1 (Data a)

(...) = enumFromTo

fold1 :: (Syntax a, Pully vec) => (a -> a -> a) -> vec DIM1 a -> a
fold1 f a = fromZero $ fold f (head a) (tail a)

fromZero :: Pully vec => vec Z a -> a
fromZero vec = ixf Z
  where Pull ixf Z = toPull vec

-- Generalize to arbitrary dimensions
maximum, minimum :: (Ord a, Pully vec) => vec DIM1 (Data a) -> (Data a)
maximum = fold1 max

minimum = fold1 min

-- Generalize
or, and :: Pully vec => vec DIM1 (Data Bool) -> Data Bool
or vec = snd (whileLoop (0,false) (\(i,b) -> not b && i < l) body)
  where body (i,b)  = (i+1,ixf (Z :. i))
        Pull ixf sh = toPull vec
        (Z,l)       = uncons sh

and vec = snd (whileLoop (0,true) (\(i,b) -> b && i < l) body)
  where body (i,b)  = (i+1,ixf (Z :. i))
        Pull ixf sh = toPull vec
        (Z,l)       = uncons sh

any, all :: (Pully vec, Functor (vec DIM1)) =>
            (a -> Data Bool) -> vec DIM1 a -> Data Bool
any p = or . fmap p

all p = and . fmap p

-- Generalize
eqVector :: (Eq a, Pully vec1, Pully vec2) =>
            vec1 DIM1 (Data a) -> vec2 DIM1 (Data a) -> Data Bool
eqVector a b = length a == length b && and (zipWith (==) a b)

-- | Compute the scalar product of two pull vectors
scalarProd :: (Syntax a, Num a, Pully vec1, Pully vec2) =>
              vec1 DIM1 a -> vec2 DIM1 a -> a
scalarProd a b = fromZero $ sum (zipWith (*) a b)


-- | An overloaded function for reordering elements of a vector.
class OneDim vec where
  permute1 :: (Data Length -> Data Index -> Data Index) -> vec a -> vec a

instance OneDim (Pull DIM1) where
  permute1 perm (Pull ixf sh@(Z :. l))
    = Pull (\(Z :. i) -> ixf (Z :. perm l i)) sh

instance OneDim (Push DIM1) where
  permute1 perm (Push f sh@(Z :. l)) =
    Push (\k -> f (\(Z :. i) a -> k (Z :. (perm l i)) a)) sh

-- | OneDim the indexes of a vector
ixmap :: OneDim vec =>
         (Data Index -> Data Index) -> vec a -> vec a
ixmap perm  = permute1 (const perm)

-- | * Multidimensional push vectors
data Push sh a = Push ((Shape sh -> a -> M ()) -> M ()) (Shape sh)

instance Functor (Push sh) where
  fmap f (Push k l) = Push k' l
    where k' func   = k (\sh a -> func sh (f a))

-- | Concatenation along the the last dimension
(++) :: Push (sh :. Data Length) a
     -> Push (sh :. Data Length) a
     -> Push (sh :. Data Length) a
(Push k1 (sh1 :. l1)) ++ (Push k2 (sh2 :. l2)) = Push k (sh1 :. (l1 + l2))
  where k func = k1 func
  	       	 >>
		 k2 (\ (sh :. i) a -> func (sh :. (i + l1)) a)
-- Assumption sh1 == sh2

-- | Concatenation along the last dimension where the two vectors have the same
--   length. There is no check that the lengths are equal.
(+=+) :: Pull (sh :. Data Length) a
      -> Pull (sh :. Data Length) a
      -> Push (sh :. Data Length) a
(Pull ixf1 (sh1 :. l1)) +=+ (Pull ixf2 (sh2 :. l2))
	  = Push f (sh1 :. (l1 + l2))
  where f k = forShape (sh1 :. l1) $ \ (shi :. i) ->
                do k (shi :. i)      (ixf1 (shi :. i))
                   k (shi :. i + l1) (ixf2 (shi :. i))

-- | Flattens an array of pairs such that the elements of a pair end up next
--   to each other in the resulting array.
unpair :: Pushy arr
       => arr (sh :. Data Length) (a,a)
       -> Push (sh :. Data Length) a
unpair v = Push f' (sh :. (l * 2))
  where (Push f ex) = toPush v
        (sh,l) = case ex of sh :. l -> (sh,l)
        f' k = f (\ (sh :. i) (a,b) -> k (sh :. (2 * i)) a
                                    >> k (sh :. (2 * i + 1)) b)

reverse :: (ShapeMap vec, Vec vec input) =>
           input (sh :. Data Length) a -> vec (sh :. Data Length) a
reverse = permute (\(sh :. l) (shi :. i) -> shi :. (l - i - 1))

-- Some helper functions in Repa to help us define riffle

halve :: Pully vec => vec (sh :. Data Length) a
      -> (Pull (sh :. Data Length) a, Pull (sh :. Data Length) a)
halve vec = (Pull ixf  (sh :. (l `div` 2))
            ,Pull ixf' (sh :. ((l+1) `div` 2)))
  where ixf' (sh :. i) = ixf (sh :. (i + (l `div` 2)))
        Pull ixf ext = toPull vec
        (sh,l) = uncons ext

riffle :: Pully vec => vec (sh :. Data Length) a -> Push (sh :. Data Length) a
riffle =  unpair . uncurry zip . halve

-- | Interleaves the elements of two vectors.
interleave :: (Pully vec1, Pully vec2) =>
              vec1 (sh :. Data Length) a -> vec2 (sh :. Data Length) a ->
              Push (sh :. Data Length) a
interleave v1 v2 = unpair (zip v1 v2)


-- Pinpointing one particular dimension

data NotThis
data This

-- | In many functions it is desirable to perform an operation along one
--   particular dimension, such as concatenating two vectors along a particular
--   dimension or reversing a vector along another dimension. The `Selector`
--   typeclass enables selecting a specific dimension in a shape.
data Select ss where
  NotThis :: Select ss -> Select (ss :. NotThis)
  This    :: Select This

class Selector ss sh where
  selectDimension :: Select ss -> Shape sh -> Data Length
  adjustDimension :: Select ss -> (Data Length -> Data Length) ->
                  Shape sh -> Shape sh

instance Selector ss sh => Selector (ss :. NotThis) (sh :. Data Length) where
  selectDimension (NotThis ss) (sh :. _) = selectDimension ss sh
  adjustDimension (NotThis ss) f (sh :. l) = adjustDimension ss f sh :. l

instance Selector This (sh :. Data Length) where
  selectDimension This (_ :. l) = l
  adjustDimension This f (sh :. l) = sh :. f l

-- | Concatenating vectors along a particular dimension
conc :: Selector sel sh =>
         Select sel -> Push sh a -> Push sh a -> Push sh a
conc s (Push k1 sh1) (Push k2 sh2)
     = Push k (adjustDimension s (+ selectDimension s sh2) sh1)
  where k func = k1 func
  	       	 >>
		 k2 (\ sh a -> func (adjustDimension s (+ selectDimension s sh1) sh) a)
-- Assumption sh1 == sh2

-- | Reverse a vector along a particular dimension.
rev :: Selector sel sh =>
       Select sel -> Push sh a -> Push sh a
rev s (Push k sh) = Push k' sh
  where k' func = k (\sh a -> func (adjustDimension s (selectDimension s sh -) sh) a)

-- | Both pull vectors and push vectors can be cheaply converted to push vectors
class (Shaped vec) => Pushy vec where
  toPush :: vec sh a -> Push sh a

instance Pushy Push where
  toPush = id

instance Pushy Pull where
  toPush (Pull ixf l) = Push f l
    where f k = forShape l (\i ->
    	    	  k i (ixf i)
	        )

-- | Store a vector in memory as a flat array
fromPush :: Type a =>
	      Push sh (Data a) -> Data [a]
fromPush (Push ixf l) = runMutableArray $
	   	   	  do marr <- newArr_ (size l)
			     ixf (\ix a -> setArr marr (toIndex l ix) a)
			     return marr

freezePush :: Type a => Push sh (Data a) -> (Data [Length], Data [a])
freezePush v   = (shapeArr, fromPush v)
  where shapeArr = fromList (toList $ extent v)

thawPush :: (Type a, Shapely sh) =>
              (Data [Length], Data [a]) -> Push sh (Data a)
thawPush (l,arr) = Push f sh
  where sh = toShape 0 l
        f k = forShape sh $ \i ->
                k i (arr ! (toIndex sh i))

instance (Syntax a, Shapely sh) => Syntactic (Push sh a)
  where
    type Domain (Push sh a) = FeldDomain
    type Internal (Push sh a) = ([Length],[Internal a])
    desugar = desugar . freezePush . fmap resugar
    sugar   = fmap resugar . thawPush . sugar

-- | Flatten a pull vector of lists so that the lists become an extra dimension
flattenList :: Shapely sh => Pull sh [a] -> Push (sh :. Data Length) a
flattenList (Pull ixf sh) = Push f sz
  where f k = forShape sh $ \i ->
  	      	do let indices = fmap (\j -> i :. j) $
				 fmap value [0..l-1]
    	           zipWithM_ k indices (ixf i)
        sz  = sh :. value l
        l   = P.fromIntegral $
	      P.length (ixf fakeShape)


-- KFFs extensions

expandS :: Data Length -> Push (sh :. Data Length) a -> Push (sh :. Data Length :. Data Length) a
expandS n (Push k ext) = Push k' $ insLeft n $ insLeft p $ ext'
  where (m, ext') = peelLeft ext
        p = m `div` n
        k' wtf = k $ \ ix v -> let (i,ix') = peelLeft ix in wtf (insLeft (i `div` p) $ insLeft (i `Feldspar.mod` p) $ ix') v

contractS :: Push (sh :. Data Length :. Data Length) a -> Push (sh :. Data Length) a
contractS (Push k ext) = Push k' $ insLeft (m*n) $ ext'
  where (m, n, ext') = peelLeft2 ext
        k' wtf = k $ \ ix v -> let (i, j, ix') = peelLeft2 ix in wtf (insLeft (i*n + j) ix') v

transS :: Push (sh :. Data Length :. Data Length) a -> Push (sh :. Data Length :. Data Length) a
transS (Push k ext) = Push k' $ insLeft n $ insLeft m $ ext'
  where (m, n, ext') = peelLeft2 ext
        k' wtf = k $ \ ix v -> let (i, j, ix') = peelLeft2 ix in wtf (insLeft j $ insLeft i $ ix') v

uncurryS :: Data Length -> (Data Length -> Push sh a) -> Push (sh :. Data Length) a
uncurryS m f = Push k' (insLeft m ext)
  where Push _ ext = f (undefined :: Data Length)
        k' wtf = forM m $ \ i -> let Push k _ = f i in k (\ ix v -> wtf (insLeft i ix) v)

expandST :: Data Length -> Push (sh :. Data Length) a -> Push (sh :. Data Length :. Data Length) a
expandST n a = transS $ expandS n $ a

contractST :: Push (sh :. Data Length :. Data Length) a -> Push (sh :. Data Length) a
contractST a = contractS $ transS $ a

-- | * Manifest arrays

data Manifest sh a = Syntax a => Manifest (Data [Internal a]) (Shape sh)

class Shaped vec => Storable vec where
  store :: Syntax a => vec sh a -> Manifest sh a

instance Storable Manifest where
  store m = m

instance Storable Pull where
  store vec@(Pull ixf sh) = Manifest (fromPull (fmap F.desugar vec)) sh

instance (Syntax a, Shapely sh) => Syntactic (Manifest sh a) where 
  type Domain   (Manifest sh a) = FeldDomain
  type Internal (Manifest sh a) = ([Length],[Internal a])
  desugar = desugar . manifestToArr
  sugar   = arrToManifest . sugar

manifestToArr :: Syntax a => Manifest sh a -> (Data [Length],Data [Internal a])
manifestToArr (Manifest arr sh) = (fromList (toList sh),arr)

arrToManifest :: (Shapely sh, Syntax a) => (Data [Length], Data [Internal a]) -> Manifest sh a
arrToManifest (ls,arr) = Manifest arr (toShape 0 ls)

class (Shaped vec) => Pully vec where
  toPull :: vec sh a -> Pull sh a

instance Pully Manifest where
  toPull (Manifest arr sh) = Pull (\i -> F.sugar $ arr ! toIndex sh i) sh

instance Pully Pull where
  toPull vec = vec

instance Pushy Manifest where
  toPush m = toPush (toPull m)

class Shaped vec where
  extent :: vec sh a -> Shape sh

instance Shaped Pull where
  extent (Pull _ sh) = sh

instance Shaped Push where
  extent (Push _ sh) = sh

instance Shaped Manifest where
  extent (Manifest _ sh) = sh

-- | * Overloaded operations

class ShapeMap vec where
  type Vec vec
  permute :: Vec vec input =>
             (Shape sh -> Shape sh -> Shape sh) -> input sh a -> vec sh a
  transpose :: Vec vec input =>
               input (sh :. Data Length :. Data Length) a
            -> vec (sh :. Data Length :. Data Length) a
  expand :: Vec vec input =>
            Data Length
         -> input (sh :. Data Length) a
         -> vec (sh :. Data Length :. Data Length) a
  contract :: Vec vec input =>
              input (sh :. Data Length :. Data Length) a
           -> vec (sh :. Data Length) a

instance ShapeMap Pull where
  type Vec Pull = Pully
  permute perm vec = Pull (\i -> ixf (perm sh i)) sh
    where Pull ixf sh = toPull vec
  transpose vec = transL (toPull vec)
  expand  l vec = expandL l (toPull vec)
  contract  vec = contractL (toPull vec)

instance ShapeMap Push where
  type Vec Push = Pushy
  permute perm vec = Push (\k -> f (\i a -> k (perm sh i) a)) sh
    where (Push f sh) = toPush vec
  transpose vec = transS (toPush vec)
  expand  l vec = expandS l (toPush vec)
  contract  vec = contractS (toPush vec)
