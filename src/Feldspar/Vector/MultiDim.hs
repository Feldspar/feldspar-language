{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
module Feldspar.Vector.MultiDim where

import qualified Prelude as P

import Language.Syntactic hiding (fold,size)
import Feldspar hiding (desugar,sugar,resugar)
import qualified Feldspar as F
import Feldspar.Vector.Shape

import Data.Tuple.Select
import Data.Tuple.Curry

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

-- | * Vectors

data Vector sh a = Vector (Shape sh) (Shape sh -> a)

type DVector sh a = Vector sh (Data a)


instance (Syntax a, Shapely sh) => Syntactic (Vector sh a) where
    type Domain   (Vector sh a) = FeldDomain
    type Internal (Vector sh a) = ([Length],[Internal a])
    desugar = desugar . freezeVector . map resugar
    sugar   = map resugar . thawVector . sugar

-- instance (Syntax a, Shapely sh) => Syntax (Vector sh a)

instance Functor (Vector sh)
  where
    fmap = map

-- | * Functions

-- | Store a vector in an array.
fromVector :: (Type a) => DVector sh a -> Data [a]
fromVector vec = parallel (size ext) (\ix -> vec !: fromIndex ext ix)
  where ext = extent vec


-- | Restore a vector from an array
toVector :: (Type a) => Shape sh -> Data [a] -> DVector sh a
toVector sh arr = Vector sh (\ix -> arr ! toIndex sh ix)

freezeVector :: (Type a) => DVector sh a -> (Data [Length], Data [a])
freezeVector v   = (shapeArr, fromVector v) -- TODO should be fromVector' to remove div and mod
  where shapeArr = fromList (toList $ extent v)

fromList :: Type a => [Data a] -> Data [a]
fromList ls = loop 1 (parallel (value len) (const (P.head ls)))
  where loop i arr
            | i P.< len = loop (i+1) (setIx arr (value i) (ls P.!! (P.fromIntegral i)))
            | otherwise = arr
        len  = P.fromIntegral $ P.length ls

thawVector :: (Type a, Shapely sh) => (Data [Length], Data [a]) -> DVector sh a
thawVector (l,arr) = toVector (toShape 0 l) arr

-- | Store a vector in memory. Use this function instead of 'force' if
--   possible as it is both much more safe and faster.
memorize :: (Type a) => DVector sh a -> DVector sh a
memorize vec = toVector (extent vec) (fromVector vec)

-- | A shape-aware version of parallel (though this implementation is
--   sequental).
parShape :: (Type a) => Shape sh -> (Shape sh -> Data a) -> Data [a]
parShape sh ixf = runMutableArray $ do
                   arr <- newArr_ (size sh)
                   forShape sh $ \i -> do
                     setArr arr (toIndex sh i) (ixf i)
                   return arr

-- | An alternative version of `fromVector` which uses `parShape`
fromVector' :: (Type a) => DVector sh a -> Data [a]
fromVector' (Vector sh ixf) = parShape sh ixf

-- | The shape and size of the vector
extent :: Vector sh a -> Shape sh
extent (Vector sh _) = sh

-- | Change the extent of the vector to the supplied value. If the supplied
-- extent will contain more elements than the old extent, the new elements 
-- will have undefined value.
newExtent :: Shape sh -> Vector sh a -> Vector sh a
newExtent sh (Vector _ ixf) = Vector sh ixf

indexed :: (Shape sh -> a) -> Shape sh -> Vector sh a
indexed ixf l = Vector l ixf

-- | Change shape and transform elements of a vector. This function is the
--   most general way of manipulating a vector.
traverse :: Vector sh  a -> (Shape sh -> Shape sh') ->
            ((Shape sh -> a) -> Shape sh' -> a') ->
            Vector sh' a'
traverse (Vector sh ixf) shf elemf
  = Vector (shf sh) (elemf ixf)

-- | Duplicates part of a vector along a new dimension.
replicate :: Slice ss -> Vector (SliceShape ss) a -> Vector (FullShape ss) a
replicate sl vec
 = backpermute (fullOfSlice sl (extent vec))
               (sliceOfFull sl) vec

-- | Extracts a slice from a vector.
slice :: Vector (FullShape ss) a -> Slice ss -> Vector (SliceShape ss) a
slice vec sl
 = backpermute (sliceOfFull sl (extent vec))
               (fullOfSlice sl) vec

-- | Change the shape of a vector. This function is potentially unsafe, the
--   new shape need to have fewer or equal number of elements compared to
--   the old shape.
reshape :: Shape sh -> Vector sh' a -> Vector sh a
reshape sh' (Vector sh ixf)
 = Vector sh' (ixf . fromIndex sh . toIndex sh')

-- | A scalar (zero dimensional) vector
unit :: a -> Vector Z a
unit a = Vector Z (const a)

-- | Index into a vector
(!:) :: Vector sh a -> Shape sh -> a
(Vector _ ixf) !: ix = ixf ix

-- | Extract the diagonal of a two dimensional vector
diagonal :: Vector DIM2 a -> Vector DIM1 a
diagonal vec = backpermute (Z :. width) (\ (_ :. x) -> Z :. x :. x) vec
  where (width : height : _) = toList (extent vec) -- brain explosion hack

-- | Change the shape of a vector.
backpermute :: Shape sh' -> (Shape sh' -> Shape sh) ->
               Vector sh a -> Vector sh' a
backpermute sh perm vec = traverse vec (const sh) (. perm)

permute :: (Shape sh -> Shape sh -> Shape sh) ->
           Vector sh a -> Vector sh a
permute perm (Vector sh ixf) = Vector sh (ixf . (perm sh))

-- | Map a function on all the elements of a vector
map :: (a -> b) -> Vector sh a -> Vector sh b
map f (Vector sh ixf) = Vector sh (f . ixf)

-- | Combines the elements of two vectors. The size of the resulting vector
--   will be the intersection of the two argument vectors.
zip :: Vector sh a -> Vector sh b -> Vector sh (a,b)
zip = zipWith (\a b -> (a,b))

-- | Combines the elements of two vectors pointwise using a function.
--   The size of the resulting vector will be the intersection of the
--   two argument vectors.
zipWith :: (a -> b -> c) -> Vector sh a -> Vector sh b -> Vector sh c
zipWith f arr1 arr2 = Vector (intersectDim (extent arr1) (extent arr2))
                      (\ix -> f (arr1 !: ix) (arr2 !: ix))

zip3 :: Vector sh a -> Vector sh b -> Vector sh c ->
        Vector sh (a,b,c)
zip3 (Vector l1 ixf1) (Vector l2 ixf2) (Vector l3 ixf3)
  = Vector (intersectDim (intersectDim l1 l2) l3) (\i -> (ixf1 i, ixf2 i, ixf3 i))

-- | Zipping four pull vectors
zip4 (Vector l1 ixf1) (Vector l2 ixf2) (Vector l3 ixf3) (Vector l4 ixf4)
  = Vector (intersectDim (intersectDim l1 l2) (intersectDim l3 l4)) (\i -> (ixf1 i, ixf2 i, ixf3 i, ixf4 i))

zip5 (Vector l1 ixf1) (Vector l2 ixf2) (Vector l3 ixf3) (Vector l4 ixf4) (Vector l5 ixf5)
  = Vector  (intersectDim (intersectDim (intersectDim l1 l2) (intersectDim l3 l4)) l5) (\i -> (ixf1 i, ixf2 i, ixf3 i, ixf4 i, ixf5 i))

-- | Unzipping two pull vectors
unzip :: Vector sh (a,b) -> (Vector sh a, Vector sh b)
unzip v = (map sel1 v, map sel2 v)

-- | Unzipping three pull vectors
unzip3 :: Vector sh (a,b,c) -> (Vector sh a, Vector sh b, Vector sh c)
unzip3 v = (map sel1 v, map sel2 v, map sel3 v)

-- | Unzipping four pull vectors
unzip4 :: Vector sh (a,b,c,d) -> (Vector sh a, Vector sh b, Vector sh c, Vector sh d)
unzip4 v = (map sel1 v, map sel2 v, map sel3 v, map sel4 v)

-- | Unzipping five pull vectors
unzip5 :: Vector sh (a,b,c,d,e) -> (Vector sh a, Vector sh b, Vector sh c, Vector sh d, Vector sh e)
unzip5 v = (map sel1 v, map sel2 v, map sel3 v, map sel4 v, map sel5 v)

-- | Reduce a vector along its last dimension

fold :: (Syntax a) =>
        (a -> a -> a)
     -> a
     -> Vector (sh :. Data Length) a
     -> Vector sh a
fold f x vec = Vector sh ixf
    where (sh, n) = uncons (extent vec) -- brain explosion hack
          ixf i = forLoop n x (\ix s -> f s (vec !: (i :. ix)))

-- Here's another version of fold which has a little bit more freedom
-- when it comes to choosing the initial element when folding

-- | A generalization of 'fold' which allows for different initial
--   values when starting to fold.
fold' :: (Syntax a)
      => (a -> a -> a)
      -> Vector sh a
      -> Vector (sh :. Data Length) a
      -> Vector sh a
fold' f x vec = Vector sh ixf
    where (sh, n) = uncons (extent vec) -- brain explosion hack
          ixf i = forLoop n (x!:i) (\ix s -> f s (vec !: (i :. ix)))

-- | Summing a vector along its last dimension
sum :: (Syntax a, Num a) => Vector (sh :. Data Length) a -> Vector sh a
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
          ShapeConc sh1 sh2 => Vector sh1 (Vector sh2 a)
       -> Vector (ShapeConcT sh1 sh2) a
flatten (Vector sh1 ixf1) = Vector sh ixf
  where ixf i = let (i1,i2) = splitIndex i sh1
  	       	    (Vector _ ixf2) = ixf1 i1
  	       	in ixf2 i2
        sh = let (i1,_ :: Shape sh2) = splitIndex fakeShape sh1
	         (Vector sh2 _) = ixf1 i1
	     in shapeConc sh1 sh2

-- Laplace

stencil :: DVector DIM2 Float -> DVector DIM2 Float
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

laplace :: Data Length -> DVector DIM2 Float -> DVector DIM2 Float
laplace steps vec = toVector (extent vec) $
                    forLoop steps (fromVector vec) (\ix ->
                       fromVector . stencil . toVector (extent vec)
                    )


-- Matrix Multiplication

transpose :: forall sh e. Vector (sh :. Data Length :. Data Length) e 
                       -> Vector (sh :. Data Length :. Data Length) e
transpose vec
  = backpermute new_extent swap vec
  where swap ((tail :: Shape sh) :. i :. j) = tail :. j :. i
        new_extent         = swap (extent vec)

transpose2D :: Vector DIM2 e -> Vector DIM2 e
transpose2D = transpose

-- | Matrix multiplication
mmMult :: (Syntax e, Num e) =>
          Vector DIM2 e -> Vector DIM2 e -> Vector DIM2 e
mmMult vA vB
  = sum (zipWith (*) vaRepl vbRepl)
  where
    tmp = transpose2D vB
    vaRepl = replicate (SZ ::: All   ::. colsB ::: All) vA
    vbRepl = replicate (SZ ::. rowsA ::: All   ::: All)  vB
    [rowsA, colsA] = toList (extent vA) -- brain explosion hack
    [rowsB, colsB] = toList (extent vB)


-- KFFs combinators

expandL :: Data Length -> Vector (sh :. Data Length) a -> Vector (sh :. Data Length :. Data Length) a
expandL n (Vector ext ixf) = Vector (insLeft n $ insLeft p $ ext') ixf'
  where (m, ext') = peelLeft ext
        p = m `div` n
        ixf' ix = let (i,ix') = peelLeft ix; (j,ix'') = peelLeft ix' in ixf $ insLeft (i*p + j) ix''

contractL :: Vector (sh :. Data Length :. Data Length) a -> Vector (sh :. Data Length) a
contractL (Vector ext ixf) = Vector (insLeft (m*n) ext') ixf'
  where (m, n, ext') = peelLeft2 ext
        ixf' ix = let (i,ix') = peelLeft ix in ixf $ insLeft (i `div` n) $ insLeft (i `mod` n) $ ix'

transL :: Vector (sh :. Data Length :. Data Length) a -> Vector (sh :. Data Length :. Data Length) a
transL (Vector ext ixf) = Vector (insLeft n $ insLeft m $ ext') ixf'
  where (m, n, ext') = peelLeft2 ext
        ixf' ix = let (i, j, ix') = peelLeft2 ix in ixf $ insLeft j $ insLeft i $ ix'


-- Note: curry is unsafe in that it produces an index function that does not check that its leftmost argument is in range
curryL :: Vector (sh :. Data Length) a -> (Data Length, Data Length -> Vector sh a)
curryL (Vector sh ixf) = (n, \ i -> Vector sh' (\ ix -> ixf $ insLeft i ix))
  where (n, sh') = peelLeft sh

uncurryL :: Data Length -> (Data Length -> Vector sh a) -> Vector (sh :. Data Length) a
uncurryL m f = Vector (insLeft m ext) ixf
  where Vector ext _ = f (undefined :: Data Length)
        ixf ix = let (i, ix') = peelLeft ix; Vector _ ixf' = f i in ixf' ix'

dmapL :: (Vector sh1 a1 -> Vector sh2 a2) -> Vector (sh1 :. Data Length) a1 -> Vector (sh2 :. Data Length) a2
dmapL f a = uncurryL n $ f . g
  where (n,g) = curryL a

dzipWithL :: (Vector sh1 a1 -> Vector sh2 a2 -> Vector sh3 a3) -> Vector (sh1 :. Data Length) a1 -> Vector (sh2 :. Data Length) a2
          -> Vector (sh3 :. Data Length) a3
dzipWithL f a1 a2 = uncurryL (min m n) $ \ i -> f (g i) (h i)
  where (m,g) = curryL a1
        (n,h) = curryL a2

-- Convenience functions that maybe should not be in the lib

expandLT :: Data Length -> Vector (sh :. Data Length) a -> Vector (sh :. Data Length :. Data Length) a
expandLT n a = transL $ expandL n $ a

contractLT :: Vector (sh :. Data Length :. Data Length) a -> Vector (sh :. Data Length) a
contractLT a = contractL $ transL $ a



{-

Here is some functions that use both pull and push vectors (pull in, push out). Hence they do not build in the current module structure,
so I include them in the form of comments. Should be uncommentable in a joint module, modulo renaming ;-)

Only difference to the pure pull variants is the use of uncurryS instead of uncurryL.

By the way: S.Vector is push vector and L.Vector is pull vector.

dmapS :: (L.Vector sh1 a1 -> S.Vector sh2 a2) -> L.Vector (sh1 :. Data Length) a1 -> S.Vector (sh2 :. Data Length) a2
dmapS f a = uncurryS n $ f . g
  where (n,g) = curryL a

dzipWithS :: (L.Vector sh1 a1 -> L.Vector sh2 a2 -> S.Vector sh3 a3) -> L.Vector (sh1 :. Data Length) a1 -> L.Vector (sh2 :. Data Length) a2
          -> S.Vector (sh3 :. Data Length) a3
dzipWithS f a1 a2 = uncurryS (min m n) $ \ i -> f (g i) (h i)
  where (m,g) = curryL a1
        (n,h) = curryL a2

-}


-- | * Functions on one dimensional Pull vectors

indexed1 :: Data Length -> (Data Index -> a) -> Vector DIM1 a
indexed1 l ixf = Vector (Z :. l) (\(Z :. i) -> ixf i)

length :: Vector DIM1 a -> Data Length
length (Vector (Z :. l) _) = l

take :: Data Length -> Vector DIM1 a -> Vector DIM1 a
take n (Vector (Z :. l) ixf) = Vector (Z :. (min n l)) ixf

-- | `drop n vec` removes the first `n` elements of `vec`
drop :: Data Length -> Vector DIM1 a -> Vector DIM1 a
drop n (Vector (Z :. l) ixf) = Vector (Z :. (l-n)) (\(Z :. i) -> ixf (Z :. i+n))

-- | Splits a pull vector in two at a particular index
splitAt :: Data Index -> Vector DIM1 a -> (Vector DIM1 a, Vector DIM1 a)
splitAt n vec = (take n vec, drop n vec)

-- | Take the first element of a pull vector
head :: Vector DIM1 a -> a
head (Vector _ ixf) = ixf (Z :. 0)

-- | Take the last element of a pull vector
last :: Vector DIM1 a -> a
last (Vector (Z :. l) ixf) = ixf (Z :. (l-1))

-- | Remove the first element of a pull vector
tail :: Vector DIM1 a -> Vector DIM1 a
tail = drop 1

-- | Remove the last element of a pull vector
init :: Vector DIM1 a -> Vector DIM1 a
init vec = take (length vec - 1) vec

-- | Create a vector containing all the suffixes of the input vector
tails :: Vector DIM1 a -> Vector DIM1 (Vector DIM1 a)
tails vec = indexed1 (length vec + 1) (`drop` vec)

-- | Create a vector containing all the prefixes of the input vector
inits :: Vector DIM1 a -> Vector DIM1 (Vector DIM1 a)
inits vec = indexed1 (length vec + 1) (`take` vec)

-- | Similar to `inits` but without the empty vector
inits1 :: Vector DIM1 a -> Vector DIM1 (Vector DIM1 a)
inits1 = tail . inits

-- | `roteateVecL n vec` rotates the elements of `vec` `n` steps to the left 
rotateVecL :: Data Index -> Vector DIM1 a -> Vector DIM1 a
rotateVecL ix = permute1 (\l i -> (i + ix) `rem` l)

-- | `roteateVecR n vec` rotates the elements of `vec` `n` steps to the right 
rotateVecR :: Data Index -> Vector DIM1 a -> Vector DIM1 a
rotateVecR ix = reverse . rotateVecL ix . reverse

-- | `replicate n a` creates a pull array containing `n` copies of `a`
replicate1 :: Data Length -> a -> Vector DIM1 a
replicate1 n a = Vector (Z :. n) (const a)

-- | A vector which enumerates numbers consecutively
enumFromTo :: forall a. (Type a, Integral a)
           => Data a -> Data a -> Vector DIM1 (Data a)
enumFromTo 1 n
    | IntType U _ <- typeRep :: TypeRep a
    = indexed1 (i2n n) ((+1) . i2n)
enumFromTo m n = indexed1 (i2n l) ((+m) . i2n)
  where
    l = (n<m) ? 0 $ (n-m+1)

(...) :: forall a. (Type a, Integral a)
      => Data a -> Data a -> Vector DIM1 (Data a)

(...) = enumFromTo

-- | An overloaded function for reordering elements of a vector.
class Permute vec where
  permute1 :: (Data Length -> Data Index -> Data Index) -> vec a -> vec a

instance Permute (Vector DIM1) where
  permute1 perm (Vector sh@(Z :. l) ixf)
    = Vector sh (\(Z :. i) -> ixf (Z :. perm l i))
{-
instance Permute Push where
  permute perm (Push f l) = Push (\k -> f (\i a -> k (perm l i) a)) l
-}
-- | Permute the indexes of a vector
ixmap :: Permute vec =>
         (Data Index -> Data Index) -> vec a -> vec a
ixmap perm  = permute1 (const perm)

-- | Reverse a vector.
reverse :: Vector DIM1 a -> Vector DIM1 a
reverse arr = ixmap (\ix -> length arr - ix - 1) arr

-- | * Manifest arrays

data Manifest sh a = Syntax a => Manifest (Data [Internal a]) (Shape sh)

class Storable vec where
  store :: Syntax a => vec sh a -> Manifest sh a

instance Storable Manifest where
  store m = m

instance Storable Vector where
  store vec@(Vector sh ixf) = Manifest (fromVector' (fmap F.desugar vec)) sh

class Pully vec where
  toPull :: vec sh a -> Vector sh a

instance Pully Manifest where
  toPull (Manifest arr sh) = Vector sh (\i -> F.sugar $ arr ! toIndex sh i)

instance Pully Vector where
  toPull vec = vec
