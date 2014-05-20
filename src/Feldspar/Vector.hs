{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
module Feldspar.Vector (
  -- $intro

  module Feldspar.Vector.Shape,
  -- * Pull Vectors
  Pull,
  DPull,Pully(..),
  indexed,newExtent,traverse,reshape,unit,fromZero,
  replicate,slice,(!:),diagonal,backpermute,
  map,
  zipWith,zipWith3,zipWith4,zipWith5,
  zip,zip3,zip4,zip5,
  unzip,unzip3,unzip4,unzip5,
  fold,fold',sum,
  halve,
  expandL,expandLT,contractL,contractLT,curryL,uncurryL,dmapL,dzipWithL,
  dmapS,dzipWithS,
  -- * Shape concatenation
  ShapeConc(..),
  flatten,flattenPush,
  -- * Slices of Pull vectors
  All(..),Any(..),Slice(..),FullShape,SliceShape,sliceOfFull,fullOfSlice,
  -- * Functions on one-dimensional vectors
  Pull1,Vector,Vector1,
  value1,indexed1,(!!),
  length,take,drop,splitAt,head,last,tail,init,tails,inits,inits1,
  rotateVecL,rotateVecR,replicate1,enumFromTo,enumFrom,(...),fold1,
  maximum,minimum,or,and,any,all,eqVector,scalarProd,chunk,
  permute,ixmap,newLen1,
  -- * Functions on two-dimensional vectors
  indexed2,mmMult,
  -- * Push vectors
  Push(..),
  DPush,Pushy(..),
  empty,(++),(+=+),unpair,unpairWith,zipUnpair,riffle,interleave,flattenList,
  forwardPermute,
  expandS,expandST,contractS,contractST,uncurryS,
  -- * Manifest vectors
  Manifest,
  Storable(..),
  -- * Flattening vectors
  Flat,FlatManifest,
  storeFlat,
  -- * Overloaded functions
  Shaped(..),ShapeMap(..),
  -- * Patches
  tVec,tVec1,
  -- * Semiquestionable things
  scan,
  -- * Ugly hacks
  freezePull1,arrToManifest,arrToPull,thawPull,thawPush,
  fromList,fromPush,fromPull,freezePull,freezePush
  ) where

import qualified Prelude as P

import Language.Syntactic hiding (fold,size)
import Feldspar hiding (desugar,sugar,resugar)
import qualified Feldspar as F
import Feldspar.Vector.Shape

import Data.Tuple.Select
import Control.Monad (zipWithM_)
import Data.Proxy

-- $intro
-- The Feldspar Vector library.
--
-- The vector library is designed to give the programmer control over
-- the performance of the generated code. There are three vector types
-- in this library: 'Pull', 'Push' and 'Manifest'. Pull and Push
-- vector are never stored to memory and all operations on them are
-- subject to fusion, meaning that all intermediate vectors will be
-- eliminated. The only way to allocate something to memory is via the
-- function 'store' which generates a Manifest vector which lives in
-- memory.
--
-- The reason for having several vector types is that there is a tension
-- between providing a rich API with many different functions, and guaranteeing
-- fusion. This library has opted for guaranteeing fusion.
--
--
-- Vectors are multidimensional. They are parameterized on their shape.
-- As an example of the syntax, the shape of a two dimensional vector look like
-- this @Z :. Data Length :. Data Length@. Shapes don't have to be fully
-- determined. A shape with at least two dimensions is written
-- @sh :. Data Length :. Data Length@ where @sh@ is a type variable.

-- Slices

data All    = All
data Any sh = Any

-- | The type of slices
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

-- | Compute a slice shape from a full shape
sliceOfFull :: Slice ss -> Shape (FullShape ss) -> Shape (SliceShape ss)
sliceOfFull SZ Z = Z
sliceOfFull SAny sh = sh
sliceOfFull (fsl ::. _) (ssl :. _) = sliceOfFull fsl ssl
sliceOfFull (fsl ::: All) (ssl :. s) = sliceOfFull fsl ssl :. s

-- | Compute a full shape from a slice shape
fullOfSlice :: Slice ss -> Shape (SliceShape ss) -> Shape (FullShape ss)
fullOfSlice SZ Z = Z
fullOfSlice SAny sh = sh
fullOfSlice (fsl ::. n) ssl = fullOfSlice fsl ssl :. n
fullOfSlice (fsl ::: All) (ssl :. s) = fullOfSlice fsl ssl :. s

-- Pull Vectors

data Pull sh a = Pull (Shape sh -> a) (Shape sh)

type DPull sh a = Pull sh (Data a)

instance Functor (Pull sh)
  where
    fmap f (Pull ixf sh) = Pull (f . ixf) sh

instance (Syntax a, Shapely sh) => Syntactic (Pull sh a)
  where
    type Domain (Pull sh a) = FeldDomain
    type Internal (Pull sh a) = ([Length],[Internal a])
    desugar = desugar . freezePull . fmap resugar
    sugar   = fmap resugar . thawPull . sugar

type instance Elem (Pull sh a) = a
type instance CollIndex (Pull sh a) = Shape sh
type instance CollSize  (Pull1 a) = Data Length

instance Indexed (Pull sh a) where
    Pull ixf _ ! i = ixf i

instance Syntax a => Sized (Pull1 a)
  where
    collSize    = length
    setCollSize = newLen1

instance CollMap (Pull sh a) (Pull sh b) where
    collMap = map

-- Functions

-- | Store a vector to memory.
fromPull :: (Type a, Shapely sh) => DPull sh a -> Data [a]
fromPull vec = parallel (size ext) (\ix -> vec !: fromIndex ext ix)
  where ext = extent vec

-- | Restore a vector from memory
arrToPull :: (Type a) => Shape sh -> Data [a] -> DPull sh a
arrToPull sh arr = Pull (\ix -> arr ! toIndex sh ix) sh

-- | Store a vector and its shape to memory
freezePull :: (Type a, Shapely sh) => DPull sh a -> (Data [Length], Data [a])
freezePull v   = (shapeArr, fromPull v) -- TODO should be fromPull' to remove div and mod
  where shapeArr = fromList (toList $ extent v)

freezePull1 :: (Type a) => DPull DIM1 a -> Data [a]
freezePull1 = snd . freezePull

-- | Create an array from a Haskell list.
fromList :: Type a => [Data a] -> Data [a]
fromList ls = runMutableArray $ newListArr ls

-- | Restore a vector and its shape from memory
thawPull :: (Type a, Shapely sh) => (Data [Length], Data [a]) -> DPull sh a
thawPull (l,arr) = arrToPull (toShape 0 l) arr

-- | A shape-aware version of parallel (though this implementation is
--   sequental).
parShape :: (Type a) => Shape sh -> (Shape sh -> Data a) -> Data [a]
parShape sh ixf = runMutableArray $ do
                   arr <- newArr_ (size sh)
                   forShape sh $ \i ->
                     setArr arr (toIndex sh i) (ixf i)
                   return arr

-- | An alternative version of 'fromVector' which uses 'parShape'
fromVector' :: (Type a) => DPull sh a -> Data [a]
fromVector' (Pull ixf sh) = parShape sh ixf

-- | Change the extent of the vector to the supplied value. If the supplied
-- extent will contain more elements than the old extent, the new elements
-- will have undefined value.
newExtent :: Shape sh -> Pull sh a -> Pull sh a
newExtent sh (Pull ixf _) = Pull ixf sh

-- | Construct a pull vector from an index function and a shape.
indexed :: Shape sh -> (Shape sh -> a) -> Pull sh a
indexed l ixf = Pull ixf l

-- | Change shape and transform elements of a vector. This function is the
--   most general way of manipulating a vector.
traverse :: Pully vec sh =>
            vec sh  a -> (Shape sh -> Shape sh') ->
            ((Shape sh -> a) -> Shape sh' -> a') ->
            Pull sh' a'
traverse vec shf elemf
  = Pull (elemf ixf) (shf sh)
  where Pull ixf sh = toPull vec

-- | Duplicates part of a vector along a new dimension.
replicate :: (Pully vec (SliceShape ss), Shapely (SliceShape ss)) =>
             Slice ss -> vec (SliceShape ss) a -> Pull (FullShape ss) a
replicate sl vec
 = backpermute (fullOfSlice sl (extent vec))
               (sliceOfFull sl) (toPull vec)

-- | Extracts a slice from a vector.
slice :: (Pully vec (FullShape ss), Shapely (FullShape ss)) =>
         vec (FullShape ss) a -> Slice ss -> Pull (SliceShape ss) a
slice vec sl
 = backpermute (sliceOfFull sl (extent vec))
               (fullOfSlice sl) vec

--- | Change the length of the vector to the supplied value. If the supplied
--- length is greater than the old length, the new elements will have undefined
--- value. Useful function for patches.
newLen1 :: Syntax a => Data Length -> Pull1 a -> Pull1 a
newLen1 l vec = reshape (Z :. l) vec

-- | Change the shape of a vector. This function is potentially unsafe, the
--   new shape need to have fewer or equal number of elements compared to
--   the old shape.
reshape :: Pully vec sh' => Shape sh -> vec sh' a -> Pull sh a
reshape sh' vec
 = Pull (ixf . fromIndex sh . toIndex sh') sh'
  where Pull ixf sh = toPull vec

-- | A scalar (zero dimensional) vector
unit :: Shapely sh => a -> Pull sh a
unit a = Pull (const a) unitDim

-- | Get the one element from a zero-dimensional vector
fromZero :: Pully vec Z => vec Z a -> a
fromZero vec = ixf Z
  where Pull ixf Z = toPull vec
-- TODO: A better name.

-- | Index into a vector
(!:) :: Pully vec sh => vec sh a -> Shape sh -> a
vec !: ix = ixf ix
  where Pull ixf _ = toPull vec

-- | Extract the diagonal of a two dimensional vector
diagonal :: Pully vec DIM2 => vec DIM2 a -> Pull DIM1 a
diagonal vec = backpermute (Z :. width) (\ (_ :. x) -> Z :. x :. x) vec
  where (width : height : _) = toList (extent vec) -- brain explosion hack

-- | Change the shape of a vector.
backpermute :: Pully vec sh =>
  Shape sh' -> (Shape sh' -> Shape sh) ->
  vec sh a -> Pull sh' a
backpermute sh perm vec = traverse (toPull vec) (const sh) (. perm)

reversePull :: Pull (sh :. Data Length) a -> Pull (sh :. Data Length) a
reversePull (Pull ixf (sh :. l)) =
  Pull (\(sh' :. ix) -> ixf (sh' :. (l - ix - 1))) (sh :. l)

-- | Combines the elements of two vectors pointwise using a function.
--   The size of the resulting vector will be the intersection of the
--   two argument vectors.
zipWith :: (Pully vec1 sh, Pully vec2 sh, Shapely sh) =>
           (a -> b -> c) -> vec1 sh a -> vec2 sh b -> Pull sh c
zipWith f arr1 arr2 = Pull (\ix -> f (arr1 !: ix) (arr2 !: ix))
                           (intersectDim (extent arr1) (extent arr2))

-- | Like 'zipWith' but combines the elements of three vectors.
zipWith3 :: (Pully vec1 sh, Pully vec2 sh, Pully vec3 sh, Shapely sh) =>
           (a -> b -> c -> d) -> vec1 sh a -> vec2 sh b -> vec3 sh c -> Pull sh d
zipWith3 f arr1 arr2 arr3 = Pull (\ix -> f (arr1 !: ix) (arr2 !: ix) (arr3 !: ix))
                           (intersectDim (extent arr1)
                              (intersectDim (extent arr2) (extent arr3)))

-- | Like 'zipWith' but combines the elements of four vectors.
zipWith4 :: ( Pully vec1 sh, Pully vec2 sh, Pully vec3 sh, Pully vec4 sh
            , Shapely sh
            ) =>
           (a -> b -> c -> d -> e) -> vec1 sh a -> vec2 sh b ->
           vec3 sh c -> vec4 sh d -> Pull sh e
zipWith4 f arr1 arr2 arr3 arr4
  = Pull (\ix -> f (arr1 !: ix) (arr2 !: ix) (arr3 !: ix) (arr4 !: ix))
                           (intersectDim (intersectDim (extent arr1) (extent arr2))
                                         (intersectDim (extent arr3) (extent arr4)))

-- | Like 'zipWith' but combines the elements of five vectors.
zipWith5 :: ( Pully vec1 sh, Pully vec2 sh, Pully vec3 sh, Pully vec4 sh
            , Pully vec5 sh, Shapely sh
            ) =>
           (a -> b -> c -> d -> e -> f) -> vec1 sh a -> vec2 sh b ->
           vec3 sh c -> vec4 sh d -> vec5 sh e -> Pull sh f
zipWith5 f arr1 arr2 arr3 arr4 arr5
  = Pull (\ix -> f (arr1 !: ix) (arr2 !: ix) (arr3 !: ix) (arr4 !: ix) (arr5 !: ix))
            (intersectDim (extent arr1)
                          (intersectDim (intersectDim (extent arr2) (extent arr3))
                                        (intersectDim (extent arr4) (extent arr5))))

-- | Combines the elements of two vectors. The size of the resulting vector
--   will be the intersection of the two argument vectors.
zip :: (Pully vec1 sh, Pully vec2 sh, Shapely sh) =>
       vec1 sh a -> vec2 sh b -> Pull sh (a,b)
zip = zipWith (\a b -> (a,b))

-- | Like 'zip' but combining three vectors.
zip3 :: (Pully vec1 sh, Pully vec2 sh, Pully vec3 sh) =>
        vec1 sh a -> vec2 sh b -> vec3 sh c ->
        Pull sh (a,b,c)
zip3 vec1 vec2 vec3
  = Pull (\i -> (ixf1 i, ixf2 i, ixf3 i))
         (intersectDim (intersectDim sh1 sh2) sh3)
  where Pull ixf1 sh1 = toPull vec1
        Pull ixf2 sh2 = toPull vec2
        Pull ixf3 sh3 = toPull vec3

-- | Zipping four vectors
zip4 :: (Pully vec1 sh, Pully vec2 sh, Pully vec3 sh, Pully vec4 sh) =>
        vec1 sh a -> vec2 sh b -> vec3 sh c -> vec4 sh d ->
        Pull sh (a,b,c,d)
zip4 vec1 vec2 vec3 vec4
  = Pull (\i -> (ixf1 i, ixf2 i, ixf3 i, ixf4 i)) (intersectDim (intersectDim sh1 sh2) (intersectDim sh3 sh4))
  where Pull ixf1 sh1 = toPull vec1
        Pull ixf2 sh2 = toPull vec2
        Pull ixf3 sh3 = toPull vec3
        Pull ixf4 sh4 = toPull vec4

-- | Zipping five vectors
zip5 :: (Pully vec1 sh, Pully vec2 sh, Pully vec3 sh, Pully vec4 sh, Pully vec5 sh) =>
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

-- | Unzipping two vectors
unzip :: Functor vec => vec (a,b) -> (vec a, vec b)
unzip v = (fmap sel1 v, fmap sel2 v)

-- | Unzipping three vectors
unzip3 :: Functor vec => vec (a,b,c) -> (vec a, vec b, vec c)
unzip3 v = (fmap sel1 v, fmap sel2 v, fmap sel3 v)

-- | Unzipping four vectors
unzip4 :: Functor vec => vec (a,b,c,d) -> (vec a, vec b, vec c, vec d)
unzip4 v = (fmap sel1 v, fmap sel2 v, fmap sel3 v, fmap sel4 v)

-- | Unzipping five vectors
unzip5 :: Functor vec => vec (a,b,c,d,e) -> (vec a, vec b, vec c, vec d, vec e)
unzip5 v = (fmap sel1 v, fmap sel2 v, fmap sel3 v, fmap sel4 v, fmap sel5 v)

-- | Reduce a vector along its outermost dimension
fold :: (Syntax b, Pully vec (sh :. Data Length), Shapely sh) =>
        (b -> a -> b)
     -> b
     -> vec (sh :. Data Length) a
     -> Pull sh b
fold f x vec = Pull ixf sh
    where (sh, n) = uncons (extent vec) -- brain explosion hack
          ixf i = forLoop n x (\ix s -> f s (vec !: (i :. ix)))

-- Here's another version of fold which has a little bit more freedom
-- when it comes to choosing the initial element when folding

-- | A generalization of 'fold' which allows for different initial
--   values when starting to fold.
fold' :: (Syntax a, Pully vec1 sh, Pully vec2 (sh :. Data Length), Shapely sh)
      => (a -> a -> a)
      -> vec1 sh a
      -> vec2 (sh :. Data Length) a
      -> Pull sh a
fold' f x vec = Pull ixf sh
    where (sh, n) = uncons (extent vec) -- brain explosion hack
          ixf i = forLoop n (x!:i) (\ix s -> f s (vec !: (i :. ix)))

-- | Summing a vector along its outermost dimension
sum :: (Syntax a, Num a, Pully vec (sh :. Data Length), Shapely sh) =>
       vec (sh :. Data Length) a -> Pull sh a
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

-- | Flatten nested pull vectors.
flatten :: forall a sh1 sh2.
           (Shapely (ShapeConcT sh1 sh2), ShapeConc sh1 sh2) =>
           Pull sh1 (Pull sh2 a) ->
           Pull (ShapeConcT sh1 sh2) a
flatten (Pull ixf1 sh1) = Pull ixf sh
  where ixf i = let (i1,i2) = splitIndex i sh1
  	       	    (Pull ixf2 _) = ixf1 i1
  	       	in ixf2 i2
        sh = let (i1,_ :: Shape sh2) = splitIndex fakeShape sh1
                 (Pull _ sh2) = ixf1 i1
             in shapeConc sh1 sh2

-- | Flatten a pull vector of push vectors.
flattenPush :: forall a sh1 sh2.
               (Shapely (ShapeConcT sh1 sh2), ShapeConc sh1 sh2) =>
               Pull sh1 (Push sh2 a) ->
               Push (ShapeConcT sh1 sh2) a
flattenPush (Pull ixf sh1) = Push f sh
  where f k = forShape sh1 $ \i ->
                let Push g sh' = ixf i
                in  g (\j a -> k (shapeConc i j) a)
        sh = let (i1,_ :: Shape sh2) = splitIndex fakeShape sh1
                 (Push _ sh2) = ixf i1
             in shapeConc sh1 sh2

-- | Create a two-dimensional Pull vector
indexed2 :: Data Length -> Data Length -> (Data Index -> Data Index -> a) -> Pull DIM2 a
indexed2 l1 l2 ixf = Pull (\(Z :. i1 :. i2) -> ixf i1 i2) (Z :. l1 :. l2)

-- Matrix Multiplication

-- | Transpose the two innermmost dimensions of a vector
transposeL :: forall sh e vec.
              (Pully vec (sh :. Data Length :. Data Length), Shapely sh) =>
              vec  (sh :. Data Length :. Data Length) e ->
              Pull (sh :. Data Length :. Data Length) e
transposeL vec
  = backpermute new_extent swap vec
  where swap ((tail :: Shape sh) :. i :. j) = tail :. j :. i
        new_extent         = swap (extent vec)

-- | Transpose a two-dimensional vector
transpose2D :: Pully vec DIM2 => vec DIM2 e -> Pull DIM2 e
transpose2D = transposeL

-- | Matrix multiplication
mmMult :: (Syntax e, Num e, Pully vec1 DIM2, Pully vec2 DIM2) =>
          Bool -> vec1 DIM2 e -> vec2 DIM2 e -> Pull DIM2 e
mmMult doForce vA vB
  = sum (zipWith (*) vaRepl vbRepl)
  where
    tmp = if doForce
           then force $ transpose (toPull vB)
           else transpose (toPull vB)
    vaRepl = replicate (SZ ::: All   ::. colsB ::: All) vA
    vbRepl = replicate (SZ ::. rowsA ::: All   ::: All) tmp
    [rowsA, colsA] = toList (extent vA) -- brain explosion hack
    [rowsB, colsB] = toList (extent vB)


-- KFFs combinators

-- | Split the innermost dimension of a pull vector in two. If the innermost
-- dimension of 'vec' has length 'i' then the innermost dimension of
-- @expandL n vec@ has length 'n' and the second innermost dimension will have
-- length @i / p@.
expandL :: Pully vec (sh :. Data Length) =>
           Data Length -> vec (sh :. Data Length) a ->
           Pull (sh :. Data Length :. Data Length) a
expandL n v = Pull ixf' (insLeft n $ insLeft p $ ext')
  where (Pull ixf ext) = toPull v
        (m, ext') = peelLeft ext
        p = m `div` n
        ixf' ix = let (i,ix') = peelLeft ix; (j,ix'') = peelLeft ix' in ixf $ insLeft (i*p + j) ix''

-- | Flatten the two innermost dimensions into a single dimension
contractL :: Pully vec (sh :. Data Length :. Data Length) =>
             vec (sh :. Data Length :. Data Length) a ->
             Pull (sh :. Data Length) a
contractL v = Pull ixf' (insLeft (m*n) ext')
  where (Pull ixf ext) = toPull v
        (m, n, ext') = peelLeft2 ext
        ixf' ix = let (i,ix') = peelLeft ix in ixf $ insLeft (i `div` n) $ insLeft (i `mod` n) $ ix'

-- | Swap the two innermmost dimensions
transL :: Pully vec (sh :. Data Length :. Data Length) =>
          vec (sh :. Data Length :. Data Length) a ->
          Pull (sh :. Data Length :. Data Length) a
transL v = Pull ixf' (insLeft n $ insLeft m $ ext')
  where (Pull ixf ext) = toPull v
        (m, n, ext') = peelLeft2 ext
        ixf' ix = let (i, j, ix') = peelLeft2 ix in ixf $ insLeft j $ insLeft i $ ix'

-- | Abstract over the innermost dimension in a vector
curryL :: Pull (sh :. Data Length) a -> (Data Length, Data Length -> Pull sh a)
curryL (Pull ixf sh) = (n, \ i -> Pull (\ ix -> ixf $ insLeft i ix) sh')
  where (n, sh') = peelLeft sh
-- Note: curry is unsafe in that it produces an index function that does not check that its leftmost argument is in range

-- | Instantiate the innermost dimension from an abstracted vector
uncurryL :: Data Length -> (Data Length -> Pull sh a) -> Pull (sh :. Data Length) a
uncurryL m f = Pull ixf (insLeft m ext)
  where Pull _ ext = f (undefined :: Data Length)
        ixf ix = let (i, ix') = peelLeft ix; Pull ixf' _ = f i in ixf' ix'

-- | Transform a pull vector, except for the innermost dimension
dmapL :: (Pull sh1 a1 -> Pull sh2 a2) -> Pull (sh1 :. Data Length) a1 -> Pull (sh2 :. Data Length) a2
dmapL f a = uncurryL n $ f . g
  where (n,g) = curryL a

-- | Zip together two pull vectors, but preserve the innermost dimension
dzipWithL :: (Pull sh1 a1 -> Pull sh2 a2 -> Pull sh3 a3) ->
             Pull (sh1 :. Data Length) a1 -> Pull (sh2 :. Data Length) a2 ->
             Pull (sh3 :. Data Length) a3
dzipWithL f a1 a2 = uncurryL (min m n) $ \ i -> f (g i) (h i)
  where (m,g) = curryL a1
        (n,h) = curryL a2

-- Convenience functions that maybe should not be in the lib

expandLT :: Pully vec (sh :. Data Length) =>
            Data Length -> vec (sh :. Data Length) a ->
            Pull (sh :. Data Length :. Data Length) a
expandLT n a = transL $ expandL n $ a

contractLT :: Pully vec (sh :. Data Length :. Data Length) =>
              vec (sh :. Data Length :. Data Length) a ->
              Pull (sh :. Data Length) a
contractLT a = contractL $ transL $ a

-- | Transform a pull vector to a push vector, but preserve the innermost
--   dimension
dmapS :: (Pull sh1 a1 -> Push sh2 a2) -> Pull (sh1 :. Data Length) a1 -> Push (sh2 :. Data Length) a2
dmapS f a = uncurryS n $ f . g
  where (n,g) = curryL a

-- | Zip together two pull vectors into a push vector, but preserve the
--   innermost dimension
dzipWithS :: (Pull sh1 a1 -> Pull sh2 a2 -> Push sh3 a3) -> Pull (sh1 :. Data Length) a1 -> Pull (sh2 :. Data Length) a2
          -> Push (sh3 :. Data Length) a3
dzipWithS f a1 a2 = uncurryS (min m n) $ \ i -> f (g i) (h i)
  where (m,g) = curryL a1
        (n,h) = curryL a2


-- Functions on one dimensional Pull vectors

type Pull1 a = Pull DIM1 (Data a)

type Vector a = Pull DIM1 a
{-# DEPRECATED Vector "Use Pull instead" #-}
type Vector1 a = Pull1 a
{-# DEPRECATED Vector1 "Use Pull1 instead" #-}

value1 :: Syntax a => [Internal a] -> Manifest DIM1 a
value1 ls = value ([P.fromIntegral (P.length ls)],ls)

-- | Create a one-dimensional Pull vector
indexed1 :: Data Length -> (Data Index -> a) -> Pull DIM1 a
indexed1 l ixf = Pull (\(Z :. i) -> ixf i) (Z :. l)

(!!) :: Pully vec DIM1 => vec DIM1 a -> Data Index -> a
vec !! i = vec !: (Z :. i)

-- | Take the length of a one-dimensional vector
length :: Shaped vec => vec DIM1 a -> Data Length
length vec = l
  where (Z,l) = uncons (extent vec)

-- | The call @take n vec@ returns a Pull vector containing the first @n@
--   elements of @vec@.
take :: Pully vec DIM1 => Data Length -> vec DIM1 a -> Pull DIM1 a
take n vec = Pull ixf (Z :. (min n l))
  where Pull ixf sh = toPull vec
        (Z,l)       = uncons sh

-- | The call @drop n vec@ removes the first @n@ elements of @vec@
drop :: Pully vec DIM1 => Data Length -> vec DIM1 a -> Pull DIM1 a
drop n vec = Pull (\(Z :. i) -> ixf (Z :. i+n)) (Z :. (l-n))
  where Pull ixf sh = toPull vec
        (Z,l)       = uncons sh

-- | Splits a pull vector in two at a particular index
splitAt :: Pully vec DIM1 =>
           Data Index -> vec DIM1 a -> (Pull DIM1 a, Pull DIM1 a)
splitAt n vec = (take n vec, drop n vec)

-- | Take the first element of a pull vector
head :: Pully vec DIM1 => vec DIM1 a -> a
head vec = ixf (Z :. 0)
  where Pull ixf _ = toPull vec

-- | Take the last element of a pull vector
last :: Pully vec DIM1 => vec DIM1 a -> a
last vec = ixf (Z :. (l-1))
  where Pull ixf sh = toPull vec
        (Z,l)       = uncons sh

-- | Remove the first element of a pull vector
tail :: Pully vec DIM1 => vec DIM1 a -> Pull DIM1 a
tail = drop 1

-- | Remove the last element of a pull vector
init :: Pully vec DIM1 => vec DIM1 a -> Pull DIM1 a
init vec = take (length vec - 1) vec

-- | Create a vector containing all the suffixes of the input vector
tails :: Pully vec DIM1 => vec DIM1 a -> Pull DIM1 (Pull DIM1 a)
tails vec = indexed1 (length vec + 1) (`drop` vec)

-- | Create a vector containing all the prefixes of the input vector
inits :: Pully vec DIM1 => vec DIM1 a -> Pull DIM1 (Pull DIM1 a)
inits vec = indexed1 (length vec + 1) (`take` vec)

-- | Similar to 'inits' but without the empty vector
inits1 :: Pully vec DIM1 => vec DIM1 a -> Pull DIM1 (Pull DIM1 a)
inits1 = tail . inits

-- | The call @rotateVecL n vec@ rotates the elements of @vec@ @n@ steps
--   to the left
rotateVecL :: Data Index -> Pull DIM1 a -> Pull DIM1 a
rotateVecL ix = permute (\l i -> (i + ix) `rem` l)

-- | The call @rotateVecR n vec@ rotates the elements of @vec@ @n@ steps
--   to the right
rotateVecR :: Data Index -> Pull DIM1 a -> Pull DIM1 a
rotateVecR ix = reverse . rotateVecL ix . reverse

-- | @replicate1 n a@ creates a one dimensional pull vector
--    containing @n@ copies of @a@
replicate1 :: Data Length -> a -> Pull DIM1 a
replicate1 n a = Pull (const a) (Z :. n)

-- | A vector which enumerates numbers consecutively
enumFromTo :: forall a. (Type a, Integral a)
           => Data a -> Data a -> Pull DIM1 (Data a)
enumFromTo 0 n
    | IntType U _ <- typeRep :: TypeRep a
    = indexed1 (i2n n + 1) i2n
enumFromTo 1 n
    | IntType U _ <- typeRep :: TypeRep a
    = indexed1 (i2n n) ((+1) . i2n)
enumFromTo m n = indexed1 (i2n l) ((+m) . i2n)
  where
    l = (n<m) ? 0 $ (n-m+1)

-- | @enumFrom m@: Enumerate the indexes from @m@ to 'maxBound'
enumFrom m = enumFromTo m (value maxBound)

-- | An infix version of 'enumFromTo'.
(...) :: forall a. (Type a, Integral a)
      => Data a -> Data a -> Pull DIM1 (Data a)
(...) = enumFromTo

scan :: (Syntax a, Syntax b) => (a -> b -> a) -> a -> Pull DIM1 b -> Pull DIM1 a
scan f init bs = toPull $ arrToManifest (fromList [length bs],
  F.sugar $ sequential (length bs) (F.desugar init) $ \i s ->
    let s' = F.desugar $ f (F.sugar s) (bs!!i)
    in  (s',s'))

-- | Transform all the elements of a vector
map :: Functor vec => (a -> b) -> vec a -> vec b
map = fmap

-- | Folding a one-dimensional vector
fold1 :: (Syntax a, Pully vec DIM1) => (a -> a -> a) -> vec DIM1 a -> a
fold1 f a = fromZero $ fold f (head a) (tail a)

-- TODO: Generalize minimum and maximum to arbitrary dimensions and Pushy vecs


-- | Get the maximum element from a vector
maximum :: (Ord a, Pully vec DIM1) => vec DIM1 (Data a) -> (Data a)
maximum = fold1 max

-- | Get the minimum element from a vector
minimum :: (Ord a, Pully vec DIM1) => vec DIM1 (Data a) -> (Data a)
minimum = fold1 min

-- TODO: Generalize or and and to arbitrary dimensions and Pushy vectors

-- | Compute logical or over a vector
or :: Pully vec DIM1 => vec DIM1 (Data Bool) -> Data Bool
or vec = snd (whileLoop (0,false) (\(i,b) -> not b && i < l) body)
  where body (i,b)  = (i+1,ixf (Z :. i))
        Pull ixf sh = toPull vec
        (Z,l)       = uncons sh

-- | Compute logical and over a vector
and :: Pully vec DIM1 => vec DIM1 (Data Bool) -> Data Bool
and vec = snd (whileLoop (0,true) (\(i,b) -> b && i < l) body)
  where body (i,b)  = (i+1,ixf (Z :. i))
        Pull ixf sh = toPull vec
        (Z,l)       = uncons sh

-- | Determine whether any of the elements of a vector satisfies a predicate
any :: (Pully vec DIM1, Functor (vec DIM1)) =>
       (a -> Data Bool) -> vec DIM1 a -> Data Bool
any p = or . fmap p

-- | Determine whether all elements of a vector satisfies a predicate
all :: (Pully vec DIM1, Functor (vec DIM1)) =>
       (a -> Data Bool) -> vec DIM1 a -> Data Bool
all p = and . fmap p

-- TODO: Generalize eqVectors, once 'and' is generalized

-- | Testing equality between two one-dimensional vectors
eqVector :: (Eq a, Pully vec1 DIM1, Pully vec2 DIM1) =>
            vec1 DIM1 (Data a) -> vec2 DIM1 (Data a) -> Data Bool
eqVector a b = length a == length b && and (zipWith (==) a b)

-- | Compute the scalar product of two pull vectors
scalarProd :: (Syntax a, Num a, Pully vec1 DIM1, Pully vec2 DIM1) =>
              vec1 DIM1 a -> vec2 DIM1 a -> a
scalarProd a b = fromZero $ sum (zipWith (*) a b)

-- | This function can distribute vector computations on chunks of a large
--   pull vector. A call @chunk l f g v@ will split the vector 'v' into chunks
--   of size 'l' and apply 'f' to these chunks. In case the length of 'v' is
--   not a multiple of 'l' then the rest of 'v' will be processed by 'g'.
chunk :: (Pully vec DIM1, Pushy vec1 DIM1, Pushy vec2 DIM1, Syntax b)
      => Data Length            -- ^ Size of the chunks
      -> (Pull DIM1 a -> vec1 DIM1 b) -- ^ Applied to every chunk
      -> (Pull DIM1 a -> vec2 DIM1 b) -- ^ Applied to the rest of the vector
      -> vec DIM1 a
      -> Push DIM1 b
chunk c f g vec = Push loop (Z :. (noc * c))
             ++ toPush (g (drop (noc * c) v))
  where l = length v
        noc = l `div` c
        loop func = forM noc $ \i ->
                      do let (Push k _) = toPush $ f (take c (drop (c*i) v))
                         k (\(Z :. j) a -> func (Z :. (c*i + j)) a)
        v = toPull vec

-- | Permutes the elements of a one-dimensional vector according to the
--   supplied function. It is important that the first argument is bijection
--   otherwise the result is undefined.
permute :: (Data Length -> Data Index -> Data Index) ->
           Pull DIM1 a -> Pull DIM1 a
permute perm (Pull ixf sh@(Z :. l))
  = Pull (\(Z :. i) -> ixf (Z :. perm l i)) sh

-- | Like 'permute' but without the length-parameter to the permutation function
ixmap :: (Data Index -> Data Index) -> Pull DIM1 a -> Pull DIM1 a
ixmap perm  = permute (const perm)

-- Multidimensional push vectors
data Push sh a = Push ((Shape sh -> a -> M ()) -> M ()) (Shape sh)

type DPush sh a = Push sh (Data a)

instance Functor (Push sh) where
  fmap f (Push k l) = Push k' l
    where k' func   = k (\sh a -> func sh (f a))

-- | The empty push vector.
empty :: Shapely sh => Push sh a
empty = Push (const (return ())) zeroDim

-- | Concatenation along the the outmost dimension
(++) :: (Pushy vec1 (sh :. Data Length), Pushy vec2 (sh :. Data Length)) =>
        vec1 (sh :. Data Length) a
     -> vec2 (sh :. Data Length) a
     -> Push (sh :. Data Length) a
vec1 ++ vec2 = Push k (sh1 :. (l1 + l2))
  where Push k1 ext1 = toPush vec1
        Push k2 ext2 = toPush vec2
        (sh1,l1) = uncons ext1
        (sh2,l2) = uncons ext2
        k func = k1 func
                 >>
                 k2 (\ (sh :. i) a -> func (sh :. (i + l1)) a)
-- Assumption sh1 == sh2

-- | Concatenation along the outermost dimension where the two vectors have
--   the same length. There is no check that the lengths are equal.
(+=+) :: (Pully vec1 (sh :. Data Length), Pully vec2 (sh :. Data Length)) =>
         vec1 (sh :. Data Length) a
      -> vec2 (sh :. Data Length) a
      -> Push (sh :. Data Length) a
vec1 +=+ vec2 = Push f (sh1 :. (l1 + l2))
  where Pull ixf1 ext1 = toPull vec1
        Pull ixf2 ext2 = toPull vec2
        (sh1,l1) = uncons ext1
        (sh2,l2) = uncons ext2
        f k = forShape (sh1 :. l1) $ \ (shi :. i) ->
                do k (shi :. i)      (ixf1 (shi :. i))
                   k (shi :. i + l1) (ixf2 (shi :. i))

-- | Flattens a vector of pairs such that the elements of a pair end up next
--   to each other in the resulting vector.
unpair :: Pushy vec (sh :. Data Length)
       => vec (sh :. Data Length) (a,a)
       -> Push (sh :. Data Length) a
unpair v = Push f' (sh :. (l * 2))
  where (Push f ex) = toPush v
        (sh,l) = uncons ex
        f' k = f (\ (sh :. i) (a,b) -> k (sh :. (2 * i)) a
                                    >> k (sh :. (2 * i + 1)) b)

-- | Similar to 'unpair' but allows control over where the elements end up
--   in the result vector.
--
-- @
--   unpair = unpairWith (\(sh :. i) -> (sh :. 2*i)) (\(sh :. i) -> (sh :. 2*i+1))
-- @
unpairWith :: Pushy vec (sh :. Data Length)
           => (Shape (sh :. Data Length) -> Shape (sh :. Data Length))
           -> (Shape (sh :. Data Length) -> Shape (sh :. Data Length))
           -> vec (sh :. Data Length) (a,a)
           -> Push (sh :. Data Length) a
unpairWith ix1 ix2 vec = Push f' (sh :. (l*2))
  where (Push f ex) = toPush vec
        (sh,l) = uncons ex
        f' k = f (\ix (a,b) -> k (ix1 ix) a >> k (ix2 ix) b)

-- | Interleaves the elements of two vectors.
zipUnpair :: (Pully vec1 (sh :. Data Length),
              Pully vec2 (sh :. Data Length),
              Shapely sh) =>
             vec1 (sh :. Data Length) a -> vec2 (sh :. Data Length) a ->
             Push (sh :. Data Length) a
zipUnpair vec1 vec2 = unpair (zip vec1 vec2)

-- Some helper functions in Repa to help us define riffle

-- | Split a vector in half along the outermost dimension. If there is an odd
--   number of elements in that dimension, the second vector of the two result
--   vectors will be one longer than the first.
halve :: Pully vec (sh :. Data Length) => vec (sh :. Data Length) a
      -> (Pull (sh :. Data Length) a, Pull (sh :. Data Length) a)
halve vec = (Pull ixf  (sh :. (l `div` 2))
            ,Pull ixf' (sh :. ((l+1) `div` 2)))
  where ixf' (sh :. i) = ixf (sh :. (i + (l `div` 2)))
        Pull ixf ext = toPull vec
        (sh,l) = uncons ext

-- | Permute the elemements of a vector such that the first half is interleaved
--   with the second half. Useful for constructing butterfly networks.
--
-- @
-- riffle (enumFromTo 1 10) == [1,6,2,7,3,8,4,9,5,10]
-- @
riffle :: (Pully vec (sh :. Data Length), Shapely sh) =>
          vec (sh :. Data Length) a -> Push (sh :. Data Length) a
riffle =  unpair . uncurry zip . halve

-- | Interleaves the elements of two vectors.
interleave :: (Pully vec1 (sh :. Data Length)
              ,Pully vec2 (sh :. Data Length)
              ,Shapely sh) =>
              vec1 (sh :. Data Length) a -> vec2 (sh :. Data Length) a ->
              Push (sh :. Data Length) a
interleave v1 v2 = unpair (zip v1 v2)

-- | Forward permute a push vector.
forwardPermute :: Pushy vec1 sh =>
                  (Shape sh -> Shape sh -> Shape sh) ->
                  vec1 sh a ->  Push sh a
forwardPermute p vec = Push g sh
  where Push f sh = toPush vec
        g k = f (\ix a -> k (p sh ix) a)

reversePush :: Push (sh :. Data Length) a -> Push (sh :. Data Length) a
reversePush (Push f (sh :. l)) =
  Push (\k -> f (\(sh' :. ix) a -> k (sh' :. (l - ix - 1)) a)) (sh :. l)

-- Pinpointing one particular dimension

data NotThis
data This

-- | In many functions it is desirable to perform an operation along one
--   particular dimension, such as concatenating two vectors along a particular
--   dimension or reversing a vector along another dimension. The 'Selector'
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

-- | This class captures all types of vectors which can turned into a 'Push'
--   vector cheaply
class (Shaped vec) => Pushy vec sh where
  toPush :: vec sh a -> Push sh a

instance Pushy Push sh where
  toPush = id

instance Pushy Pull sh where
  toPush (Pull ixf l) = Push f l
    where f k = forShape l $ \i ->
    	    	  k i (ixf i)

-- | Store a vector in memory as a flat array
fromPush :: Type a
         => Push sh (Data a) -> Data [a]
fromPush (Push ixf l) = runMutableArray $
                          do marr <- newArr_ (size l)
                             ixf (\ix a -> setArr marr (toIndex l ix) a)
                             return marr

freezePush :: (Type a, Shapely sh) =>
              Push sh (Data a) -> (Data [Length], Data [a])
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

-- | Split the innermost dimension of a push vector in two
expandS :: Pushy vec (sh :. Data Length) =>
           Data Length -> vec (sh :. Data Length) a ->
           Push (sh :. Data Length :. Data Length) a
expandS n v = Push g $ insLeft n $ insLeft p $ ext'
  where (Push f ext) = toPush v
        (m, ext') = peelLeft ext
        p = m `div` n
        g k = f $ \ ix v -> let (i,ix') = peelLeft ix in k (insLeft (i `div` p) $ insLeft (i `Feldspar.mod` p) $ ix') v

contractS :: Pushy vec (sh :. Data Length :. Data Length) =>
             vec (sh :. Data Length :. Data Length) a ->
             Push (sh :. Data Length) a
contractS v = Push g $ insLeft (m*n) $ ext'
  where (Push f ext) = toPush v
        (m, n, ext') = peelLeft2 ext
        g k = f $ \ ix v -> let (i, j, ix') = peelLeft2 ix in k (insLeft (i*n + j) ix') v

transS :: Pushy vec (sh :. Data Length :. Data Length) =>
          vec (sh :. Data Length :. Data Length) a ->
          Push (sh :. Data Length :. Data Length) a
transS v = Push g $ insLeft n $ insLeft m $ ext'
  where (Push f ext) = toPush v
        (m, n, ext') = peelLeft2 ext
        g k = f $ \ ix v -> let (i, j, ix') = peelLeft2 ix in k (insLeft j $ insLeft i $ ix') v

-- | Instantiate the innermost dimension from an abstracted push vector
uncurryS :: Data Length -> (Data Length -> Push sh a) -> Push (sh :. Data Length) a
uncurryS m f = Push g (insLeft m ext)
  where Push _ ext = f (undefined :: Data Length)
        g k = forM m $ \ i -> let Push h _ = f i in h (\ ix v -> k (insLeft i ix) v)

expandST :: Pushy vec (sh :. Data Length) =>
            Data Length -> vec (sh :. Data Length) a ->
            Push (sh :. Data Length :. Data Length) a
expandST n a = transS $ expandS n $ a

contractST :: Pushy vec (sh :. Data Length :. Data Length) =>
              vec (sh :. Data Length :. Data Length) a ->
              Push (sh :. Data Length) a
contractST a = contractS $ transS $ a

-- | Manifest vectors live in memory. Pull- and Push vectors can be allocated
--   as Manifest using the 'store' function.
data Manifest sh a = Syntax a => Manifest (Data [Internal a]) (Data [Length])

-- | A class for memory allocation. All vectors are instances of this class.
class Shaped vec => Storable vec where
  -- | Allocates a vector to memory.
  store :: (Syntax a, Shapely sh) => vec sh a -> Manifest sh a

instance Storable Manifest where
  store m = m

instance Storable Pull where
  store vec@(Pull ixf sh) = Manifest (fromPull (fmap F.desugar vec)) (fromList (toList sh))

instance Storable Push where
  store vec@(Push f sh) = Manifest (fromPush (fmap F.desugar vec)) (fromList (toList sh))

instance (Syntax a, Shapely sh) => Syntactic (Manifest sh a) where
  type Domain   (Manifest sh a) = FeldDomain
  type Internal (Manifest sh a) = ([Length],[Internal a])
  desugar = desugar . manifestToArr
  sugar   = arrToManifest . sugar

manifestToArr :: Syntax a => Manifest sh a -> (Data [Length],Data [Internal a])
manifestToArr (Manifest arr sh) = (sh,arr)

arrToManifest :: Syntax a => (Data [Length], Data [Internal a]) -> Manifest sh a
arrToManifest (ls,arr) = Manifest arr ls

-- | A typeclass for types of array elements which can be flattened. An example
--   is an array of pairs, which can be flattened into a pair of arrays.
class Flat sh a where
  type FlatManifest sh a
  type Arr a
  allocArray  :: Proxy a -> Proxy sh -> Data Length -> M (Arr a)
  writeArray  :: Proxy sh -> Arr a -> ((Data Index -> a -> M ()) -> M ()) -> M ()
  freezeArr   :: Proxy a -> Shape sh -> Arr a -> M (FlatManifest sh a)

instance Type a => Flat sh (Data a) where
  type FlatManifest sh (Data a) = Manifest sh (Data a)
  type Arr (Data a) = Data (MArr a)
  allocArray _ _ = newArr_
  writeArray _ marr f = f (\i a -> setArr marr i a)
  freezeArr _ sh arr = fmap (\a -> Manifest a (fromList (toList sh))) $
                       freezeArray arr

instance (Flat sh a, Flat sh b) => Flat sh (a,b) where
  type FlatManifest sh (a,b) = (FlatManifest sh a, FlatManifest sh b)
  type Arr (a,b) = (Arr a, Arr b)
  allocArray (_ :: Proxy (a,b)) (_ :: Proxy sh) l = do
    a1 <- allocArray (Proxy :: Proxy a) (Proxy :: Proxy sh) l
    a2 <- allocArray (Proxy :: Proxy b) (Proxy :: Proxy sh) l
    return (a1,a2)
  writeArray (_ :: Proxy sh) (arr1,arr2) f =
    f (\i (a,b) -> writeArray (Proxy :: Proxy sh) arr1 (\k -> k i a) >>
                   writeArray (Proxy :: Proxy sh) arr2 (\k -> k i b)
      )
  freezeArr (_ :: Proxy (a,b)) sh (arr1,arr2) = do
    a1 <- freezeArr (Proxy :: Proxy a) sh arr1
    a2 <- freezeArr (Proxy :: Proxy b) sh arr2
    return (a1,a2)

instance (Flat sh a, Flat sh b, Flat sh c) => Flat sh (a,b,c) where
  type FlatManifest sh (a,b,c) = (FlatManifest sh a, FlatManifest sh b,FlatManifest sh c)
  type Arr (a,b,c) = (Arr a, Arr b, Arr c)
  allocArray (p :: Proxy (a,b,c)) (_ :: Proxy sh) l = do
    a1 <- allocArray (Proxy :: Proxy a) (Proxy :: Proxy sh) l
    a2 <- allocArray (Proxy :: Proxy b) (Proxy :: Proxy sh) l
    a3 <- allocArray (Proxy :: Proxy c) (Proxy :: Proxy sh) l
    return (a1,a2,a3)
  writeArray (_ :: Proxy sh) (arr1,arr2,arr3) f =
    f (\i (a,b,c) ->
        writeArray (Proxy :: Proxy sh) arr1 (\k -> k i a) >>
        writeArray (Proxy :: Proxy sh) arr2 (\k -> k i b) >>
        writeArray (Proxy :: Proxy sh) arr3 (\k -> k i c)
      )
  freezeArr (p :: Proxy (a,b,c)) sh (arr1,arr2,arr3) = do
    a1 <- freezeArr (Proxy :: Proxy a) sh arr1
    a2 <- freezeArr (Proxy :: Proxy b) sh arr2
    a3 <- freezeArr (Proxy :: Proxy c) sh arr3
    return (a1,a2,a3)

-- | Stores a vector into one or several flattened manifest vectors. The
--   elements of the vector have to be instances of the 'Flat' type class.
--   For example, a vector of type @Pull sh (Data Int,Data Float)@ will be
--   stored as @(Manifest sh (Data Int), Manifest sh (Data Float))@.
storeFlat :: forall a vec sh. (Flat sh a, Pushy vec sh, Syntax (FlatManifest sh a)) =>
             vec sh a -> FlatManifest sh a
storeFlat vec = runMutable $ do
                  arr <- allocArray (Proxy :: Proxy a) (Proxy :: Proxy sh) (size l)
                  writeArray (Proxy :: Proxy sh) arr (\k -> f (\sh a -> k (toIndex l sh) a))
                  freezeArr (Proxy :: Proxy a) l arr
  where (Push f l) = toPush vec

-- | This class captures all vectors which can be turned into a 'Pull' vector
--   without allocating memory.
class (Shaped vec) => Pully vec sh where
  toPull :: vec sh a -> Pull sh a

instance Shapely sh => Pully Manifest sh where
  toPull (Manifest arr shA) = Pull (\i -> F.sugar $ arr ! toIndex sh i) sh
    where sh = toShape 0 shA

instance Pully Pull sh where
  toPull vec = vec

instance Shapely sh => Pushy Manifest sh where
  toPush m = toPush (toPull m)

-- | A single method class for getting the shape of a vector
class Shaped vec where
  extent :: Shapely sh => vec sh a -> Shape sh

instance Shaped Pull where
  extent (Pull _ sh) = sh

instance Shaped Push where
  extent (Push _ sh) = sh

instance Shaped Manifest where
  extent (Manifest _ sh) = toShape 0 sh

-- Overloaded operations

-- | A class with various functions for manipulating the shape of a vector
class ShapeMap vec where
  -- | Reverse a vector along its outermost dimension
  reverse :: vec (sh :. Data Length) a -> vec (sh :. Data Length) a
  -- | Transpose a vector
  transpose :: vec (sh :. Data Length :. Data Length) a
            -> vec (sh :. Data Length :. Data Length) a
  -- | Add an extra dimension to a vector by splitting the innermost
  --   dimension in two, using the first argument.
  expand :: Data Length
         -> vec (sh :. Data Length) a
         -> vec (sh :. Data Length :. Data Length) a
  -- | Merge the two innermost dimensions
  contract :: vec (sh :. Data Length :. Data Length) a
           -> vec (sh :. Data Length) a

instance ShapeMap Pull where
  reverse   vec = reversePull vec
  transpose vec = transL vec
  expand  l vec = expandL l vec
  contract  vec = contractL vec

instance ShapeMap Push where
  reverse   vec = reversePush vec
  transpose vec = transS vec
  expand  l vec = expandS l vec
  contract  vec = contractS vec

-------------------------------------------------------------------------------
 --- Misc.
-------------------------------------------------------------------------------

tVec :: Patch a a -> Patch (Pull sh a) (Pull sh a)
tVec _ = id

tVec1 :: Patch a a -> Patch (Pull1 a) (Pull1 a)
tVec1 _ = id

-- tVec2 :: Patch a a -> Patch (Pull (Vector (Data a))) (Vector (Vector (Data a)))
-- tVec2 _ = id
