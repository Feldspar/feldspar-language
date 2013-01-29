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
import Feldspar.Vector.Shape

import QuickAnnotate

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

-- | * Fuctions

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
sum :: (Type a, Numeric a) =>
       DVector (sh :. Data Length) a -> DVector sh a
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
mmMult :: (Type e, Numeric e)
       => DVector DIM2 e -> DVector DIM2 e -> DVector DIM2 e
mmMult vA vB
  = sum (zipWith (*) vaRepl vbRepl)
  where
    tmp = transpose2D vB
    vaRepl = replicate (SZ ::: All   ::. colsB ::: All) vA
    vbRepl = replicate (SZ ::. rowsA ::: All   ::: All)  vB
    [rowsA, colsA] = toList (extent vA) -- brain explosion hack
    [rowsB, colsB] = toList (extent vB)
