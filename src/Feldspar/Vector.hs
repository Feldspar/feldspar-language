{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}

module Feldspar.Vector (
  -- $intro

  module Feldspar.Vector.Shape,
  VecShape,
  -- * Pull Vectors
  Pull(..),
  DPull,Pully(..),
  indexed,newExtent,traverse,reshape,singleton,fromZero,
  replicate,slice,(!:),diagonal,backpermute,
  map,
  zipWith,zipWith3,zipWith4,zipWith5,
  zip,zip3,zip4,zip5,
  unzip,unzip3,unzip4,unzip5,
  fold,fold',sum,
  zeros,ones,constant,
  halve, outerProd, interior, takeSh, dropSh,
  expandL,expandLT,contractL,contractLT,curryL,uncurryL,dmapL,dzipWithL,
  dmapS,dzipWithS,
  curry',uncurry',dmap',uncurryS',dmapS',
  -- * Shape concatenation
  ShapeConc(..),
  flatten,flattenPush,
  AppendShape,appendShape,splitShape,
  leftFlatten,leftFlatten',rightFlatten,rightFlatten',
  leftNest,leftNest',leftMap,
  rightNest,rightNest',
  -- * Slices of Pull vectors
  All(..),Any(..),Slice(..),FullShape,SliceShape,sliceOfFull,fullOfSlice,
  -- * Functions on one-dimensional vectors
  Pull1,Vector,Vector1,
  value1,indexed1,(!!),
  length,take,drop,splitAt,head,last,tail,init,tails,inits,inits1,
  rotateVecL,rotateVecR,replicate1,enumFromTo,enumFrom,(...),fold1,
  maximum,minimum,or,and,any,all,eqVector,scalarProd,chunk,
  permute,ixmap,dup,newLen1,
  find,
  -- * Functions on two-dimensional vectors
  indexed2,mmMult,eye,eyePush,eye2,eye2Push,Matrixy(..),above,beside,
  -- * Push vectors
  Push(..),
  DPush,Pushy(..),
  PushK,
  empty,(++),(+=+),unpair,unpairWith,riffle,interleave,flattenList,
  forwardPermute,
  expandS,expandST,contractS,contractST,uncurryS,
  -- * Manifest vectors
  Manifest(..),
  Storable(..),
  -- * Flattening vectors
  Flat,FlatManifest,
  storeFlat,
  -- * Overloaded functions
  Shaped(..),ShapeMap(..),
  -- * Patches
  tVec,tVec1,tPull,tPull1,
  -- * Vector concatenation
  Select(..), conc,
  -- * Semiquestionable things
  scan,
  -- * Ugly hacks
  freezePull1,arrToManifest,arrToPull,thawPull,thawPull1,thawPush,
  fromList,fromPush,fromPull,freezePull,freezePush,freezePush1
  ) where

import qualified Prelude as P

import Feldspar.Core.Reify (Syntactic(..))
import Feldspar hiding (desugar, sugar)
import qualified Feldspar as F
import Feldspar.Core.Language
import Feldspar.Core.Tuple
import Feldspar.Vector.Shape

import Control.Monad (zipWithM_)
import Data.Proxy
import Data.Hash (Hashable)
import Data.List (genericLength)

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


-- NOTES on overloading
--
-- Since this library uses several types of arrays, there are many operations
-- which are overloaded so that they can work with multiple, or all, types
-- of arrays. However, too much overloading can easily lead to ambiguities
-- in the user code, so that the type checker cannot figure what type to
-- use and the programmer has to insert type annotations. Annoying, and
-- something we want to avoid.
--
-- This library uses the following form of overloading: arguments are typically
-- overloaded, with one of the classes Pully or Pushy. Return types are either
-- always a concrete type, i.e. Pull, Push or Manifest, or polymorphic but the
-- same type as one of the input arguments. Return types are never fully
-- overloaded.
-- This style of overloading enables a lot of reuse while still enables the
-- type checker to figure out all the types in most cases.

-- Slices

data All    = All
data Any sh = Any

-- | The type of slices
data Slice ss where
  SZ    :: Slice Z
  (::.) :: Slice sl -> Data Length -> Slice (sl :. Data Length)
  (:::) :: Slice sl -> All -> Slice (sl :. All)
  SAny  :: Slice (Any sl)


type family FullShape ss where
  FullShape Z                   = Z
  FullShape (Any sh)            = sh
  FullShape (sl :. Data Length) = FullShape sl :. Data Length
  FullShape (sl :. All)         = FullShape sl :. Data Length

type family SliceShape ss where
  SliceShape Z                   = Z
  SliceShape (Any sh)            = sh
  SliceShape (sl :. Data Length) = SliceShape sl
  SliceShape (sl :. All)         = SliceShape sl :. Data Length

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

type family InternalShape sh a where
    InternalShape Z         a = Internal a
    InternalShape (Z  :. l) a = [Internal a]
    InternalShape sh a = Tuple '[[Internal a], Internal (Tuple (ShapeTupT sh))]

class ShapeTup a where
  type ShapeTupT a :: [*]
  toTup :: Shape a -> Tuple (ShapeTupT a)
  fromTup :: Tuple (ShapeTupT a) -> Shape a

instance ShapeTup Z where
  type ShapeTupT Z = '[]
  toTup _ = TNil
  fromTup _ = Z

instance ShapeTup sh => ShapeTup (sh :. Data Length) where
  type ShapeTupT (sh :. Data Length) = Data Length ': ShapeTupT sh
  toTup (sh :. n) = n :* toTup sh
  fromTup (n :* t) = fromTup t :. n

instance (Syntax a, Shapely sh, ShapeTup sh, SyntacticTup (ShapeTupT sh))
      => Syntactic (Pull sh a)
  where
    type Internal (Pull sh a) = InternalShape sh a

    desugar v@(Pull _ sh) = case sh of
        Z           -> desugar $ fromZero v
        Z :. _      -> desugar $ freezePull1 $ fmap resugar v
        _ :. _ :. _ -> desugar $ freezePull $ fmap resugar v

    sugar v = case fakeShape :: Shape sh of
        Z           -> singleton $ sugar v
        Z :. _      -> fmap resugar $ thawPull1 $ sugar v
        _ :. _ :. _ -> fmap resugar $ thawPull $ sugar v

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
fromPull :: Type a => DPull sh a -> Data [a]
fromPull (Pull ixf sh) = parShape sh ixf

-- | Restore a vector from memory
arrToPull :: (Type a) => Shape sh -> Data [a] -> DPull sh a
arrToPull sh arr = Pull (\ix -> arr ! toIndex sh ix) sh

-- | Store a vector and its shape to memory
freezePull :: (Type a, Shapely sh, ShapeTup sh, SyntacticTup (ShapeTupT sh))
           => DPull sh a -> Tuple '[Data [a], Tuple (ShapeTupT sh)]
freezePull v = twotup (fromPull v) (toTup $ extent v)

freezePull1 :: (Type a) => DPull DIM1 a -> Data [a]
freezePull1 = fromPull

-- | Create an array from a Haskell list.
fromList :: Type a => [Data a] -> Data [a]
fromList ls = materialize (value $ genericLength ls)
                        $ P.foldr (\ (ix,v) e -> write (value ix) v `par` e) skip
                        $ P.zip [0..] ls

-- | Restore a vector and its shape from memory
thawPull :: (Type a, Shapely sh, ShapeTup sh)
         => Tuple '[Data [a], Tuple (ShapeTupT sh)] -> DPull sh a
thawPull tarr = arrToPull (fromTup $ nsnd tarr) (nfst tarr)

-- | Restore a vector and its shape from memory
thawPull1 :: Type a => Data [a] -> DPull DIM1 a
thawPull1 arr = arrToPull (Z :. getLength arr) arr

-- | A shape-aware version of parallel.
parShape :: Type a => Shape sh -> (Shape sh -> Data a) -> Data [a]
parShape sh ixf = materialize (size sh) $ toLoops (\ ix -> write (toIndex sh ix) (ixf ix)) sh
  where toLoops :: Type a => (Shape sh -> Data (Elements a)) -> Shape sh -> Data (Elements a)
        toLoops f Z = f Z
        toLoops f (bnds :. n) = toLoops (\ ix -> parFor n $ \ i -> f (ix :. i)) bnds

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
traverse :: Pully vec =>
            vec  a -> (Shape (VecShape vec) -> Shape sh') ->
            ((Shape (VecShape vec) -> a) -> Shape sh' -> a') ->
            Pull sh' a'
traverse vec shf elemf
  = Pull (elemf ixf) (shf sh)
  where Pull ixf sh = toPull vec

-- | Duplicates part of a vector along a new dimension.
replicate :: (Pully vec, VecShape vec ~ SliceShape ss, Shapely (SliceShape ss)) =>
             Slice ss -> vec a -> Pull (FullShape ss) a
replicate sl vec
 = backpermute (fullOfSlice sl (extent vec))
               (sliceOfFull sl) (toPull vec)

-- | Extracts a slice from a vector.
slice :: (Pully vec, VecShape vec ~ FullShape ss, Shapely (FullShape ss)) =>
         vec a -> Slice ss -> Pull (SliceShape ss) a
slice vec sl
 = backpermute (sliceOfFull sl (extent vec))
               (fullOfSlice sl) vec

--- | Change the length of the vector to the supplied value. If the supplied
--- length is greater than the old length, the new elements will have undefined
--- value. Useful function for patches.
newLen1 :: Data Length -> Pull DIM1 a -> Pull DIM1 a
newLen1 l = reshape (Z :. l)

-- | Change the shape of a vector. This function is potentially unsafe, the
--   new shape need to have fewer or equal number of elements compared to
--   the old shape.
reshape :: Pully vec => Shape sh -> vec a -> Pull sh a
reshape sh' vec
 = Pull (ixf . fromIndex sh . toIndex sh') sh'
  where Pull ixf sh = toPull vec

-- | A scalar (zero dimensional) vector
singleton :: Shapely sh => a -> Pull sh a
singleton a = Pull (const a) unitDim

-- | Get the one element from a zero-dimensional vector
fromZero :: (Pully vec, VecShape vec ~ Z) => vec a -> a
fromZero vec = ixf Z
  where Pull ixf Z = toPull vec
-- TODO: A better name.

-- | Index into a vector
(!:) :: Pully vec => vec a -> Shape (VecShape vec) -> a
vec !: ix = ixf ix
  where Pull ixf _ = toPull vec

-- | Extract the diagonal of a two dimensional vector
diagonal :: (Pully vec, VecShape vec ~ DIM2) => vec a -> Pull DIM1 a
diagonal vec = backpermute (Z :. width) (\ (_ :. x) -> Z :. x :. x) vec
  where (width : height : _) = toList (extent vec) -- brain explosion hack

-- | Change the shape of a vector.
backpermute :: Pully vec =>
  Shape sh' -> (Shape sh' -> Shape (VecShape vec)) ->
  vec a -> Pull sh' a
backpermute sh perm vec = traverse (toPull vec) (const sh) (. perm)

reversePull :: Pull (sh :. Data Length) a -> Pull (sh :. Data Length) a
reversePull (Pull ixf (sh :. l)) =
  Pull (\(sh' :. ix) -> ixf (sh' :. (l - ix - 1))) (sh :. l)

-- | Combines the elements of two vectors pointwise using a function.
--   The size of the resulting vector will be the intersection of the
--   two argument vectors.
zipWith :: (Pully vec1, VecShape vec1 ~ sh, Pully vec2, VecShape vec2 ~ sh, Shapely sh) =>
           (a -> b -> c) -> vec1 a -> vec2 b -> Pull sh c
zipWith f arr1 arr2 = Pull (\ix -> f (arr1 !: ix) (arr2 !: ix))
                           (intersectDim (extent arr1) (extent arr2))

-- | Like 'zipWith' but combines the elements of three vectors.
zipWith3 :: (Pully vec1, VecShape vec1 ~ sh, Pully vec2, VecShape vec2 ~ sh, Pully vec3, VecShape vec3 ~ sh, Shapely sh) =>
           (a -> b -> c -> d) -> vec1 a -> vec2 b -> vec3 c -> Pull sh d
zipWith3 f arr1 arr2 arr3 = Pull (\ix -> f (arr1 !: ix) (arr2 !: ix) (arr3 !: ix))
                           (intersectDim (extent arr1)
                              (intersectDim (extent arr2) (extent arr3)))

-- | Like 'zipWith' but combines the elements of four vectors.
zipWith4 :: ( Pully vec1, VecShape vec1 ~ sh, Pully vec2, VecShape vec2 ~ sh, Pully vec3, VecShape vec3 ~ sh, Pully vec4, VecShape vec4 ~ sh
            , Shapely sh
            ) =>
           (a -> b -> c -> d -> e) -> vec1 a -> vec2 b ->
           vec3 c -> vec4 d -> Pull sh e
zipWith4 f arr1 arr2 arr3 arr4
  = Pull (\ix -> f (arr1 !: ix) (arr2 !: ix) (arr3 !: ix) (arr4 !: ix))
                           (intersectDim (intersectDim (extent arr1) (extent arr2))
                                         (intersectDim (extent arr3) (extent arr4)))

-- | Like 'zipWith' but combines the elements of five vectors.
zipWith5 :: ( Pully vec1, VecShape vec1 ~ sh, Pully vec2, VecShape vec2 ~ sh, Pully vec3, VecShape vec3 ~ sh, Pully vec4, VecShape vec4 ~ sh
            , Pully vec5, VecShape vec5 ~ sh, Shapely sh
            ) =>
           (a -> b -> c -> d -> e -> f) -> vec1 a -> vec2 b ->
           vec3 c -> vec4 d -> vec5 e -> Pull sh f
zipWith5 f arr1 arr2 arr3 arr4 arr5
  = Pull (\ix -> f (arr1 !: ix) (arr2 !: ix) (arr3 !: ix) (arr4 !: ix) (arr5 !: ix))
            (intersectDim (extent arr1)
                          (intersectDim (intersectDim (extent arr2) (extent arr3))
                                        (intersectDim (extent arr4) (extent arr5))))

-- | Combines the elements of two vectors. The size of the resulting vector
--   will be the intersection of the two argument vectors.
zip :: (Pully vec1, VecShape vec1 ~ sh, Pully vec2, VecShape vec2 ~ sh, Shapely sh) =>
       vec1 a -> vec2 b -> Pull sh (a,b)
zip = zipWith (\a b -> (a,b))

-- | Like 'zip' but combining three vectors.
zip3 :: (Pully vec1, VecShape vec1 ~ sh, Pully vec2, VecShape vec2 ~ sh, Pully vec3, VecShape vec3 ~ sh) =>
        vec1 a -> vec2 b -> vec3 c ->
        Pull sh (a,b,c)
zip3 vec1 vec2 vec3
  = Pull (\i -> (ixf1 i, ixf2 i, ixf3 i))
         (intersectDim (intersectDim sh1 sh2) sh3)
  where Pull ixf1 sh1 = toPull vec1
        Pull ixf2 sh2 = toPull vec2
        Pull ixf3 sh3 = toPull vec3

-- | Zipping four vectors
zip4 :: (Pully vec1, VecShape vec1 ~ sh, Pully vec2, VecShape vec2 ~ sh, Pully vec3, VecShape vec3 ~ sh, Pully vec4, VecShape vec4 ~ sh) =>
        vec1 a -> vec2 b -> vec3 c -> vec4 d ->
        Pull sh (a,b,c,d)
zip4 vec1 vec2 vec3 vec4
  = Pull (\i -> (ixf1 i, ixf2 i, ixf3 i, ixf4 i)) (intersectDim (intersectDim sh1 sh2) (intersectDim sh3 sh4))
  where Pull ixf1 sh1 = toPull vec1
        Pull ixf2 sh2 = toPull vec2
        Pull ixf3 sh3 = toPull vec3
        Pull ixf4 sh4 = toPull vec4

-- | Zipping five vectors
zip5 :: (Pully vec1, VecShape vec1 ~ sh, Pully vec2, VecShape vec2 ~ sh, Pully vec3, VecShape vec3 ~ sh, Pully vec4, VecShape vec4 ~ sh, Pully vec5, VecShape vec5 ~ sh) =>
        vec1 a -> vec2 b -> vec3 c -> vec4 d -> vec5 e ->
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
fold :: (Syntax b, Pully vec, VecShape vec ~ (sh :. Data Length), Shapely sh) =>
        (b -> a -> b)
     -> b
     -> vec a
     -> Pull sh b
fold f x vec = Pull ixf sh
    where (sh, n) = uncons (extent vec) -- brain explosion hack
          ixf i = forLoop n x (\ix s -> f s (vec !: (i :. ix)))

-- Here's another version of fold which has a little bit more freedom
-- when it comes to choosing the initial element when folding

-- | A generalization of 'fold' which allows for different initial
--   values when starting to fold.
fold' :: (Syntax a, Pully vec1, VecShape vec1 ~ sh, Pully vec2, VecShape vec2 ~ (sh :. Data Length), Shapely sh)
      => (a -> a -> a)
      -> vec1 a
      -> vec2 a
      -> Pull sh a
fold' f x vec = Pull ixf sh
    where (sh, n) = uncons (extent vec) -- brain explosion hack
          ixf i = forLoop n (x!:i) (\ix s -> f s (vec !: (i :. ix)))

-- | Summing a vector along its outermost dimension
sum :: (Syntax a, Num a, Pully vec, VecShape vec ~ (sh :. Data Length), Shapely sh) =>
       vec a -> Pull sh a
sum = fold (+) 0

-- | Concatenating shapes.
class ShapeConc sh1 sh2 where
  type ShapeConcT sh1 sh2
  shapeConc :: Shape sh1 -> Shape sh2 -> Shape (ShapeConcT sh1 sh2)

  splitIndex :: Shape (ShapeConcT sh1 sh2) -> Shape sh2 -> (Shape sh1,Shape sh2)

instance ShapeConc sh1 Z where
  type ShapeConcT sh1 Z = sh1
  shapeConc sh1 Z = sh1

  splitIndex sh Z = (sh,Z)

instance ShapeConc sh1 sh2 => ShapeConc sh1 (sh2 :. Data Length) where
  type ShapeConcT sh1 (sh2 :. Data Length) = ShapeConcT sh1 sh2 :. Data Length
  shapeConc sh1 (sh2 :. l) = shapeConc sh1 sh2 :. l

  splitIndex (sh :. i) (sh1 :. _) = (i1,i2 :. i)
    where (i1,i2) = splitIndex sh sh1

-- | Flatten nested pull vectors.
flatten :: forall a sh1 sh2.
           (Shapely (ShapeConcT sh1 sh2), ShapeConc sh1 sh2) =>
           Pull sh1 (Pull sh2 a) ->
           Pull (ShapeConcT sh1 sh2) a
flatten (Pull ixf1 sh1) = Pull ixf sh
  where ixf i = let (i1,i2) = splitIndex i sh2
                    (Pull ixf2 sh2) = ixf1 i1
                in ixf2 i2
        sh = let (i1,_ :: Shape sh2) = splitIndex fakeShape sh2
                 (Pull _ sh2) = ixf1 i1
             in shapeConc sh1 sh2

-- | Flatten a pull vector of push vectors.
flattenPush :: forall a sh1 sh2.
               (Shapely (ShapeConcT sh1 sh2), ShapeConc sh1 sh2) =>
               Pull sh1 (Push sh2 a) ->
               Push (ShapeConcT sh1 sh2) a
flattenPush (Pull ixf sh1) = Push f sh
  where f :: PushK (ShapeConcT sh1 sh2) a
        f k = parForShape sh1 $ \i ->
                let Push g sh' = ixf i
                in  g (\j a -> k (shapeConc i j) a)
        sh = let (i1,_ :: Shape sh2) = splitIndex fakeShape sh2
                 (Push _ sh2) = ixf i1
             in shapeConc sh1 sh2

type family AppendShape sh1 sh2 where
  AppendShape sh1 (sh2 :. i) = AppendShape sh1 sh2 :. i
  AppendShape sh1 Z = sh1

appendShape :: Shape sh1 -> Shape sh2 -> Shape (AppendShape sh1 sh2)
appendShape sh1 (sh2 :. i) = appendShape sh1 sh2 :. i
appendShape sh1 Z = sh1

splitShape :: Shape (AppendShape sh1 sh2) -> Shape sh2 -> (Shape sh1, Shape sh2)
splitShape (sh1 :. i) (sh2 :. _) = (sh1L, sh1R :. i)
  where (sh1L,sh1R) = splitShape sh1 sh2
splitShape sh Z = (sh, Z)

leftFlatten :: forall sh1 sh2 a . Shapely sh1 => Pull sh1 (Pull sh2 a) -> Pull (AppendShape sh2 sh1) a
leftFlatten (Pull ixf1 bnds1) = Pull ixf bnds
  where ixf ix = let (ix2,ix1) = splitShape ix bnds1  -- The outer shape in the original vector is the rightmost part in the new
                     Pull ixf2 _ = ixf1 ix1
                 in ixf2 ix2
        bnds = appendShape bnds2 bnds1
        Pull _ bnds2 = ixf1 (fakeShape :: Shape sh1)

leftFlatten' :: forall sh1 sh2 a . Pull sh1 (Pull sh2 a) -> Pull (AppendShape sh2 sh1) a
leftFlatten' (Pull ixf1 bnds1) = Pull ixf bnds
  where ixf ix = let (ix2,ix1) = splitShape ix bnds1  -- The outer shape in the original vector is the rightmost part in the new
                     Pull ixf2 _ = ixf1 ix1
                 in ixf2 ix2
        bnds = appendShape bnds2 bnds1
        Pull _ bnds2 = ixf1 bnds1

rightFlatten :: forall sh1 sh2 a . (Shapely sh1, Shapely sh2) => Pull sh1 (Pull sh2 a) -> Pull (AppendShape sh1 sh2) a
rightFlatten (Pull ixf1 bnds1) = Pull ixf bnds
  where ixf ix = let (ix1,ix2) = splitShape ix (fakeShape :: Shape sh2)
                     Pull ixf2 _ = ixf1 ix1
                 in ixf2 ix2
        bnds = appendShape bnds1 bnds2
        Pull _ bnds2 = ixf1 (fakeShape :: Shape sh1)

rightFlatten' :: forall sh1 sh2 a . Pull sh1 (Pull sh2 a) -> Pull (AppendShape sh1 sh2) a
rightFlatten' (Pull ixf1 bnds1) = Pull ixf bnds
  where ixf ix = let (ix1,ix2) = splitShape ix bnds2
                     Pull ixf2 _ = ixf1 ix1
                 in ixf2 ix2
        bnds = appendShape bnds1 bnds2
        Pull _ bnds2 = ixf1 bnds1

leftNest :: forall sh1 sh2 a . Shapely sh2 => Pull (AppendShape sh1 sh2) a -> Pull sh2 (Pull sh1 a)
leftNest (Pull ixf bnds) = Pull ixf2 bnds2
  where ixf2 ix2 = Pull (\ ix1 -> ixf $ appendShape ix1 ix2) bnds1
        (bnds1,bnds2) = splitShape bnds (fakeShape :: Shape sh2)

leftNest' :: Shape sh2 -> Pull (AppendShape sh1 sh2) a -> Pull sh2 (Pull sh1 a)
leftNest' fake (Pull ixf bnds) = Pull ixf2 bnds2
  where ixf2 ix2 = Pull (\ ix1 -> ixf $ appendShape ix1 ix2) bnds1
        (bnds1,bnds2) = splitShape bnds fake

leftMap :: forall sh1a sh1b sh2 a b . Shape sh2 ->
                   (Pull sh1a a -> Pull sh1b b) ->
                   Pull (AppendShape sh1a sh2) a ->
                   Pull (AppendShape sh1b sh2) b
leftMap sh f v = leftFlatten' $ fmap f $ leftNest' sh v

rightNest :: forall sh1 sh2 a . Shapely sh2 => Pull (AppendShape sh1 sh2) a -> Pull sh1 (Pull sh2 a)
rightNest (Pull ixf bnds) = Pull ixf2 bnds1
  where ixf2 ix1 = Pull (\ ix2 -> ixf $ appendShape ix1 ix2) bnds2
        (bnds1,bnds2) = splitShape bnds (fakeShape :: Shape sh2)

rightNest' :: forall sh1 sh2 a . Shape sh2 -> Pull (AppendShape sh1 sh2) a -> Pull sh1 (Pull sh2 a)
rightNest' sh2 (Pull ixf bnds) = Pull ixf2 bnds1
  where ixf2 ix1 = Pull (\ ix2 -> ixf $ appendShape ix1 ix2) bnds2
        (bnds1,bnds2) = splitShape bnds sh2

rightMap :: forall sh1a sh1b sh2 a b . Shape sh1a -> Shape sh2 ->
                   (Pull sh1a a -> Pull sh1b b) ->
                   Pull (AppendShape sh2 sh1a) a ->
                   Pull (AppendShape sh2 sh1b) b
rightMap fake s f v = rightFlatten' $
                       fmap f (rightNest' fake v :: Pull sh2 (Pull sh1a a))

outerProd :: (Pully vec1, Pully vec2) =>
             (a -> b -> c) -> vec1 a -> vec2 b ->
             Pull (AppendShape (VecShape vec1) (VecShape vec2)) c
outerProd f v1 v2 = Pull ixf (appendShape sh1 sh2)
  where Pull ixf1 sh1 = toPull v1
        Pull ixf2 sh2 = toPull v2
        ixf shi = let (shi1,shi2) = splitShape shi sh2
                  in f (ixf1 shi1) (ixf2 shi2)

slide :: (Pully vec1, VecShape vec1 ~ DIM2) => vec1 a -> Pull DIM2 a
slide v = Pull (\ (Z :. dy :. dx) -> ixf (Z :. (dy + dx) `mod` y :. dx)) sh
  where Pull ixf sh = toPull v
        [y,x] = toList sh

slideS :: forall vec1 a . (Pully vec1, VecShape vec1 ~ DIM2) => vec1 a -> Push DIM2 a
slideS v = Push f sh
  where Pull ixf sh = toPull v
        [y,x] = toList sh
        f :: PushK DIM2 a
        f wf = parFor x $ \dx ->
                 par
                   (parFor dx $ \dy ->
                      wf (Z :. dy :. dx) (ixf (Z :. (y - dx + dy) :. dx)))
                   (parFor (y - dx) $ \dy ->
                      wf (Z :. dy + dx :. dx) (ixf (Z :. dy :. dx)))

-- stencil v s = reduce $ transposeL $ outerprod (*) v s

interior :: Data Length -> Pull sh a -> Pull sh a
interior n (Pull ixf sh) = Pull ixf' (interiorSh sh)
  where ixf' sh = ixf (interiorIx sh)
        interiorIx :: Shape sh -> Shape sh
        interiorIx Z = Z
        interiorIx (sh :. i) = interiorIx sh :. (i + n)
        interiorSh :: Shape sh -> Shape sh
        interiorSh Z = Z
        interiorSh (sh :. i) = interiorSh sh :. (i - 2 * n)

takeSh :: Pull sh a -> Shape sh -> Pull sh a
takeSh (Pull ixf sh) shn = Pull ixf (intersectDim sh shn)

dropSh :: Pull sh a -> Shape sh -> Pull sh a
dropSh (Pull ixf sh) shn = Pull ixf' (zipShape (-) sh shn)
  where ixf' shi = ixf (zipShape (+) shi shn)

-- | Create a two-dimensional Pull vector
indexed2 :: Data Length -> Data Length -> (Data Index -> Data Index -> a) -> Pull DIM2 a
indexed2 l1 l2 ixf = Pull (\(Z :. i1 :. i2) -> ixf i1 i2) (Z :. l1 :. l2)

-- Matrix Multiplication

-- | Transpose the two innermost dimensions of a vector
transposeL :: forall sh e vec.
              (Pully vec, VecShape vec ~ (sh :. Data Length :. Data Length)
              ,Shapely sh) =>
              vec e ->
              Pull (sh :. Data Length :. Data Length) e
transposeL vec
  = backpermute new_extent swap vec
  where swap ((tail :: Shape sh) :. i :. j) = tail :. j :. i
        new_extent         = swap (extent vec)

-- | Transpose a two-dimensional vector
transpose2D :: (Pully vec, VecShape vec ~ DIM2) => vec e -> Pull DIM2 e
transpose2D = transposeL

-- | Matrix multiplication
mmMult :: (Syntax e, Num e
          ,Pully vec1, VecShape vec1 ~ DIM2
          ,Pully vec2, VecShape vec2 ~ DIM2) =>
          Bool -> vec1 e -> vec2 e -> Pull DIM2 e
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

above, beside :: forall vec1 vec2 a .
                 (Pushy vec1, VecShape vec1 ~ DIM2,
                  Pushy vec2, VecShape vec2 ~ DIM2) =>
                 vec1 a -> vec2 a -> Push DIM2 a
above vec1 vec2 = Push ixf (Z :. y1 + y2 :. x1)
  where Push ixf1 ext1 = toPush vec1
        Push ixf2 ext2 = toPush vec2
        [x1,y1] = toList ext1
        [x2,y2] = toList ext2
        -- Assumption x1 == x2
        ixf :: PushK DIM2 a
        ixf wf = ixf1 wf `par`
                 ixf2 (\ (Z :. y :. x) a -> wf (Z :. y + y1 :. x) a)
-- above vec1 vec = transpose (transpose (toPush vec1) `beside`
--                             transpose (toPush vec2))
-- The definition of 'above' in terms of 'beside' and 'transpose' is
-- almost as efficient as the actual implementation above. However,
-- the iteration order makes it such that multiplications get pushed
-- inside of loops making it do many more multiplications.
-- I haven't benchmarked the two versions yet though.

beside vec1 vec2 = vec1 ++ vec2

class Matrixy vec where
  columnVector :: vec DIM1 a -> vec DIM2 a
  rowVector :: vec DIM1 a -> vec DIM2 a

instance Matrixy Pull where
  columnVector (Pull ixf (Z :. l)) = Pull ixf' (Z :. l :. 1)
    where ixf' (Z :. i :. _) = ixf (Z :. i)
  rowVector (Pull ixf (Z :. l)) = Pull ixf' (Z :. 1 :. l)
    where ixf' (Z :. _ :. i) = ixf (Z :. i)

instance Matrixy Push where
  columnVector (Push ixf (Z :. l)) = Push (ixf' ixf) (Z :. l :. 1)
    where ixf' :: PushK DIM1 a -> PushK DIM2 a
          ixf' ixf wf = ixf (\ (Z :. i) a -> wf (Z :. i :. 0) a)
  rowVector (Push ixf (Z :. l)) = Push (ixf' ixf) (Z :. 1 :. l)
    where ixf' :: PushK DIM1 a -> PushK DIM2 a
          ixf' ixf wf = ixf (\ (Z :. i) a -> wf (Z :. 0 :. i) a)

-- | A square identity matrix.
eye :: (Num e, Syntax e) => Data WordN -> Pull DIM2 e
eye n = Pull (\(Z :. i :. j) -> i == j ? 1 $ 0) (Z :. n :. n)

-- | A square identity matrix. This version can sometimes be faster
--   than 'eye' as it avoids a test in the inner loop. However,
--   half of the elements are written in an order which is not
--   cache friendly.
eyePush :: Num e => Data WordN -> Push DIM2 e
eyePush n = Push ixf (Z :. n :. n)
  where ixf :: Num a => PushK DIM2 a
        ixf wf = par (parFor n $ \i ->
                        parFor i $ \j ->
                          wf (Z :. i :. j) 0 `par`
                          wf (Z :. j :. i) 0)
                     (parFor n $ \i ->
                        wf (Z :. i :. i) 1)

-- | An identity matrix which is not necessarily square.
eye2 :: (Num e, Syntax e) => Data WordN -> Data WordN -> Pull DIM2 e
eye2 n m = Pull (\(Z :. i :. j) -> i == j ? 1 $ 0) (Z :. n :. m)

-- | Similar to 'eye2' but potentially more efficient in the same
--   way that 'eyePush' relates to 'eye'.
eye2Push :: Num e => Data WordN -> Data WordN -> Push DIM2 e
eye2Push n m = ifPush (n > m)
                 (eyePush m `above`  zeros (Z :. n - m :. m))
                 (eyePush n `beside` zeros (Z :. n :. m - n))

-- KFFs combinators

-- | Split the innermost dimension of a pull vector in two. If the innermost
-- dimension of 'vec' has length 'i' then the innermost dimension of
-- @expandL n vec@ has length 'n' and the second innermost dimension will have
-- length @i / p@.
expandL :: (Pully vec, VecShape vec ~ (sh :. Data Length)) =>
           Data Length -> vec a ->
           Pull (sh :. Data Length :. Data Length) a
expandL n v = Pull ixf' (insLeft n $ insLeft p ext')
  where (Pull ixf ext) = toPull v
        (m, ext') = peelLeft ext
        p = m `div` n
        ixf' ix = let (i, ix')  = peelLeft ix
                      (j, ix'') = peelLeft ix'
                  in ixf $ insLeft (i*p + j) ix''

-- | Flatten the two innermost dimensions into a single dimension
contractL :: (Pully vec, VecShape vec ~ (sh :. Data Length :. Data Length)) =>
             vec a ->
             Pull (sh :. Data Length) a
contractL v = Pull ixf' (insLeft (m*n) ext')
  where (Pull ixf ext) = toPull v
        (m, n, ext') = peelLeft2 ext
        ixf' ix = let (i, ix') = peelLeft ix
                  in ixf $ insLeft (i `div` n) $ insLeft (i `mod` n) ix'

-- | Swap the two innermmost dimensions
transL :: (Pully vec, VecShape vec ~ (sh :. Data Length :. Data Length)) =>
          vec a ->
          Pull (sh :. Data Length :. Data Length) a
transL v = Pull ixf' (insLeft n $ insLeft m ext')
  where (Pull ixf ext) = toPull v
        (m, n, ext') = peelLeft2 ext
        ixf' ix = let (i, j, ix') = peelLeft2 ix
                  in ixf $ insLeft j $ insLeft i ix'

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

curry' :: Pull (sh :. Data Length) a ->
          (Data Length, Data Length -> Pull sh a)
curry' (Pull ixf (sh:.l)) = (l, \i -> Pull (\ix -> ixf (ix :. i)) sh)

uncurry' :: Data Length -> (Data Length -> Pull sh a) -> Pull (sh :. Data Length) a
uncurry' n f = Pull ixf (sh :. n)
  where Pull _ sh   = f (undefined :: Data Length)
        ixf (sh :.i) = let Pull ixf' _ = f i in ixf' sh

dmap' :: (Pull sh1 a1 -> Pull sh2 a2) -> Pull (sh1 :. Data Length) a1 -> Pull (sh2 :. Data Length) a2
dmap' f a = uncurry' n $ f . g
  where (n,g) = curry' a

-- Convenience functions that maybe should not be in the lib

expandLT :: (Pully vec, VecShape vec ~ (sh :. Data Length)) =>
            Data Length -> vec a ->
            Pull (sh :. Data Length :. Data Length) a
expandLT n a = transL $ expandL n a

contractLT :: (Pully vec, VecShape vec ~ (sh :. Data Length :. Data Length)) =>
              vec a ->
              Pull (sh :. Data Length) a
contractLT a = contractL $ transL a

-- | Transform a pull vector to a push vector, but preserve the innermost
--   dimension
dmapS :: (Pull sh1 a1 -> Push sh2 a2) -> Pull (sh1 :. Data Length) a1 -> Push (sh2 :. Data Length) a2
dmapS f a = uncurryS n $ f . g
  where (n,g) = curryL a

dmapS' f a = uncurryS' n $ f . g
  where (n,g) = curry' a

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

value1 :: (Syntax a, Hashable (Internal a)) => [Internal a] -> Manifest DIM1 a
value1 = value

-- | Create a one-dimensional Pull vector
indexed1 :: Data Length -> (Data Index -> a) -> Pull DIM1 a
indexed1 l ixf = Pull (\(Z :. i) -> ixf i) (Z :. l)

(!!) :: (Pully vec, VecShape vec ~ DIM1) => vec a -> Data Index -> a
vec !! i = vec !: (Z :. i)

-- | Take the length of a one-dimensional vector
length :: (Shaped vec, VecShape vec ~ DIM1) => vec a -> Data Length
length vec = l
  where (Z,l) = uncons (extent vec)

-- | The call @take n vec@ returns a Pull vector containing the first @n@
--   elements of @vec@.
take :: (Pully vec, VecShape vec ~ (sh :. Data Length)) =>
        Data Length -> vec a -> Pull (sh :. Data Length) a
take n vec = Pull ixf (shr :. min n l)
  where Pull ixf sh = toPull vec
        (shr,l)     = uncons sh

-- | The call @drop n vec@ removes the first @n@ elements of @vec@
drop :: (Pully vec, VecShape vec ~ DIM1) => Data Length -> vec a -> Pull DIM1 a
drop n vec = Pull (\(Z :. i) -> ixf (Z :. i+n)) (Z :. (l-n))
  where Pull ixf sh = toPull vec
        (Z,l)       = uncons sh

-- | Splits a pull vector in two at a particular index
splitAt :: (Pully vec, VecShape vec ~ DIM1) =>
           Data Index -> vec a -> (Pull DIM1 a, Pull DIM1 a)
splitAt n vec = (take n vec, drop n vec)

-- | Take the first element of a pull vector
head :: (Pully vec, VecShape vec ~ DIM1) => vec a -> a
head vec = ixf (Z :. 0)
  where Pull ixf _ = toPull vec

-- | Take the last element of a pull vector
last :: (Pully vec, VecShape vec ~ DIM1) => vec a -> a
last vec = ixf (Z :. (l-1))
  where Pull ixf sh = toPull vec
        (Z,l)       = uncons sh

-- | Remove the first element of a pull vector
tail :: (Pully vec, VecShape vec ~ DIM1) => vec a -> Pull DIM1 a
tail = drop 1

-- | Remove the last element of a pull vector
init :: (Pully vec, VecShape vec ~ DIM1) => vec a -> Pull DIM1 a
init vec = take (length vec - 1) vec

-- | Create a vector containing all the suffixes of the input vector
tails :: (Pully vec, VecShape vec ~ DIM1) => vec a -> Pull DIM1 (Pull DIM1 a)
tails vec = indexed1 (length vec + 1) (`drop` vec)

-- | Create a vector containing all the prefixes of the input vector
inits :: (Pully vec, VecShape vec ~ DIM1) => vec a -> Pull DIM1 (Pull DIM1 a)
inits vec = indexed1 (length vec + 1) (`take` vec)

-- | Similar to 'inits' but without the empty vector
inits1 :: (Pully vec, VecShape vec ~ DIM1) => vec a -> Pull DIM1 (Pull DIM1 a)
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
enumFromTo m n = indexed1 (i2n l) ((+m) . i2n)
  where
    l = n < m ? 0 $ n - m + 1

enumFromThenTo :: forall a. (Type a, Integral a)
               => Data a -> Data a -> Data a -> Pull DIM1 (Data a)
enumFromThenTo m k n = indexed1 (i2n l) (\i -> i2n i * s + m)
  where
    l = (n-m) `div` s
    s = k-m

enumFromThenToF :: forall a. (Type a, P.RealFloat a, Fraction a)
                => Data a -> Data a -> Data a -> Pull DIM1 (Data a)
enumFromThenToF m k n = indexed1 (truncate l) (\i -> i2f i * s + m)
  where
    l = (n-m) / s
    s = k-m

enumFromStepToF :: forall a. (Type a, P.RealFloat a, Fraction a)
                => Data a -> Data a -> Data a -> Pull DIM1 (Data a)
enumFromStepToF m s n = indexed1 (truncate l) (\i -> i2f i * s + m)
  where
    l = (n-m) / s

-- | @enumFrom m@: Enumerate the indexes from @m@ to 'maxBound'
enumFrom m = enumFromTo m (value maxBound)

-- | An infix version of 'enumFromTo'.
(...) :: forall a. (Type a, Integral a)
      => Data a -> Data a -> Pull DIM1 (Data a)
(...) = enumFromTo

scan :: (Syntax a, Syntax b) => (a -> b -> a) -> a -> Pull DIM1 b -> Pull DIM1 a
scan f init bs = toPull $ arrToManifest
  (F.sugar $ sequential (length bs) (F.desugar init) $ \i s ->
    let s' = F.desugar $ f (F.sugar s) (bs!!i)
    in  (s',s'), build $ tuple (length bs))

-- | Transform all the elements of a vector
map :: Functor vec => (a -> b) -> vec a -> vec b
map = fmap

-- | Folding a one-dimensional vector
fold1 :: (Syntax a, Pully vec, VecShape vec ~ DIM1) => (a -> a -> a) -> vec a -> a
fold1 f a = fromZero $ fold f (head a) (tail a)

-- TODO: Generalize minimum and maximum to arbitrary dimensions and Pushy vecs


-- | Get the maximum element from a vector
maximum :: (Ord a, Pully vec, VecShape vec ~ DIM1) => vec (Data a) -> Data a
maximum = fold1 max

-- | Get the minimum element from a vector
minimum :: (Ord a, Pully vec, VecShape vec ~ DIM1) => vec (Data a) -> Data a
minimum = fold1 min

-- TODO: Generalize or and and to arbitrary dimensions and Pushy vectors

-- | Compute logical or over a vector
or :: (Pully vec, VecShape vec ~ DIM1) => vec (Data Bool) -> Data Bool
or vec = snd (whileLoop (0,false) (\(i,b) -> not b && i < l) body)
  where body (i,b)  = (i+1,ixf (Z :. i))
        Pull ixf sh = toPull vec
        (Z,l)       = uncons sh

-- | Compute logical and over a vector
and :: (Pully vec, VecShape vec ~ DIM1) => vec (Data Bool) -> Data Bool
and vec = snd (whileLoop (0,true) (\(i,b) -> b && i < l) body)
  where body (i,b)  = (i+1,ixf (Z :. i))
        Pull ixf sh = toPull vec
        (Z,l)       = uncons sh

-- | Determine whether any of the elements of a vector satisfies a predicate
any :: (Pully vec, VecShape vec ~ DIM1, Functor vec) =>
       (a -> Data Bool) -> vec a -> Data Bool
any p = or . fmap p

-- | Determine whether all elements of a vector satisfies a predicate
all :: (Pully vec, VecShape vec ~ DIM1, Functor vec) =>
       (a -> Data Bool) -> vec a -> Data Bool
all p = and . fmap p

-- TODO: Generalize eqVectors, once 'and' is generalized

-- | Testing equality between two one-dimensional vectors
eqVector :: (Eq a, Pully vec1, VecShape vec1 ~ DIM1
            ,Pully vec2, VecShape vec2 ~ DIM1) =>
            vec1 (Data a) -> vec2 (Data a) -> Data Bool
eqVector a b = length a == length b && and (zipWith (==) a b)

-- | Compute the scalar product of two pull vectors
scalarProd :: (Syntax a, Num a, Pully vec1, VecShape vec1 ~ DIM1
              ,Pully vec2, VecShape vec2 ~ DIM1) =>
              vec1 a -> vec2 a -> a
scalarProd a b = fromZero $ sum (zipWith (*) a b)

-- | This function can distribute vector computations on chunks of a large
--   pull vector. A call @chunk l f g v@ will split the vector 'v' into chunks
--   of size 'l' and apply 'f' to these chunks. In case the length of 'v' is
--   not a multiple of 'l' then the rest of 'v' will be processed by 'g'.
chunk :: forall vec vec1 vec2 a b .
         (Pully vec, VecShape vec ~ DIM1
         ,Pushy vec1, VecShape vec1 ~ DIM1
         ,Pushy vec2, VecShape vec2 ~ DIM1, Syntax b)
      => Data Length            -- ^ Size of the chunks
      -> (Pull DIM1 a -> vec1 b) -- ^ Applied to every chunk
      -> (Pull DIM1 a -> vec2 b) -- ^ Applied to the rest of the vector
      -> vec a
      -> Push DIM1 b
chunk c f g vec = Push loop (Z :. (noc * c))
             ++ toPush (g (drop (noc * c) v))
  where l = length v
        noc = l `div` c
        loop :: PushK DIM1 b
        loop func = parFor noc $ \i ->
                         let (Push k _) = toPush $ f (take c (drop (c*i) v))
                         in k (\(Z :. j) a -> func (Z :. (j + c*i)) a)
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

-- | Duplicates a vector
dup :: (Pushy vec, VecShape vec ~ DIM1) => vec a -> Push DIM1 a
dup vec = vec ++ vec

-- Multidimensional push vectors
data Push sh a = Push (PushK sh a) (Shape sh)

type PushK sh a = forall b . Type b => (Shape sh -> a -> Data (Elements b)) -> Data (Elements b)

type DPush sh a = Push sh (Data a)

instance Functor (Push sh) where
  fmap :: forall a b. (a -> b) -> Push sh a -> Push sh b
  fmap f (Push k l) = Push k' l
   where k' :: PushK sh b
         k' func   = k (\sh a -> func sh (f a))

-- | The empty push vector.
empty :: Shapely sh => Push sh a
empty = Push (const skip) zeroDim

-- | Concatenation along the the outmost dimension
(++) :: forall vec1 vec2 sh a .
        (Pushy vec1, VecShape vec1 ~ (sh :. Data Length)
        ,Pushy vec2, VecShape vec2 ~ (sh :. Data Length)) =>
        vec1 a
     -> vec2 a
     -> Push (sh :. Data Length) a
vec1 ++ vec2 = Push k (sh1 :. (l1 + l2))
  where Push k1 ext1 = toPush vec1
        Push k2 ext2 = toPush vec2
        (sh1,l1) = uncons ext1
        (sh2,l2) = uncons ext2
        k :: PushK (sh :. Data Length) a
        k func = k1 func
                 `par`
                 k2 (\ (sh :. i) a -> func (sh :. (i + l1)) a)
-- Assumption sh1 == sh2

-- | Concatenation along the outermost dimension where the two vectors have
--   the same length. There is no check that the lengths are equal.
(+=+) :: forall vec1 vec2 sh a .
         (Pully vec1, VecShape vec1 ~ (sh :. Data Length)
         ,Pully vec2, VecShape vec2 ~ (sh :. Data Length)) =>
         vec1 a
      -> vec2 a
      -> Push (sh :. Data Length) a
vec1 +=+ vec2 = Push f (sh1 :. (l1 + l2))
  where Pull ixf1 ext1 = toPull vec1
        Pull ixf2 ext2 = toPull vec2
        (sh1,l1) = uncons ext1
        (sh2,l2) = uncons ext2
        f :: PushK (sh :. Data Length) a
        f k = parForShape (sh1 :. l1) $ \ (shi :. i) ->
                k (shi :. i)      (ixf1 (shi :. i)) `par`
                k (shi :. i + l1) (ixf2 (shi :. i))

-- | Flattens a vector of pairs such that the elements of a pair end up next
--   to each other in the resulting vector.
unpair :: forall vec sh a . (Pushy vec, VecShape vec ~ (sh :. Data Length))
       => vec (a,a)
       -> Push (sh :. Data Length) a
unpair v = Push f' (sh :. (l * 2))
  where (Push f ex) = toPush v
        (sh,l) = uncons ex
        f' :: PushK (sh :. Data Length) a
        f' k = f (\ (sh :. i) (a,b) -> k (sh :. (2 * i)) a
                                    `par` k (sh :. (2 * i + 1)) b)

-- | Similar to 'unpair' but allows control over where the elements end up
--   in the result vector.
--
-- @
--   unpair = unpairWith (\(sh :. i) -> (sh :. 2*i)) (\(sh :. i) -> (sh :. 2*i+1))
-- @
unpairWith :: forall vec sh a . (Pushy vec, VecShape vec ~ (sh :. Data Length))
           => (Shape (sh :. Data Length) -> Shape (sh :. Data Length))
           -> (Shape (sh :. Data Length) -> Shape (sh :. Data Length))
           -> vec (a,a)
           -> Push (sh :. Data Length) a
unpairWith ix1 ix2 vec = Push f' (sh :. (l*2))
  where (Push f ex) = toPush vec
        (sh,l) = uncons ex
        f' :: PushK (sh :. Data Length) a
        f' k = f (\ix (a,b) -> k (ix1 ix) a `par` k (ix2 ix) b)

-- Some helper functions in Repa to help us define riffle

-- | Split a vector in half along the outermost dimension. If there is an odd
--   number of elements in that dimension, the second vector of the two result
--   vectors will be one longer than the first.
halve :: (Pully vec, VecShape vec ~ (sh :. Data Length))
      => vec a
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
riffle :: (Pully vec, VecShape vec ~ (sh :. Data Length), Shapely sh) =>
          vec a -> Push (sh :. Data Length) a
riffle =  unpair . uncurry zip . halve

-- | Interleaves the elements of two vectors.
interleave :: (Pully vec1, VecShape vec1 ~ (sh :. Data Length)
              ,Pully vec2, VecShape vec2 ~ (sh :. Data Length)
              ,Shapely sh) =>
              vec1 a -> vec2 a ->
              Push (sh :. Data Length) a
interleave v1 v2 = unpair (zip v1 v2)

-- | Forward permute a push vector.
forwardPermute :: forall vec sh a . (Pushy vec, VecShape vec ~ sh) =>
                  (Shape sh -> Shape sh -> Shape sh) ->
                  vec a ->  Push sh a
forwardPermute p vec = Push g sh
  where Push f sh = toPush vec
        g :: PushK sh a
        g k = f (\ix a -> k (p sh ix) a)

reversePush :: Push (sh :. Data Length) a -> Push (sh :. Data Length) a
reversePush (Push f (sh :. l)) =
  Push (\k -> f (\(sh' :. ix) a -> k (sh' :. (l - ix - 1)) a)) (sh :. l)

ifPush :: forall sh a . Data Bool -> Push sh a -> Push sh a -> Push sh a
ifPush cond (Push ixf1 sh1) (Push ixf2 sh2) = Push ixf sh
  where ixf :: PushK sh a
        ixf wf = cond ? ixf1 wf $ ixf2 wf
        sh     = zipShape (cond ?) sh1 sh2

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
conc :: forall sel sh a . Selector sel sh =>
         Select sel -> Push sh a -> Push sh a -> Push sh a
conc s (Push k1 sh1) (Push k2 sh2)
     = Push k (adjustDimension s (+ selectDimension s sh2) sh1)
  where k :: PushK sh a
        k func = k1 func
                 `par`
                 k2 (\ sh a -> func (adjustDimension s (+ selectDimension s sh1) sh) a)
-- Assumption sh1 == sh2

-- | Reverse a vector along a particular dimension.
rev :: forall sel sh a . Selector sel sh =>
       Select sel -> Push sh a -> Push sh a
rev s (Push k sh) = Push k' sh
  where k' :: PushK sh a
        k' func = k (\sh a -> func (adjustDimension s (selectDimension s sh -) sh) a)

-- | This class captures all types of vectors which can turned into a 'Push'
--   vector cheaply
class (Shaped vec) => Pushy vec where
  toPush :: vec a -> Push (VecShape vec) a

instance Pushy (Push sh) where
  toPush = id

instance Pushy (Pull sh) where
  toPush (Pull ixf l) = Push (\k -> parForShapeR l $ \ i ->
                                      k i (ixf i))
                             l

-- | Store a vector in memory as a flat array
fromPush :: Type a => Push sh (Data a) -> Data [a]
fromPush (Push ixf l) = materialize (size l) $
                          ixf (\ix a -> write (toIndex l ix) a)

freezePush :: (Type a, Shapely sh, ShapeTup sh) =>
              Push sh (Data a) -> Tuple '[Data [a], Tuple (ShapeTupT sh)]
freezePush v = twotup (fromPush v) (toTup $ extent v)

freezePush1 :: Type a => Push DIM1 (Data a) -> Data [a]
freezePush1 = fromPush

thawPush :: forall a sh . (Type a, Shapely sh, ShapeTup sh) =>
              Tuple '[Data [a], Tuple (ShapeTupT sh)] -> Push sh (Data a)
thawPush larr = Push f sh
  where sh = fromTup $ nsnd larr
        f :: PushK sh (Data a)
        f k = parForShape sh $ \i ->
                k i (nfst larr ! toIndex sh i)

thawPush1 :: Type a => Data [a] -> Push DIM1 (Data a)
thawPush1 = toPush . thawPull1

instance (Syntax a, Shapely sh, ShapeTup sh, SyntacticTup (ShapeTupT sh)) => Syntactic (Push sh a)
  where
    type Internal (Push sh a) = InternalShape sh a

    desugar v@(Push _ sh) = case sh of
        Z             -> desugar v
        (Z :. _)      -> desugar $ fromPush $ fmap resugar v
        (Z :. _ :. _) -> desugar $ freezePush $ fmap resugar v

    sugar v = case fakeShape :: Shape sh of
        Z           -> toPush $ singleton $ sugar v
        Z :. _      -> fmap resugar $ thawPush1 $ sugar v
        _ :. _ :. _ -> fmap resugar $ thawPush $ sugar v

-- | Flatten a pull vector of lists so that the lists become an extra dimension
flattenList :: forall sh a . Shapely sh => Pull sh [a] -> Push (sh :. Data Length) a
flattenList (Pull ixf sh) = Push f sz
  where f :: PushK (sh :. Data Length) a
        f k = parForShape sh $ \i ->
                 let indices = fmap (i :.) $
                                 fmap value [0..l-1]
                  in P.foldr (\ (i,a) e -> k i a `par` e) skip $ P.zip indices (ixf i)
        sz  = sh :. value l
        l   = P.fromIntegral $
              P.length (ixf fakeShape)


-- KFFs extensions

-- | Split the innermost dimension of a push vector in two
expandS :: forall vec sh a . (Pushy vec, VecShape vec ~ (sh :. Data Length)) =>
           Data Length -> vec a ->
           Push (sh :. Data Length :. Data Length) a
expandS n v = Push g $ insLeft n $ insLeft p ext'
  where (Push f ext) = toPush v
        (m, ext') = peelLeft ext
        p = m `div` n
        g :: PushK (sh :. Data Length :. Data Length) a
        g k = f $ \ix v -> let (i, ix') = peelLeft ix
                           in k (insLeft (i `div` p) $ insLeft (i `Feldspar.mod` p) ix') v

contractS :: forall vec sh a . (Pushy vec, VecShape vec ~ (sh :. Data Length :. Data Length)) =>
             vec a ->
             Push (sh :. Data Length) a
contractS v = Push g $ insLeft (m*n) ext'
  where (Push f ext) = toPush v
        (m, n, ext') = peelLeft2 ext
        g :: PushK (sh :. Data Length) a
        g k = f $ \ix v -> let (i, j, ix') = peelLeft2 ix
                           in k (insLeft (i*n + j) ix') v

transS :: forall vec sh a . (Pushy vec, VecShape vec ~ (sh :. Data Length :. Data Length)) =>
          vec a ->
          Push (sh :. Data Length :. Data Length) a
transS v = Push g $ insLeft n $ insLeft m ext'
  where (Push f ext) = toPush v
        (m, n, ext') = peelLeft2 ext
        g :: PushK (sh :. Data Length :. Data Length) a
        g k = f $ \ix v -> let (i, j, ix') = peelLeft2 ix
                           in k (insLeft j $ insLeft i ix') v

-- | Instantiate the innermost dimension from an abstracted push vector
uncurryS :: forall sh a . Data Length -> (Data Length -> Push sh a) -> Push (sh :. Data Length) a
uncurryS m f = Push g (insLeft m ext)
  where Push _ ext = f (undefined :: Data Length)
        g :: PushK (sh :. Data Length) a
        g k = parFor m $ \i -> let Push h _ = f i
                               in h (\ix v -> k (insLeft i ix) v)

uncurryS' :: forall sh a . Data Length -> (Data Length -> Push sh a) -> Push (sh :. Data Length) a
uncurryS' m f = Push g (ext :. m)
  where Push _ ext = f (undefined :: Data Length)
        g :: PushK (sh :. Data Length) a
        g k = parFor m $ \ i -> let Push h _ = f i in h (\ ix v -> k (ix :. i) v)

expandST :: (Pushy vec, VecShape vec ~ (sh :. Data Length)) =>
            Data Length -> vec a ->
            Push (sh :. Data Length :. Data Length) a
expandST n a = transS $ expandS n a

contractST :: (Pushy vec, VecShape vec ~ (sh :. Data Length :. Data Length)) =>
              vec a ->
              Push (sh :. Data Length) a
contractST a = contractS $ transS a

-- | Manifest vectors live in memory. Pull- and Push vectors can be allocated
--   as Manifest using the 'store' function.
data Manifest sh a = Syntax a => Manifest (Data [Internal a]) (Shape sh)

-- | A class for memory allocation. All vectors are instances of this class.
class Shaped vec => Storable vec where
  -- | Allocates a vector to memory.
  store :: Syntax a => vec a -> Manifest (VecShape vec) a

instance Storable (Manifest sh) where
  store m = m

instance Shapely sh => Storable (Pull sh) where
  store vec@(Pull ixf sh) = Manifest (save $ fromPull (fmap F.desugar vec)) sh

instance Storable (Push sh) where
  store vec@(Push f sh) = Manifest (save $ fromPush (fmap F.desugar vec)) sh

instance (Syntax a, Shapely sh, ShapeTup sh, SyntacticTup (ShapeTupT sh))
      => Syntactic (Manifest sh a) where
  type Internal (Manifest sh a) = InternalShape sh a
  desugar v@(Manifest _ sh) = case sh of
      Z           -> desugar $ fromZero v
      Z :. _      -> desugar $ manifestToArr1 v
      _ :. _ :. _ -> desugar $ manifestToArr v

  sugar v = case fakeShape :: Shape sh of
      Z           -> store $ singleton $ sugar v
      Z :. _      -> arrToManifest1 $ sugar v
      _ :. _ :. _ -> arrToManifest $ sugar v

manifestToArr1 :: Syntax a => Manifest DIM1 a -> Data [Internal a]
manifestToArr1 (Manifest arr _) = arr

manifestToArr :: (Syntax a, ShapeTup sh)
              => Manifest sh a -> (Data [Internal a], Tuple (ShapeTupT sh))
manifestToArr (Manifest arr sh) = (arr, toTup sh)

arrToManifest1 :: Syntax a => Data [Internal a] -> Manifest DIM1 a
arrToManifest1 arr = Manifest arr (Z:.getLength arr)

arrToManifest :: (Syntax a, Shapely sh, ShapeTup sh)
              => (Data [Internal a], Tuple (ShapeTupT sh)) -> Manifest sh a
arrToManifest (arr, ls) = Manifest arr (fromTup ls)

-- | A typeclass for types of array elements which can be flattened. An example
--   is an array of pairs, which can be flattened into a pair of arrays.
class Flat sh a where
  type FlatManifest sh a
  aos2soa :: Push sh a -> FlatManifest sh a

instance Type a => Flat sh (Data a) where
  type FlatManifest sh (Data a) = Manifest sh (Data a)
  aos2soa vec = Manifest (fromPush vec) (extent vec)

instance (Flat sh a, Flat sh b) => Flat sh (a,b) where
  type FlatManifest sh (a,b) = (FlatManifest sh a, FlatManifest sh b)
  aos2soa vec = (aos2soa $ fmap fst vec, aos2soa $ fmap snd vec)

instance (Flat sh a, Flat sh b, Flat sh c) => Flat sh (a,b,c) where
  type FlatManifest sh (a,b,c) = (FlatManifest sh a, FlatManifest sh b, FlatManifest sh c)
  aos2soa vec = (aos2soa $ fmap (\(a,_,_) -> a) vec,
                 aos2soa $ fmap (\(_,b,_) -> b) vec,
                 aos2soa $ fmap (\(_,_,c) -> c) vec)

-- | Stores a vector into one or several flattened manifest vectors. The
--   elements of the vector have to be instances of the 'Flat' type class.
--   For example, a vector of type @Pull sh (Data Int,Data Float)@ will be
--   stored as @(Manifest sh (Data Int), Manifest sh (Data Float))@.
storeFlat :: forall a vec sh.
            (Flat sh a, Pushy vec, VecShape vec ~ sh
            ,Syntax (FlatManifest sh a)) =>
             vec a -> FlatManifest sh a
storeFlat vec = aos2soa $ toPush vec

-- | This class captures all vectors which can be turned into a 'Pull' vector
--   without allocating memory.
class Shaped vec => Pully vec where
  toPull :: vec a -> Pull (VecShape vec) a

instance Shapely sh => Pully (Manifest sh) where
  toPull (Manifest arr sh) = Pull (\i -> F.sugar $ arr ! toIndex sh i) sh

instance Pully (Pull sh) where
  toPull vec = vec

instance Shapely sh => Pushy (Manifest sh) where
  toPush m = toPush (toPull m)

type family VecShape (vec :: * -> *) :: *
type instance VecShape (Pull sh) = sh
type instance VecShape (Push sh) = sh
type instance VecShape (Manifest sh) = sh

-- | A single method class for getting the shape of a vector
class Shaped vec where
  extent :: vec a -> Shape (VecShape vec)

instance Shaped (Pull sh) where
  extent (Pull _ sh) = sh

instance Shaped (Push sh) where
  extent (Push _ sh) = sh

instance Shaped (Manifest sh) where
  extent (Manifest _ sh) = sh

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

zeros sh = constant sh 0
ones  sh = constant sh 1

constant :: Shape sh -> t -> Pull sh t
constant sh c = Pull (const c) sh

-- | Find all the indexes which are True in the input vector. The
--   result is a vector containing all the indexes which are true
--   and the number of true elements that were found.
find :: (Pully vec, VecShape vec ~ DIM1) =>
        vec (Data Bool) -> M (Manifest DIM1 (Data Index),Data Length)
find vec = do arr <- newArr l 0
              ref <- newRef 0
              forM l $ \i ->
                whenM (vec !! i) $ do
                  j <- getRef ref
                  setArr arr j i
                  setRef ref (j+1)
              len <- getRef ref
              parr <- freezeArray arr -- ideally we should trim the array here
              return (arrToManifest1 parr,len)
  where l = length vec

-------------------------------------------------------------------------------
 --- Misc.
-------------------------------------------------------------------------------

tVec :: Patch a a -> Patch (Pull sh a) (Pull sh a)
tVec _ = id
{-# DEPRECATED tVec "Use tPull instead" #-}

tVec1 :: Patch a a -> Patch (Pull1 a) (Pull1 a)
tVec1 _ = id
{-# DEPRECATED tVec1 "Use tPull1 instead" #-}

tPull :: Patch a a -> Patch (Pull sh a) (Pull sh a)
tPull _ = id

tPull1 :: Patch a a -> Patch (Pull1 a) (Pull1 a)
tPull1 _ = id
