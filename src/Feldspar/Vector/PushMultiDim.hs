{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module Feldspar.Vector.PushMultiDim where

import Feldspar hiding (desugar,sugar,resugar)
import qualified Feldspar.Vector.MultiDim as R
import Feldspar.Vector.Shape
import Language.Syntactic.Syntax

import Prelude hiding (div)
import qualified Prelude as P

-- import Feldspar.Compiler

import Control.Monad (zipWithM_)

import Feldspar.Stream hiding (map)

-- | Multidimentional push vectors
data Vector sh a = Push ((sh -> a -> M ()) -> M ()) sh

shape :: Vector sh a -> sh
shape (Push _ sh) = sh

instance Functor (Vector sh) where
  fmap f (Push k l) = Push k' l
    where k' func   = k (\sh a -> func sh (f a))

-- | Concatenation along the the last dimension
(++) :: Vector (sh :. Data Length) a
     -> Vector (sh :. Data Length) a
     -> Vector (sh :. Data Length) a
(Push k1 (sh1 :. l1)) ++ (Push k2 (sh2 :. l2)) = Push k (sh1 :. (l1 + l2))
  where k func = k1 func
  	       	 >>
		 k2 (\ (sh :. i) a -> func (sh :. (i + l1)) a)
-- Assumption sh1 == sh2

-- | Concatenation along the last dimension where the two vectors have the same
--   length. There is no check that the lengths are equal.
(+=+) :: Shape sh
      => R.Vector (sh :. Data Length) a
      -> R.Vector (sh :. Data Length) a
      -> Vector (sh :. Data Length) a
(R.Vector (sh1 :. l1) ixf1) +=+ (R.Vector (sh2 :. l2) ixf2)
	  = Push f (sh1 :. (l1 + l2))
  where f k = forShape (sh1 :. l1) $ \ (shi :. i) ->
                do k (shi :. i)      (ixf1 (shi :. i))
                   k (shi :. i + l1) (ixf2 (shi :. i))

-- | Flattens an array of pairs such that the elements of a pair end up next
--   to each other in the resulting array.
unpair :: (Shape sh, Pushy arr)
       => arr (sh :. Data Length) (a,a)
       -> Vector (sh :. Data Length) a
unpair v = Push k' (sh :. (l * 2))
  where (Push k (sh :. l)) = toPush v
        k' func = k (\ (sh :. i) (a,b) -> func (sh :. (2 * i)) a
                                       >> func (sh :. (2 * i + 1)) b)

-- | Transform the indices of a vector.
ixmap :: (sh -> sh) -> Vector sh a -> Vector sh a
ixmap ixf (Push k l) = Push k' l
  where k' func = k (\sh a -> func (ixf sh) a)

-- | Reverse a vector along its last dimension
rev1 ::  Vector (sh :. Data Length) a -> Vector (sh :. Data Length) a
rev1 v = ixmap (\ (sh :. l1) -> sh :. (l - l1 - 1)) v
  where (_ :. l) = shape v

-- | Transpose a two dimensional vector.
transpose :: Vector DIM2 a -> Vector DIM2 a
transpose = ixmap (\ (Z :. x :. y) -> (Z :. y :. x))

-- Some helper functions in Repa to help us define riffle

halve :: R.Vector (sh :. Data Length) a
      -> (R.Vector (sh :. Data Length) a, R.Vector (sh :. Data Length) a)
halve (R.Vector (sh :. l) ixf) = (R.Vector (sh :. (l `div` 2)) ixf
      		    	       	 ,R.Vector (sh :. ((l+1) `div` 2)) ixf')
  where ixf' (sh :. i) = ixf (sh :. (i + (l `div` 2)))


riffle :: Shape sh =>
          R.Vector (sh :. Data Length) a -> Vector (sh :. Data Length) a
riffle =  unpair . uncurry R.zip . halve

-- Pinpointing one particular dimension

data NotThis = NotThis
data This = This

-- | In many functions it is desirable to perform an operation along one
--   particular dimension, such as concatenating two vectors along a particular
--   dimension or reversing a vector along another dimension. The `Selector`
--   typeclass enables selecting a specific dimension in a shape.
class Selector a sh | a -> sh where
  selectLength :: a -> sh -> Data Length
  adjustLength :: a -> (Data Length -> Data Length) -> sh -> sh

instance Selector a sh => Selector (a :. NotThis) (sh :. Data Length) where
  selectLength (s :. _) (sh :. _) = selectLength s sh
  adjustLength (s :. _) f (sh :. l) = sh' :. l
   where sh' = adjustLength s f sh

instance Selector This (sh :. Data Length) where
  selectLength This (_ :. l) = l
  adjustLength This f (sh :. l) = sh :. f l

-- | Concatenating vectors along a particular dimension
conc :: Selector sel sh =>
         sel -> Vector sh a -> Vector sh a -> Vector sh a
conc s (Push k1 sh1) (Push k2 sh2)
     = Push k (adjustLength s (+ selectLength s sh2) sh1)
  where k func = k1 func
  	       	 >>
		 k2 (\ sh a -> func (adjustLength s (+ selectLength s sh1) sh) a)
-- Assumption sh1 == sh2

-- | Reverse a vector along a particular dimension.
rev :: Selector sel sh =>
       sel -> Vector sh a -> Vector sh a
rev s (Push k sh) = Push k' sh
  where k' func = k (\ sh a -> func (adjustLength s (selectLength s sh -) sh) a)

-- | Both pull vectors and push vectors can be cheaply converted to push vectors
class Pushy arr where
  toPush :: Shape sh => arr sh a -> Vector sh a

instance Pushy Vector where
  toPush = id

instance Pushy R.Vector where
  toPush (R.Vector l ixf) = Push f l
    where f k = forShape l (\i ->
    	    	  k i (ixf i)
	        )

-- | Store a vector in memory as a flat array
fromVector :: (Type a, Shape sh) =>
	      Vector sh (Data a) -> Data [a]
fromVector (Push ixf l) = runMutableArray $
	   	   	  do marr <- newArr_ (size l)
			     ixf (\ix a -> setArr marr (toIndex l ix) a)
			     return marr

freezeVector :: (Shape sh, Type a) => Vector sh (Data a) -> (Data [Length], Data [a])
freezeVector v   = (shapeArr, fromVector v)
  where shapeArr = fromList (toList $ shape v)

fromList :: Type a => [Data a] -> Data [a]
fromList ls = loop 1 (parallel (value len) (const (P.head ls)))
  where loop i arr
            | i P.< len = loop (i+1) (setIx arr (value i) (ls P.!! (P.fromIntegral i)))
            | otherwise = arr
        len  = P.fromIntegral $ P.length ls

thawVector :: (Shape sh, Type a) => (Data [Length], Data [a]) -> Vector sh (Data a)
thawVector (l,arr) = toVector (toShape 0 l) arr

toVector :: (Type a, Shape sh) => sh -> Data [a] -> Vector sh (Data a)
toVector sh arr = Push f sh
  where f k = forShape sh $ \i ->
                k i (arr ! (toIndex sh i))

instance  (Shape sh, Syntax a) => Syntactic (Vector sh a) FeldDomainAll
  where
    type Internal (Vector sh a) = ([Length],[Internal a])
    desugar = desugar . freezeVector . fmap resugar
    sugar   = fmap resugar . thawVector . sugar

-- | Flatten a pull vector of lists so that the lists become an extra dimension
flattenList :: Shape sh =>
	   R.Vector sh [a] -> Vector (sh :. Data Length) a
flattenList (R.Vector sh ixf) = Push f sz
  where f k = forShape sh $ \i ->
  	      	do let indices = map (\j -> i :. j) $
				 map value [0..l-1]
    	           zipWithM_ k indices (ixf i)
        sz  = sh :. value l
        l   = P.fromIntegral $
	      P.length (ixf fakeShape)
