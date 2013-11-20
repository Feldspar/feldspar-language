{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE GADTs                  #-}
module Feldspar.Vector.PushMultiDim where

import Feldspar hiding (desugar,sugar,resugar)
import qualified Feldspar.Vector.MultiDim as R
import Feldspar.Vector.Shape
import Language.Syntactic hiding (size)

import Prelude hiding (div)
import qualified Prelude as P

-- import Feldspar.Compiler

import Control.Monad (zipWithM_)

import Feldspar.Stream hiding (map)

-- | Multidimentional push vectors
data Vector sh a = Push ((Shape sh -> a -> M ()) -> M ()) (Shape sh)

shape :: Vector sh a -> Shape sh
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
(+=+) :: R.Pull (sh :. Data Length) a
      -> R.Pull (sh :. Data Length) a
      -> Vector (sh :. Data Length) a
(R.Pull (sh1 :. l1) ixf1) +=+ (R.Pull (sh2 :. l2) ixf2)
	  = Push f (sh1 :. (l1 + l2))
  where f k = forShape (sh1 :. l1) $ \ (shi :. i) ->
                do k (shi :. i)      (ixf1 (shi :. i))
                   k (shi :. i + l1) (ixf2 (shi :. i))

-- | Flattens an array of pairs such that the elements of a pair end up next
--   to each other in the resulting array.
unpair :: Pushy arr
       => arr (sh :. Data Length) (a,a)
       -> Vector (sh :. Data Length) a
unpair v = Push k' (sh :. (l * 2))
  where (Push k ex) = toPush v
        (sh,l) = case ex of sh :. l -> (sh,l)
        k' func = k (\ (sh :. i) (a,b) -> func (sh :. (2 * i)) a
                                       >> func (sh :. (2 * i + 1)) b)

-- | Transform the indices of a vector.
ixmap :: (Shape sh -> Shape sh) -> Vector sh a -> Vector sh a
ixmap ixf (Push k l) = Push k' l
  where k' func = k (\sh a -> func (ixf sh) a)

-- | Reverse a vector along its last dimension
rev1 ::  Vector (sh :. Data Length) a -> Vector (sh :. Data Length) a
rev1 (Push f ex@(sh :. l)) = Push f' ex
  where f' k = f (\(shi :. ix) a -> k (shi :. (l - ix - 1)) a)

-- | Transpose a two dimensional vector.
transpose :: Vector DIM2 a -> Vector DIM2 a
transpose = ixmap (\ (Z :. x :. y) -> (Z :. y :. x))

-- Some helper functions in Repa to help us define riffle

halve :: R.Pull (sh :. Data Length) a
      -> (R.Pull (sh :. Data Length) a, R.Pull (sh :. Data Length) a)
halve (R.Pull (sh :. l) ixf) = (R.Pull (sh :. (l `div` 2)) ixf
      		    	       	 ,R.Pull (sh :. ((l+1) `div` 2)) ixf')
  where ixf' (sh :. i) = ixf (sh :. (i + (l `div` 2)))


riffle :: R.Pull (sh :. Data Length) a -> Vector (sh :. Data Length) a
riffle =  unpair . uncurry R.zip . halve

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
         Select sel -> Vector sh a -> Vector sh a -> Vector sh a
conc s (Push k1 sh1) (Push k2 sh2)
     = Push k (adjustDimension s (+ selectDimension s sh2) sh1)
  where k func = k1 func
  	       	 >>
		 k2 (\ sh a -> func (adjustDimension s (+ selectDimension s sh1) sh) a)
-- Assumption sh1 == sh2

-- | Reverse a vector along a particular dimension.
rev :: Selector sel sh =>
       Select sel -> Vector sh a -> Vector sh a
rev s (Push k sh) = Push k' sh
  where k' func = k (\sh a -> func (adjustDimension s (selectDimension s sh -) sh) a)

-- | Both pull vectors and push vectors can be cheaply converted to push vectors
class Pushy arr where
  toPush :: arr sh a -> Vector sh a

instance Pushy Vector where
  toPush = id

instance Pushy R.Pull where
  toPush (R.Pull l ixf) = Push f l
    where f k = forShape l (\i ->
    	    	  k i (ixf i)
	        )

-- | Store a vector in memory as a flat array
fromVector :: Type a =>
	      Vector sh (Data a) -> Data [a]
fromVector (Push ixf l) = runMutableArray $
	   	   	  do marr <- newArr_ (size l)
			     ixf (\ix a -> setArr marr (toIndex l ix) a)
			     return marr

freezeVector :: Type a => Vector sh (Data a) -> (Data [Length], Data [a])
freezeVector v   = (shapeArr, fromVector v)
  where shapeArr = fromList (toList $ shape v)

fromList :: Type a => [Data a] -> Data [a]
fromList ls = loop 1 (parallel (value len) (const (P.head ls)))
  where loop i arr
            | i P.< len = loop (i+1) (setIx arr (value i) (ls P.!! (P.fromIntegral i)))
            | otherwise = arr
        len  = P.fromIntegral $ P.length ls

thawVector :: (Type a, Shapely sh) =>
              (Data [Length], Data [a]) -> Vector sh (Data a)
thawVector (l,arr) = toVector (toShape 0 l) arr

toVector :: Type a => Shape sh -> Data [a] -> Vector sh (Data a)
toVector sh arr = Push f sh
  where f k = forShape sh $ \i ->
                k i (arr ! (toIndex sh i))

instance (Syntax a, Shapely sh) => Syntactic (Vector sh a)
  where
    type Domain (Vector sh a) = FeldDomain
    type Internal (Vector sh a) = ([Length],[Internal a])
    desugar = desugar . freezeVector . fmap resugar
    sugar   = fmap resugar . thawVector . sugar

-- | Flatten a pull vector of lists so that the lists become an extra dimension
flattenList :: Shapely sh => R.Pull sh [a] -> Vector (sh :. Data Length) a
flattenList (R.Pull sh ixf) = Push f sz
  where f k = forShape sh $ \i ->
  	      	do let indices = map (\j -> i :. j) $
				 map value [0..l-1]
    	           zipWithM_ k indices (ixf i)
        sz  = sh :. value l
        l   = P.fromIntegral $
	      P.length (ixf fakeShape)


-- KFFs extensions

expandS :: Data Length -> Vector (sh :. Data Length) a -> Vector (sh :. Data Length :. Data Length) a
expandS n (Push k ext) = Push k' $ insLeft n $ insLeft p $ ext' 
  where (m, ext') = peelLeft ext
        p = m `div` n
        k' wtf = k $ \ ix v -> let (i,ix') = peelLeft ix in wtf (insLeft (i `div` p) $ insLeft (i `Feldspar.mod` p) $ ix') v

contractS :: Vector (sh :. Data Length :. Data Length) a -> Vector (sh :. Data Length) a
contractS (Push k ext) = Push k' $ insLeft (m*n) $ ext'
  where (m, n, ext') = peelLeft2 ext
        k' wtf = k $ \ ix v -> let (i, j, ix') = peelLeft2 ix in wtf (insLeft (i*n + j) ix') v

transS :: Vector (sh :. Data Length :. Data Length) a -> Vector (sh :. Data Length :. Data Length) a
transS (Push k ext) = Push k' $ insLeft n $ insLeft m $ ext'
  where (m, n, ext') = peelLeft2 ext
        k' wtf = k $ \ ix v -> let (i, j, ix') = peelLeft2 ix in wtf (insLeft j $ insLeft i $ ix') v

uncurryS :: Data Length -> (Data Length -> Vector sh a) -> Vector (sh :. Data Length) a
uncurryS m f = Push k' (insLeft m ext)
  where Push _ ext = f (undefined :: Data Length)
        k' wtf = forM m $ \ i -> let Push k _ = f i in k (\ ix v -> wtf (insLeft i ix) v)

-- Convenience functions that maybe should not be in the lib

expandST :: Data Length -> Vector (sh :. Data Length) a -> Vector (sh :. Data Length :. Data Length) a
expandST n a = transS $ expandS n $ a

contractST :: Vector (sh :. Data Length :. Data Length) a -> Vector (sh :. Data Length) a
contractST a = contractS $ transS $ a



