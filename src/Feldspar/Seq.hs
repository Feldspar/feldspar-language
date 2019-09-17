{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies  #-}
module Feldspar.Seq where

import qualified Prelude as P

import Feldspar
import Feldspar.Vector hiding (singleton,zipWith)
import Feldspar.Mutable
import Data.Hash

data Seq a = Seq (M (Data Index -> M a)) (Data Length)

type instance VecShape Seq = Z :. Data Length

instance Pushy Seq where
  toPush (Seq init n) = Push f (Z :. n)
    where f wf = do step <- init
                    forM n $ \i -> do
                      a <- step i
                      wf (Z :. i) a

instance Storable Seq where
  store = store . toPush

instance Shaped Seq where
  extent (Seq _ n) = Z :. n

class Seqy vec where
  toSeq :: vec a -> Seq a

instance Seqy (Manifest sh) where
  toSeq = manifestToSeq

instance Seqy (Pull sh) where
  toSeq = pullToSeq

instance Seqy Seq where
  toSeq = id

loop = return

toPushS :: Shape sh -> Seq a -> Push sh a
toPushS sh (Seq init n) = Push f sh
  where f wf = do step <- init
                  forShape sh $ \shi ->
                    do a <- step (toIndex sh shi)
                       wf shi a

pullToSeq :: Pull sh a -> Seq a
pullToSeq (Pull ixf sh) = Seq init (size sh)
  where init = loop $ \i -> do
                 return (ixf (fromIndex sh i))

manifestToSeq :: Manifest sh a -> Seq a
manifestToSeq (Manifest arr sh) = Seq init len
  where len = size sh
        init = loop $ \i -> do
                 return (sugar (arr ! i))

zipWith :: (a -> b -> c) -> Seq a -> Seq b -> Seq c
zipWith f (Seq initA lA) (Seq initB lB) = Seq init (min lA lB)
  where init = do stepA <- initA
                  stepB <- initB
                  loop $ \i -> do
                    a <- stepA i
                    b <- stepB i
                    return (f a b)

instance Functor Seq where
  fmap f (Seq init l) = Seq init' l
    where init' = do step <- init
                     loop $ \i -> do
                       a <- step i
                       return (f a)
    
take :: Data WordN -> Seq a -> Seq a
take n (Seq init _) = Seq init n

replicate :: Data WordN -> a -> Seq a
replicate l a = Seq (loop $ \_ -> return a) l

singleton :: a -> Seq a
singleton a = Seq (loop $ \_ -> return a) 1

instance Num a => Num (Seq a) where
  (+) = zipWith (+)
  (-) = zipWith (-)
  (*) = zipWith (*)
  negate = fmap negate
  abs    = fmap abs
  signum = fmap signum
  fromInteger = singleton . fromInteger

-- | 'scan f a str' produces a sequential vector by successively applying 'f' to
--   each element of the input 'str' and the previous element of
--   the output.
scan :: Syntax a => (a -> b -> a) -> a -> Seq b -> Seq a
scan f a (Seq init l) = Seq init' (l+1)
  where init' = do next <- init
                   r <- newRef a
                   loop $ \i -> do
                     x <- next i
                     acc <- getRef r
                     setRef r (f acc x)
                     return acc

-- | A scan but without an initial element.
scan1 :: Syntax a => (a -> a -> a) -> Seq a -> Seq a
scan1 f (Seq init l) = Seq init' l
  where init' = do next <- init
                   a    <- next 0
                   r    <- newRef a
                   loop $ \i -> do
                     a <- getRef r
                     b <- next (i + 1)
                     let c = f a b
                     setRef r c
                     return c

recurrenceI :: (Type a, Type b, Hashable b) =>
               Pull1 a -> Seq (Data a) ->
               (Pull1 a -> Data b) ->
               Seq (Data b)
recurrenceI ii seq mkExpr
    = recurrenceIO ii seq (toPull $ value1 []) (\i _ -> mkExpr i)

recurrenceIO :: (Type a, Type b, Hashable b) =>
                Pull1 a -> Seq (Data a) -> Pull1 b ->
                (Pull1 a -> Pull1 b -> Data b) ->
                Seq (Data b)
recurrenceIO ii (Seq init l) io mkExpr = Seq init' (l + length ii)
  where init' = do next <- init
                   ibuf <- initBuffer2 ii
                   obuf <- initBuffer2 io
                   loop $ \i -> do
                     a <- next i
                     whenM (lenI /= 0) $ putBuf ibuf a
                     b <- withBuf ibuf $ \ib ->
                            withBuf obuf $ \ob ->
                              return $ mkExpr ib ob
                     whenM (lenO /= 0) $ putBuf obuf b
                     return b
                       where
                         lenI = length ii
                         lenO = length io

fir :: (Numeric a, Hashable a) => Pull1 a ->
       Seq (Data a) -> Seq (Data a)
fir b inp = recurrenceI (replicate1 (length b) 0) inp (\i -> scalarProd b i)

iir :: (Fraction a, Hashable a) => Data a -> Pull1 a -> Pull1 a ->
       Seq (Data a) -> Seq (Data a)
iir a0 a b inp =
    recurrenceIO (replicate1 (length b) 0) inp
                 (replicate1 (length a) 0)
      (\i o -> 1 / a0 * ( scalarProd b i
                        - scalarProd a o)
      )
