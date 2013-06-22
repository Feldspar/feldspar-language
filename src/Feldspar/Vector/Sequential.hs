{-# LANGUAGE GADTs #-}
module Feldspar.Vector.Sequential where

import qualified Prelude
import Feldspar

data Seq a where
  Seq :: Syntax state => Data Length -> M state -> (state -> M a) -> Seq a

length :: Seq a -> Data Length
length (Seq l _ _) = l

toArray :: Type a => Seq (Data a) -> Data [a]
toArray (Seq l init next) = runMutableArray $
                            do state <- init
                               arr   <- newArr_ l
                               forM l (\i -> do
                                             a <- next state
                                             setArr arr i a
                                      )
                               return arr

instance Functor Seq where
  fmap f (Seq l i next) = Seq l i n
    where
      n s = do a <- next s
               return (f a)

tail :: Syntax a => Seq a -> Seq a
tail (Seq l i next) = Seq (max 0 (l-1)) i' next
  where i' = do s <- i
                next s
                return s

seq :: Syntax s => Data Length -> s -> (Data Index -> s -> (a,s)) -> Seq a
seq l i next = Seq l init step
  where
    init = do rc <- newRef (0 :: Data WordN)
              rs <- newRef i
              return (rs,rc)
    step (rs,rc) = do c <- getRef rc
                      s <- getRef rs
                      let (a,s') = next c s
                      setRef rs s'
                      setRef rc (c+1)
                      return a

zipWith :: (a -> b -> c) -> Seq a -> Seq b -> Seq c
zipWith f (Seq l1 i1 n1) (Seq l2 i2 n2) = Seq (min l1 l2) i next
  where
    i = do s1 <- i1
           s2 <- i2
           return (s1,s2)
    next (s1,s2) = do a <- n1 s1
                      b <- n2 s2
                      return (f a b)

interleave :: Syntax a => Seq a -> Seq a -> Seq a
interleave (Seq l1 i1 n1) (Seq l2 i2 n2)
  = Seq (2*(min l1 l2)) i next
  where
    i = do s1 <- i1
           s2 <- i2
           b  <- newRef true
           return (b,s1,s2)
    next (rb,s1,s2) = do b <- getRef rb
                         setRef rb (not b)
                         ifM b
                           (n1 s1)
                           (n2 s2)
-- This function is slower than need be, because we can only write one
-- element at a time. If we had something akin to a push array then we
-- wouldn't have to have a boolean to switch between the two different
-- step functions, we could just execute them one after the other.
