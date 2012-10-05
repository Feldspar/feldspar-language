{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

--
-- Copyright (c) 2009-2011, ERICSSON AB
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
-- 
--     * Redistributions of source code must retain the above copyright notice, 
--       this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in the
--       documentation and/or other materials provided with the distribution.
--     * Neither the name of the ERICSSON AB nor the names of its contributors
--       may be used to endorse or promote products derived from this software
--       without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

module Feldspar.Vector.Push where

import qualified Prelude

import Feldspar hiding (sugar,desugar)
import qualified Feldspar.Vector as V

import Language.Syntactic (Syntactic(..))

data PushVector a where
  Push :: ((Data Index -> a -> M ()) -> M ()) -> Data Length -> PushVector a

instance Syntax a => Syntactic (PushVector a) FeldDomainAll
  where
    type Internal (PushVector a) = [Internal a]
    desugar = desugar . freezePush
    sugar   = thawPush . sugar

-- | Store push vectors in memory.
freezePush :: Syntax a => PushVector a -> Data [Internal a]
freezePush (Push k l) = runMutableArray $ do
                          arr <- newArr_ l
                          k (\i a -> setArr arr i (resugar a))
                          return arr

-- | Store a push vector to memory and return it as an ordinary vector.
freezeToVector :: Syntax a => PushVector a -> V.Vector a
freezeToVector = V.map resugar . V.thawVector . freezePush

-- | Create a push vector from an array stored in memory.
thawPush :: Syntax a => Data [Internal a] -> PushVector a
thawPush arr = Push f (getLength arr)
  where f k = forM (getLength arr) $ \ix ->
                k ix (resugar (arr ! ix))

instance Syntax a => Syntax (PushVector a)

-- | Any kind of vector, push or pull, can cheaply be converted to a push vector
class Pushy arr where
  toPush :: Syntax a => arr a -> PushVector a

instance Pushy PushVector where
  toPush = id

instance Pushy V.Vector where
  toPush vec = Push (\k -> forM (length vec) (\i -> k i (vec!i))) (length vec)

instance Functor PushVector where
  fmap f (Push g l) = Push (\k -> g (\i a -> k i (f a))) l

-- | Concatenating two arrays.
(++) :: (Pushy arr, Syntax a) => arr a -> arr a -> PushVector a
v1 ++ v2 = Push (\func -> f func >>
                          g (\i a -> func (l1 + i) a))
                (l1 + l2)
  where
    Push f l1 = toPush v1
    Push g l2 = toPush v2

-- | Given an array of pairs, flatten the array so that the elements of the
--   pairs end up next to each other in the resulting vector.
unpair :: (Pushy arr, Syntax a) => arr (a,a) -> PushVector a
unpair arr = Push (\k -> f (everyOther k)) (2 * l)
  where
    Push f l = toPush arr

everyOther :: (Data Index -> a -> M b)
           -> Data Index -> (a,a) -> M b
everyOther f = \ix (a1,a2) -> f (ix * 2) a1 >> f (ix * 2 + 1) a2

-- | Interleaves the elements of two vectors.
zipUnpair :: Syntax a => V.Vector a -> V.Vector a -> PushVector a
zipUnpair v1 v2 = unpair (V.zip v1 v2)

-- | An overloaded function for reordering elements of a vector.
class Ixmap arr where
  ixmap :: Syntax a => (Data Index -> Data Index) -> arr a -> arr a

instance Ixmap V.Vector where
  ixmap f vec = V.indexed (length vec) (\i -> vec ! (f i))

instance Ixmap PushVector where
  ixmap f (Push g l) = Push (\k -> g (\i a -> k (f i) a)) l

-- | Reverse a vector. Works for both push and pull vectors.
reverse :: (Ixmap arr, Len arr, Syntax a) =>
           arr a -> arr a
reverse arr = ixmap (\ix -> length arr - ix - 1) arr

-- | Split a pull vector in half.
--
--   If the input vector has an odd length the second result vector
--   will be one element longer than the first.
halve :: Syntax a => V.Vector a -> (V.Vector a, V.Vector a)
halve v = (V.indexed (l `div` 2) ixf
       	  ,V.indexed ((l+1) `div` 2) (\i -> ixf (i + (l `div` 2))))
  where l   = length v
  	ixf = (v!)

-- | Split a vector in half and interleave the two two halves.
riffle :: Syntax a => V.Vector a -> PushVector a
riffle = unpair . uncurry V.zip . halve

-- | A class for overloading `length` for both pull and push vectors
class Len arr where
  length :: arr a -> Data Length

instance Len V.Vector where
  length = V.length

instance Len PushVector where
  length (Push _ l) = l

-- | This function can distribute array computations on chunks of a large
--   pull vector. A call `chunk l f g v` will split the vector `v` into chunks
--   of size `l` and apply `f` to these chunks. In case the length of `v` is
--   not a multiple of `l` then the rest of `v` will be processed by `g`.
chunk :: (Pushy arr1, Pushy arr2, Syntax b)
      => Data Length            -- ^ Size of the chunks
      -> (V.Vector a -> arr1 b) -- ^ Applied to every chunk
      -> (V.Vector a -> arr2 b) -- ^ Applied to the rest of the vector
      -> V.Vector a
      -> PushVector b
chunk c f g v = Push loop (noc * c)
             ++ toPush (g (V.drop (noc * c) v))
  where l = length v
        noc = l `div` c
        loop func = forM noc $ \i ->
                      do let (Push k _) = toPush $ f (V.take c (V.drop (c*i) v))
                         k (\j a -> func (c*i + j) a)

-- | The empty push vector.
empty :: PushVector a
empty = Push (const (return ())) 0

-- | Flattens a pull vector containing push vectors into an unnested push vector
--
--   Note that there are no restrictions on the lengths of the push vectors
--   inside the pull vector.
flatten :: Syntax a => V.Vector (PushVector a) -> PushVector a
flatten v = Push f len
  where len = V.sum (V.map length v)
  	f k = do l <- newRef 0
	      	 forM (length v) $ \i ->
		   do let (Push g m) = v ! i
		      n <- getRef l
		      g (\j a -> k (n + j) a)
		      setRef l (n+m)
