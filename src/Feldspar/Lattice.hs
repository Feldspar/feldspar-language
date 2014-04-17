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

-- | General operations on sets

module Feldspar.Lattice where



import Data.Lens.Common



-- | Lattice types
class Eq a => Lattice a
  where
    bot       :: a
    top       :: a
    -- | Join
    (\/)      :: a -> a -> a
    -- | Meet
    (/\)      :: a -> a -> a

empty :: Lattice a => a
empty = bot

universal :: Lattice a => a
universal = top

instance Lattice ()
  where
    bot       = ()
    top       = ()
    () \/ ()  = ()
    () /\ ()  = ()

-- | Lattice product
instance (Lattice a, Lattice b) => Lattice (a,b)
  where
    bot = (bot,bot)
    top = (top,top)
    (a1,a2) \/ (b1,b2) = (a1 \/ b1, a2 \/ b2)
    (a1,a2) /\ (b1,b2) = (a1 /\ b1, a2 /\ b2)

-- | Three-way product
instance (Lattice a, Lattice b, Lattice c) => Lattice (a,b,c)
  where
    bot = (bot,bot,bot)
    top = (top,top,top)
    (a1,a2,a3) \/ (b1,b2,b3) = (a1 \/ b1, a2 \/ b2, a3 \/ b3)
    (a1,a2,a3) /\ (b1,b2,b3) = (a1 /\ b1, a2 /\ b2, a3 /\ b3)

-- | Four-way product
instance (Lattice a, Lattice b, Lattice c, Lattice d) => Lattice (a,b,c,d)
  where
    bot = (bot,bot,bot,bot)
    top = (top,top,top,top)
    (a1,a2,a3,a4) \/ (b1,b2,b3,b4) = (a1 \/ b1, a2 \/ b2, a3 \/ b3, a4 \/ b4)
    (a1,a2,a3,a4) /\ (b1,b2,b3,b4) = (a1 /\ b1, a2 /\ b2, a3 /\ b3, a4 /\ b4)

-- | Five-way product
instance (Lattice a, Lattice b, Lattice c, Lattice d, Lattice e) => Lattice (a,b,c,d,e)
  where
    bot = (bot,bot,bot,bot,bot)
    top = (top,top,top,top,top)
    (a1,a2,a3,a4,a5) \/ (b1,b2,b3,b4,b5) = (a1 \/ b1, a2 \/ b2, a3 \/ b3, a4 \/ b4, a5 \/ b5)
    (a1,a2,a3,a4,a5) /\ (b1,b2,b3,b4,b5) = (a1 /\ b1, a2 /\ b2, a3 /\ b3, a4 /\ b4, a5 /\ b5)

-- | Six-way product
instance (Lattice a, Lattice b, Lattice c, Lattice d, Lattice e, Lattice f) => Lattice (a,b,c,d,e,f)
  where
    bot = (bot,bot,bot,bot,bot,bot)
    top = (top,top,top,top,top,top)
    (a1,a2,a3,a4,a5,a6) \/ (b1,b2,b3,b4,b5,b6) = (a1 \/ b1, a2 \/ b2, a3 \/ b3, a4 \/ b4, a5 \/ b5, a6 \/ b6)
    (a1,a2,a3,a4,a5,a6) /\ (b1,b2,b3,b4,b5,b6) = (a1 /\ b1, a2 /\ b2, a3 /\ b3, a4 /\ b4, a5 /\ b5, a6 /\ b6)

-- | Seven-way product
instance (Lattice a, Lattice b, Lattice c, Lattice d, Lattice e, Lattice f, Lattice g) => Lattice (a,b,c,d,e,f,g)
  where
    bot = (bot,bot,bot,bot,bot,bot,bot)
    top = (top,top,top,top,top,top,top)
    (a1,a2,a3,a4,a5,a6,a7) \/ (b1,b2,b3,b4,b5,b6,b7) = (a1 \/ b1, a2 \/ b2, a3 \/ b3, a4 \/ b4, a5 \/ b5, a6 \/ b6, a7 \/ b7)
    (a1,a2,a3,a4,a5,a6,a7) /\ (b1,b2,b3,b4,b5,b6,b7) = (a1 /\ b1, a2 /\ b2, a3 /\ b3, a4 /\ b4, a5 /\ b5, a6 /\ b6, a7 /\ b7)

-- | Eight-way product
instance (Lattice a, Lattice b, Lattice c, Lattice d, Lattice e, Lattice f, Lattice g, Lattice h) => Lattice (a,b,c,d,e,f,g,h)
  where
    bot = (bot,bot,bot,bot,bot,bot,bot,bot)
    top = (top,top,top,top,top,top,top,top)
    (a1,a2,a3,a4,a5,a6,a7,a8) \/ (b1,b2,b3,b4,b5,b6,b7,b8) = (a1 \/ b1, a2 \/ b2, a3 \/ b3, a4 \/ b4, a5 \/ b5, a6 \/ b6, a7 \/ b7, a8 \/ b8)
    (a1,a2,a3,a4,a5,a6,a7,a8) /\ (b1,b2,b3,b4,b5,b6,b7,b8) = (a1 /\ b1, a2 /\ b2, a3 /\ b3, a4 /\ b4, a5 /\ b5, a6 /\ b6, a7 /\ b7, a8 /\ b8)

-- | Nine-way product
instance (Lattice a, Lattice b, Lattice c, Lattice d, Lattice e, Lattice f, Lattice g, Lattice h, Lattice i) => Lattice (a,b,c,d,e,f,g,h,i)
  where
    bot = (bot,bot,bot,bot,bot,bot,bot,bot,bot)
    top = (top,top,top,top,top,top,top,top,top)
    (a1,a2,a3,a4,a5,a6,a7,a8,a9) \/ (b1,b2,b3,b4,b5,b6,b7,b8,b9) = (a1 \/ b1, a2 \/ b2, a3 \/ b3, a4 \/ b4, a5 \/ b5, a6 \/ b6, a7 \/ b7, a8 \/ b8, a9 \/ b9)
    (a1,a2,a3,a4,a5,a6,a7,a8,a9) /\ (b1,b2,b3,b4,b5,b6,b7,b8,b9) = (a1 /\ b1, a2 /\ b2, a3 /\ b3, a4 /\ b4, a5 /\ b5, a6 /\ b6, a7 /\ b7, a8 /\ b8, a9 /\ b9)

-- | Ten-way product
instance (Lattice a, Lattice b, Lattice c, Lattice d, Lattice e, Lattice f, Lattice g, Lattice h, Lattice i, Lattice j) => Lattice (a,b,c,d,e,f,g,h,i,j)
  where
    bot = (bot,bot,bot,bot,bot,bot,bot,bot,bot,bot)
    top = (top,top,top,top,top,top,top,top,top,top)
    (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) \/ (b1,b2,b3,b4,b5,b6,b7,b8,b9,b10) = (a1 \/ b1, a2 \/ b2, a3 \/ b3, a4 \/ b4, a5 \/ b5, a6 \/ b6, a7 \/ b7, a8 \/ b8, a9 \/ b9, a10 \/ b10)
    (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) /\ (b1,b2,b3,b4,b5,b6,b7,b8,b9,b10) = (a1 /\ b1, a2 /\ b2, a3 /\ b3, a4 /\ b4, a5 /\ b5, a6 /\ b6, a7 /\ b7, a8 /\ b8, a9 /\ b9, a10 /\ b10)

-- | Eleven-way product
instance (Lattice a, Lattice b, Lattice c, Lattice d, Lattice e, Lattice f, Lattice g, Lattice h, Lattice i, Lattice j, Lattice k) => Lattice (a,b,c,d,e,f,g,h,i,j,k)
  where
    bot = (bot,bot,bot,bot,bot,bot,bot,bot,bot,bot,bot)
    top = (top,top,top,top,top,top,top,top,top,top,top)
    (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) \/ (b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11) = (a1 \/ b1, a2 \/ b2, a3 \/ b3, a4 \/ b4, a5 \/ b5, a6 \/ b6, a7 \/ b7, a8 \/ b8, a9 \/ b9, a10 \/ b10, a11 \/ b11)
    (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) /\ (b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11) = (a1 /\ b1, a2 /\ b2, a3 /\ b3, a4 /\ b4, a5 /\ b5, a6 /\ b6, a7 /\ b7, a8 /\ b8, a9 /\ b9, a10 /\ b10, a11 /\ b11)

-- | Twelve-way product
instance (Lattice a, Lattice b, Lattice c, Lattice d, Lattice e, Lattice f, Lattice g, Lattice h, Lattice i, Lattice j, Lattice k, Lattice l) => Lattice (a,b,c,d,e,f,g,h,i,j,k,l)
  where
    bot = (bot,bot,bot,bot,bot,bot,bot,bot,bot,bot,bot,bot)
    top = (top,top,top,top,top,top,top,top,top,top,top,top)
    (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12) \/ (b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12) = (a1 \/ b1, a2 \/ b2, a3 \/ b3, a4 \/ b4, a5 \/ b5, a6 \/ b6, a7 \/ b7, a8 \/ b8, a9 \/ b9, a10 \/ b10, a11 \/ b11, a12 \/ b12)
    (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12) /\ (b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12) = (a1 /\ b1, a2 /\ b2, a3 /\ b3, a4 /\ b4, a5 /\ b5, a6 /\ b6, a7 /\ b7, a8 /\ b8, a9 /\ b9, a10 /\ b10, a11 /\ b11, a12 /\ b12)

-- | Thirteen-way product
instance (Lattice a, Lattice b, Lattice c, Lattice d, Lattice e, Lattice f, Lattice g, Lattice h, Lattice i, Lattice j, Lattice k, Lattice l, Lattice m) => Lattice (a,b,c,d,e,f,g,h,i,j,k,l,m)
  where
    bot = (bot,bot,bot,bot,bot,bot,bot,bot,bot,bot,bot,bot,bot)
    top = (top,top,top,top,top,top,top,top,top,top,top,top,top)
    (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13) \/ (b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13) = (a1 \/ b1, a2 \/ b2, a3 \/ b3, a4 \/ b4, a5 \/ b5, a6 \/ b6, a7 \/ b7, a8 \/ b8, a9 \/ b9, a10 \/ b10, a11 \/ b11, a12 \/ b12, a13 \/ b13)
    (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13) /\ (b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13) = (a1 /\ b1, a2 /\ b2, a3 /\ b3, a4 /\ b4, a5 /\ b5, a6 /\ b6, a7 /\ b7, a8 /\ b8, a9 /\ b9, a10 /\ b10, a11 /\ b11, a12 /\ b12, a13 /\ b13)

-- | Fourteen-way product
instance (Lattice a, Lattice b, Lattice c, Lattice d, Lattice e, Lattice f, Lattice g, Lattice h, Lattice i, Lattice j, Lattice k, Lattice l, Lattice m, Lattice n) => Lattice (a,b,c,d,e,f,g,h,i,j,k,l,m,n)
  where
    bot = (bot,bot,bot,bot,bot,bot,bot,bot,bot,bot,bot,bot,bot,bot)
    top = (top,top,top,top,top,top,top,top,top,top,top,top,top,top)
    (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14) \/ (b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14) = (a1 \/ b1, a2 \/ b2, a3 \/ b3, a4 \/ b4, a5 \/ b5, a6 \/ b6, a7 \/ b7, a8 \/ b8, a9 \/ b9, a10 \/ b10, a11 \/ b11, a12 \/ b12, a13 \/ b13, a14 \/ b14)
    (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14) /\ (b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14) = (a1 /\ b1, a2 /\ b2, a3 /\ b3, a4 /\ b4, a5 /\ b5, a6 /\ b6, a7 /\ b7, a8 /\ b8, a9 /\ b9, a10 /\ b10, a11 /\ b11, a12 /\ b12, a13 /\ b13, a14 /\ b14)

-- | Fifteen-way product
instance (Lattice a, Lattice b, Lattice c, Lattice d, Lattice e, Lattice f, Lattice g, Lattice h, Lattice i, Lattice j, Lattice k, Lattice l, Lattice m, Lattice n, Lattice o) => Lattice (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)
  where
    bot = (bot,bot,bot,bot,bot,bot,bot,bot,bot,bot,bot,bot,bot,bot,bot)
    top = (top,top,top,top,top,top,top,top,top,top,top,top,top,top,top)
    (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15) \/ (b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15) = (a1 \/ b1, a2 \/ b2, a3 \/ b3, a4 \/ b4, a5 \/ b5, a6 \/ b6, a7 \/ b7, a8 \/ b8, a9 \/ b9, a10 \/ b10, a11 \/ b11, a12 \/ b12, a13 \/ b13, a14 \/ b14, a15 \/ b15)
    (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15) /\ (b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15) = (a1 /\ b1, a2 /\ b2, a3 /\ b3, a4 /\ b4, a5 /\ b5, a6 /\ b6, a7 /\ b7, a8 /\ b8, a9 /\ b9, a10 /\ b10, a11 /\ b11, a12 /\ b12, a13 /\ b13, a14 /\ b14, a15 /\ b15)

-- | Accumulated join
unions :: Lattice a => [a] -> a
unions = foldr (\/) bot

-- | Accumulated meet
intersections :: Lattice a => [a] -> a
intersections = foldr (/\) top



-- * Computing fixed points

-- | Generalization of 'fixedPoint' to functions whose argument and result
-- contain (i.e has a lens to) a common lattice
lensedFixedPoint :: Lattice lat =>
    Lens a lat -> Lens b lat -> (a -> b) -> (a -> b)
lensedFixedPoint aLens bLens f a
    | aLat == bLat = (bLens ^= aLat) b
    | otherwise    = lensedFixedPoint aLens bLens f a'
  where
    aLat = a ^! aLens
    b    = f a
    bLat = (b ^! bLens) \/ aLat
    a'   = (aLens ^= bLat) a

-- | Generalization of 'indexedFixedPoint' to functions whose argument and
-- result contain (i.e has a lens to) a common lattice
lensedIndexedFixedPoint :: Lattice lat =>
    Lens a lat -> Lens b lat -> (Int -> a -> b) -> (a -> (b,Int))
lensedIndexedFixedPoint aLens bLens f = go 0
  where
    go i a
        | aLat == bLat = ((bLens ^= aLat) b, i)
        | otherwise    = go (i+1) a'
      where
        aLat = a ^! aLens
        b    = f i a
        bLat = (b ^! bLens) \/ aLat
        a'   = (aLens ^= bLat) a

-- | Take the fixed point of a function. The second argument is an initial
--  element. A sensible default for the initial element is 'bot'.
--
-- The function is not required to be monotonic. It is made monotonic internally
-- by always taking the union of the result and the previous value.
fixedPoint :: Lattice a => (a -> a) -> a -> a
fixedPoint = lensedFixedPoint (iso id id) (iso id id)

-- | Much like 'fixedPoint' but keeps track of the number of iterations
--   in the fixed point iteration. Useful for defining widening operators.
indexedFixedPoint :: Lattice a => (Int -> a -> a) -> a -> (a,Int)
indexedFixedPoint = lensedIndexedFixedPoint (iso id id) (iso id id)

-- | The type of widening operators. A widening operator modifies a
--   function that is subject to fixed point analysis. A widening
--   operator introduces approximations in order to guarantee (fast)
--   termination of the fixed point analysis.
type Widening a = (Int -> a -> a) -> (Int -> a -> a)

-- | A widening operator which defaults to 'top' when the number of
--   iterations goes over the specified value.
cutOffAt :: Lattice a => Int -> Widening a
cutOffAt n f i a | i >= n    = top
                 | otherwise = f i a

-- | A bounded version of 'lensedFixedPoint'. It will always do at least one
-- iteration regardless of the provided bound (in order to return something of
-- the right type).
boundedLensedFixedPoint :: Lattice lat =>
    Int -> Lens a lat -> Lens b lat -> (a -> b) -> (a -> (b,Int))
boundedLensedFixedPoint n aLens bLens f = go 0
  where
    go i a
        | aLat == bLat = ((bLens ^= aLat) b, i)
        | i >= n-1     = ((bLens ^= top) b, i)
        | otherwise    = go (i+1) a'
      where
        aLat = a ^! aLens
        b    = f a
        bLat = (b ^! bLens) \/ aLat
        a'   = (aLens ^= bLat) a
  -- Note: This function achieves a similar effect to
  -- `indexedFixedPoint (cutOffAt n ...)`. The difference is that it works for
  -- lensed fixed points. It would be possible to define a version of `cutOffAt`
  -- that works for lensed fixed points, but such an operator would be less
  -- efficient since it would have to apply the argument function in the base
  -- case as well as the recursive case (in order to return something of the
  -- right type). This means that there would always be one extra unnecessary
  -- iteration. In particular, it would not be possible to get a meaningful
  -- result from performing a single iteration. To only perform a single
  -- iteration, the cut-off function would have to "fail" immediately, which
  -- means that the whole fixed point iteration would also "fail". (A
  -- single-iteration fixed point is an important special case as it avoids
  -- exponential blowup when performing several nested iterations.)

