--
-- Copyright (c) 2019, ERICSSON AB
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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}

module Feldspar.Core.Tuple
  ( -- * Converting tuples to nested tuples
    Tuply(..)
    -- * Taking tuples apart
  , sel1
  , sel2
  , sel3
  , sel4
  , sel5
  , sel6
  , sel7
  , sel8
  , sel9
  , sel10
  , sel11
  , sel12
  , sel13
  , sel14
  , sel15
  ) where

import Feldspar.Core.NestedTuples (TSelect, Tuple(..), build, tuple)
import qualified Feldspar.Core.NestedTuples as N

class Tuply a where
  type Unpack a
  unpack :: a -> Unpack a

instance Tuply (a,b) where
  type Unpack (a, b) = Tuple '[a, b]
  unpack (a, b) = build $ tuple a b
  {-# INLINE unpack #-}

instance Tuply (a,b,c) where
  type Unpack (a, b, c) = Tuple '[a, b, c]
  unpack (a, b, c) = build $ tuple a b c
  {-# INLINE unpack #-}

instance Tuply (a,b,c,d) where
  type Unpack (a, b, c, d) = Tuple '[a, b, c, d]
  unpack (a, b, c, d) = build $ tuple a b c d
  {-# INLINE unpack #-}

instance Tuply (a,b,c,d,e) where
  type Unpack (a, b, c, d, e) = Tuple '[a, b, c, d, e]
  unpack (a, b, c, d, e) = build $ tuple a b c d e
  {-# INLINE unpack #-}

instance Tuply (a,b,c,d,e,f) where
  type Unpack (a, b, c, d, e, f) = Tuple '[a, b, c, d, e, f]
  unpack (a, b, c, d, e, f) = build $ tuple a b c d e f
  {-# INLINE unpack #-}

instance Tuply (a,b,c,d,e,f,g) where
  type Unpack (a, b, c, d, e, f, g) = Tuple '[a, b, c, d, e, f, g]
  unpack (a, b, c, d, e, f, g) = build $ tuple a b c d e f g
  {-# INLINE unpack #-}

instance Tuply (a,b,c,d,e,f,g,h) where
  type Unpack (a, b, c, d, e, f, g, h) = Tuple '[a, b, c, d, e, f, g, h]
  unpack (a, b, c, d, e, f, g, h) = build $ tuple a b c d e f g h
  {-# INLINE unpack #-}

instance Tuply (a,b,c,d,e,f,g,h,i) where
  type Unpack (a, b, c, d, e, f, g, h, i)
             = Tuple '[a, b, c, d, e, f, g, h, i]
  unpack (a, b, c, d, e, f, g, h, i) = build $ tuple a b c d e f g h i
  {-# INLINE unpack #-}

instance Tuply (a,b,c,d,e,f,g,h,i,j) where
  type Unpack (a, b, c, d, e, f, g, h, i, j)
             = Tuple '[a, b, c, d, e, f, g, h, i, j]
  unpack (a, b, c, d, e, f, g, h, i, j) = build $ tuple a b c d e f g h i j
  {-# INLINE unpack #-}

instance Tuply (a,b,c,d,e,f,g,h,i,j,k) where
  type Unpack (a, b, c, d, e, f, g, h, i, j, k)
             = Tuple '[a, b, c, d, e, f, g, h, i, j, k]
  unpack (a, b, c, d, e, f, g, h, i, j, k) = build $ tuple a b c d e f g h i j k
  {-# INLINE unpack #-}

instance Tuply (a,b,c,d,e,f,g,h,i,j,k,l) where
  type Unpack (a, b, c, d, e, f, g, h, i, j, k, l)
             = Tuple '[a, b, c, d, e, f, g, h, i, j, k, l]
  unpack (a, b, c, d, e, f, g, h, i, j, k, l)
             = build $ tuple a b c d e f g h i j k l
  {-# INLINE unpack #-}

instance Tuply (a,b,c,d,e,f,g,h,i,j,k,l,m) where
  type Unpack (a, b, c, d, e, f, g, h, i, j, k, l, m)
             = Tuple '[a, b, c, d, e, f, g, h, i, j, k, l, m]
  unpack (a, b, c, d, e, f, g, h, i, j, k, l, m)
             = build $ tuple a b c d e f g h i j k l m
  {-# INLINE unpack #-}

instance Tuply (a,b,c,d,e,f,g,h,i,j,k,l,m,n) where
  type Unpack (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
             = Tuple '[a, b, c, d, e, f, g, h, i, j, k, l, m, n]
  unpack (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
             = build $ tuple a b c d e f g h i j k l m n
  {-# INLINE unpack #-}

instance Tuply (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) where
  type Unpack (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
             = Tuple '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o]
  unpack (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
             = build $ tuple a b c d e f g h i j k l m n o
  {-# INLINE unpack #-}

-- | Extract the first component of a tuple.
sel1 :: (TSelect 0 a c, Tuply t, Unpack t ~ Tuple a) => t -> c
sel1 = N.sel1 . unpack
{-# INLINE sel1 #-}

-- | Extract the second component of a tuple.
sel2 :: (TSelect 1 a c, Tuply t, Unpack t ~ Tuple a) => t -> c
sel2 = N.sel2 . unpack
{-# INLINE sel2 #-}

-- | Extract the third component of a tuple.
sel3 :: (TSelect 2 a c, Tuply t, Unpack t ~ Tuple a) => t -> c
sel3 = N.sel3 . unpack
{-# INLINE sel3 #-}

-- | Extract the fourth component of a tuple.
sel4 :: (TSelect 3 a c, Tuply t, Unpack t ~ Tuple a) => t -> c
sel4 = N.sel4 . unpack
{-# INLINE sel4 #-}

-- | Extract the fifth component of a tuple.
sel5 :: (TSelect 4 a c, Tuply t, Unpack t ~ Tuple a) => t -> c
sel5 = N.sel5 . unpack
{-# INLINE sel5 #-}

-- | Extract the sixth component of a tuple.
sel6 :: (TSelect 5 a c, Tuply t, Unpack t ~ Tuple a) => t -> c
sel6 = N.sel6 . unpack
{-# INLINE sel6 #-}

-- | Extract the seventh component of a tuple.
sel7 :: (TSelect 6 a c, Tuply t, Unpack t ~ Tuple a) => t -> c
sel7 = N.sel7 . unpack
{-# INLINE sel7 #-}

-- | Extract the eigth component of a tuple.
sel8 :: (TSelect 7 a c, Tuply t, Unpack t ~ Tuple a) => t -> c
sel8 = N.sel8 . unpack
{-# INLINE sel8 #-}

-- | Extract the ninth component of a tuple.
sel9 :: (TSelect 8 a c, Tuply t, Unpack t ~ Tuple a) => t -> c
sel9 = N.sel9 . unpack
{-# INLINE sel9 #-}

-- | Extract the tenth component of a tuple.
sel10 :: (TSelect 9 a c, Tuply t, Unpack t ~ Tuple a) => t -> c
sel10 = N.sel10 . unpack
{-# INLINE sel10 #-}

-- | Extract the eleventh component of a tuple.
sel11 :: (TSelect 10 a c, Tuply t, Unpack t ~ Tuple a) => t -> c
sel11 = N.sel11 . unpack
{-# INLINE sel11 #-}

-- | Extract the twelth component of a tuple.
sel12 :: (TSelect 11 a c, Tuply t, Unpack t ~ Tuple a) => t -> c
sel12 = N.sel12 . unpack
{-# INLINE sel12 #-}

-- | Extract the thirteenth component of a tuple.
sel13 :: (TSelect 12 a c, Tuply t, Unpack t ~ Tuple a) => t -> c
sel13 = N.sel13 . unpack
{-# INLINE sel13 #-}

-- | Extract the fourteenth component of a tuple.
sel14 :: (TSelect 13 a c, Tuply t, Unpack t ~ Tuple a) => t -> c
sel14 = N.sel14 . unpack
{-# INLINE sel14 #-}

-- | Extract the fifteenth component of a tuple.
sel15 :: (TSelect 14 a c, Tuply t, Unpack t ~ Tuple a) => t -> c
sel15 = N.sel15 . unpack
{-# INLINE sel15 #-}
