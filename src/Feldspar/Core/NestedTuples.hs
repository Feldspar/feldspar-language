{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -Wall #-}

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

module Feldspar.Core.NestedTuples
  ( Tuple(..)
  , Proxy(..)
  , sel
  , tuple
  , build
  , nfst
  , nsnd
  -- * Building tuples
  , onetup
  , twotup
  , threetup
  , fourtup
  , fivetup
  , sixtup
  , seventup
  ) where

import Data.Kind
import Data.Proxy
import GHC.TypeLits

-- | Data and type constructor for nested tuples
infixr 5 :*

-- | GADT for tuples that ensure that they are only built with TNil and :*
data Tuple a where
  TNil :: Tuple '[]
  (:*) :: a -> Tuple b -> Tuple (a ': b)

-- | Eq and Show instances

instance (Eq a, Eq (Tuple b)) => Eq (Tuple (a ': b)) where
  (a1 :* b1) == (a2 :* b2) = a1 == a2 && b1 == b2

instance Eq (Tuple '[]) where
  TNil == TNil = True

instance (Show a, Show (Tuple b)) => Show (Tuple (a ': b)) where
  show (x :* xs) = show x ++ " :* " ++ show xs

instance Show (Tuple '[]) where
  show TNil = "TNil"

-- | Selecting components of a tuple
--     sel (Proxy @0)
--   selects the first component of any nonempty nested tuple while
--     sel (Proxy @1)
--   selects the second component of any tuple with arity at least two.

-- | Type recursive selection based on type level naturals
class TSelect (n :: Nat) (a :: [Type]) (b :: Type) | n a -> b where
  sel :: Proxy n -> Tuple a -> b

instance TSelect 0 (a ': t) a where
  sel _ (x :* _) = x

instance {-# OVERLAPS #-} TSelect (n - 1) t b => TSelect n (a ': t) b where
  sel _ (_ :* y) = sel (Proxy @(n - 1)) y

-- | Reverse a nested tuple
class TupleReverse a b c where
  tupleReverse :: Tuple a -> Tuple b -> Tuple c

instance TupleReverse (h ': a) t c => TupleReverse a (h ': t) c where
  tupleReverse xs (h :* t) = tupleReverse (h :* xs) t

instance a ~ c => TupleReverse a '[] c where
  tupleReverse xs TNil = xs

-- | Varargs class for building tuples
class TupleBuild a b where
  stuple :: Tuple a -> b

instance TupleBuild (b ': a) c => TupleBuild a (b -> c) where
  stuple xs x = stuple (x :* xs)

instance Tuple a ~ b => TupleBuild a (TT b) where
  stuple = TT

newtype TT a = TT a

-- | The functions below are intended to be used to build nested tuples.
--     build $ tuple e1 ... ek
--   builds the nested tuple (e1 :* ... :* ek :* TNil).

-- | Variadic function for building a nested tuple.
tuple :: TupleBuild '[] b => b
tuple = stuple TNil

-- | Function for marking the end of the application of 'tuple'.
build :: TupleReverse '[] a b => TT (Tuple a) -> Tuple b
build (TT x) = tupleReverse TNil x

-- * Non-variadic tuples

-- | Convenience functions for short fixed tuples.
--   Since ordinary tuples are desugared to nested tuples, desugared
--   contexts of pairs, e.g. in the Sequential AST operator, need types
--   of the form Tuple '[a, b] for which we provide support here.
--   A second reason to provide these functions is that the variable arity
--   of build does not interact well with <$> and <*> in instances.

-- | Extract the first component of a tuple.
nfst :: TSelect 0 a b => Tuple a -> b
nfst = sel (Proxy @0)

-- | Extract the second component of a tuple.
nsnd :: TSelect 1 a b => Tuple a -> b
nsnd = sel (Proxy @1)

-- | Function for constructing a one-tuple from its components.
onetup :: a -> Tuple '[a]
onetup a = build $ tuple a

-- | Function for constructing a two-tuple from its components.
twotup :: a -> b -> Tuple '[a, b]
twotup a b = build $ tuple a b

-- | Function for constructing a three-tuple from its components.
threetup :: a -> b -> c -> Tuple '[a, b, c]
threetup a b c = build $ tuple a b c

-- | Function for constructing a four-tuple from its components.
fourtup :: a -> b -> c -> d -> Tuple '[a, b, c, d]
fourtup a b c d = build $ tuple a b c d

-- | Function for constructing a five-tuple from its components.
fivetup :: a -> b -> c -> d -> e -> Tuple '[a, b, c, d, e]
fivetup a b c d e = build $ tuple a b c d e

-- | Function for constructing a six-tuple from its components.
sixtup :: a -> b -> c -> d -> e -> f -> Tuple '[a, b, c, d, e, f]
sixtup a b c d e f = build $ tuple a b c d e f

-- | Function for constructing a seven-tuple from its components.
seventup :: a -> b -> c -> d -> e -> f -> g -> Tuple '[a, b, c, d, e, f, g]
seventup a b c d e f g = build $ tuple a b c d e f g
