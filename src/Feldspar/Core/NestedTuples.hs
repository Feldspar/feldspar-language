{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
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
  , TSelect
  , sel
  , tuple
  , build
  -- * Taking tuples apart
  , nfst
  , nsnd
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
  -- * Building tuples
  , onetup
  , twotup
  , threetup
  , fourtup
  , fivetup
  , sixtup
  , seventup
  ) where

import Data.Hash (Hash, Hashable(..), combine, hashInt)
import Data.Kind
import Data.Proxy
import GHC.TypeLits
import System.Plugins.MultiStage (Marshal(..))
import Test.QuickCheck (Arbitrary(..))

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

class HashTup a where
  hashTup :: Tuple a -> Hash

instance HashTup '[] where
  hashTup _ = hashInt 1

instance (Hashable h, HashTup t) => HashTup (h ': t) where
  hashTup (x :* xs) = hashInt 2 `combine` hash x `combine` hash xs

-- This instance is needed to allow nested tuples as literals (by the value function)
instance HashTup a => Hashable (Tuple a) where
  hash = hashTup

-- | Arbitrary instances for nested tuples
instance Arbitrary (Tuple '[]) where
  arbitrary = return TNil

instance (Arbitrary a, Arbitrary (Tuple b)) => Arbitrary (Tuple (a ': b)) where
  arbitrary = do a <- arbitrary
                 b <- arbitrary
                 return (a :* b)

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

-- | Extract the first component of a tuple.
sel1 :: TSelect 0 a b => Tuple a -> b
sel1 = nfst

-- | Extract the second component of a tuple.
sel2 :: TSelect 1 a b => Tuple a -> b
sel2 = nsnd

-- | Extract the third component of a tuple.
sel3 :: TSelect 2 a b => Tuple a -> b
sel3 = sel (Proxy @2)

-- | Extract the fourth component of a tuple.
sel4 :: TSelect 3 a b => Tuple a -> b
sel4 = sel (Proxy @3)

-- | Extract the fifth component of a tuple.
sel5 :: TSelect 4 a b => Tuple a -> b
sel5 = sel (Proxy @4)

-- | Extract the sixth component of a tuple.
sel6 :: TSelect 5 a b => Tuple a -> b
sel6 = sel (Proxy @5)

-- | Extract the seventh component of a tuple.
sel7 :: TSelect 6 a b => Tuple a -> b
sel7 = sel (Proxy @6)

-- | Extract the eight component of a tuple.
sel8 :: TSelect 7 a b => Tuple a -> b
sel8 = sel (Proxy @7)

-- | Extract the ninth component of a tuple.
sel9 :: TSelect 8 a b => Tuple a -> b
sel9 = sel (Proxy @8)

-- | Extract the tenth component of a tuple.
sel10 :: TSelect 9 a b => Tuple a -> b
sel10 = sel (Proxy @9)

-- | Extract the eleventh component of a tuple.
sel11 :: TSelect 10 a b => Tuple a -> b
sel11 = sel (Proxy @10)

-- | Extract the twelth component of a tuple.
sel12 :: TSelect 11 a b => Tuple a -> b
sel12 = sel (Proxy @11)

-- | Extract the thirteenth component of a tuple.
sel13 :: TSelect 12 a b => Tuple a -> b
sel13 = sel (Proxy @12)

-- | Extract the fourteenth component of a tuple.
sel14 :: TSelect 13 a b => Tuple a -> b
sel14 = sel (Proxy @13)

-- | Extract the fifteenth component of a tuple.
sel15 :: TSelect 14 a b => Tuple a -> b
sel15 = sel (Proxy @14)

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

-- * Marshaling

instance Marshal a => Marshal (Tuple '[a]) where
  type Rep (Tuple '[a]) = Rep a
  to (a :* TNil) = to a
  from a = onetup <$> from a

instance (Marshal a, Marshal b) => Marshal (Tuple '[a, b]) where
  type Rep (Tuple '[a, b]) = (Rep a, Rep b)
  to (a :* b :* TNil) = (,) <$> to a <*> to b
  from (a, b) = twotup <$> from a <*> from b

instance (Marshal a, Marshal b, Marshal c) => Marshal (Tuple '[a, b, c]) where
  type Rep (Tuple '[a, b, c]) = (Rep a, Rep b, Rep c)
  to (a :* b :* c :* TNil) = (,,) <$> to a <*> to b <*> to c
  from (a, b, c) = threetup <$> from a <*> from b <*> from c

instance (Marshal a, Marshal b, Marshal c, Marshal d)
         => Marshal (Tuple '[a, b, c, d]) where
  type Rep (Tuple '[a, b, c, d]) = (Rep a, Rep b, Rep c, Rep d)
  to (a :* b :* c :* d :* TNil) = (,,,) <$> to a <*> to b <*> to c <*> to d
  from (a, b, c, d) = fourtup <$> from a <*> from b <*> from c <*> from d

instance (Marshal a, Marshal b, Marshal c, Marshal d, Marshal e)
         => Marshal (Tuple '[a, b, c, d, e]) where
  type Rep (Tuple '[a, b, c, d, e]) = (Rep a, Rep b, Rep c, Rep d, Rep e)
  to (a :* b :* c :* d :* e :* TNil) =
    (,,,,) <$> to a <*> to b <*> to c <*> to d <*> to e
  from (a, b, c, d, e) =
    fivetup <$> from a <*> from b <*> from c <*> from d <*> from e

instance (Marshal a, Marshal b, Marshal c, Marshal d, Marshal e, Marshal f)
         => Marshal (Tuple '[a, b, c, d, e, f]) where
  type Rep (Tuple '[a, b, c, d, e, f]) =
    (Rep a, Rep b, Rep c, Rep d, Rep e, Rep f)
  to (a :* b :* c :* d :* e :* f :* TNil) =
    (,,,,,) <$> to a <*> to b <*> to c <*> to d <*> to e <*> to f
  from (a, b, c, d, e, f) =
    sixtup <$> from a <*> from b <*> from c <*> from d <*> from e <*> from f

instance ( Marshal a, Marshal b, Marshal c, Marshal d, Marshal e, Marshal f
         , Marshal g)
         => Marshal (Tuple '[a, b, c, d, e, f, g]) where
  type Rep (Tuple '[a, b, c, d, e, f, g]) =
    (Rep a, Rep b, Rep c, Rep d, Rep e, Rep f, Rep g)
  to (a :* b :* c :* d :* e :* f :* g :* TNil) =
    (,,,,,,) <$> to a <*> to b <*> to c <*> to d <*> to e <*> to f <*> to g
  from (a, b, c, d, e, f, g) =
    seventup <$> from a <*> from b <*> from c <*> from d <*> from e <*> from f
             <*> from g
