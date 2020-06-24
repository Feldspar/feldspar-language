{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
  ( (:*), TNil
  , Tuple(..)
  , Skip(..)
  , First(..)
  , sel
  , tuple
  , build
  , NPair
  , npair
  , nfst
  , nsnd
  ) where

-- | Data and type constructor for nested tuples
infixr 5 :*

-- | Types for nonempty and empty nested tuples
data a :* b
data TNil

-- | GADT for tuples that ensure that they are build only with TNil and :*
data Tuple a where
  (:*) :: a -> Tuple b -> Tuple (a :* b)
  TNil :: Tuple TNil

-- | Eq and Show instances

instance (Eq a, Eq (Tuple b)) => Eq (Tuple (a :* b)) where
  (a1 :* b1) == (a2 :* b2) = a1 == a2 && b1 == b2

instance Eq (Tuple TNil) where
  TNil == TNil = True

instance (Show a, Show (Tuple b)) => Show (Tuple (a :* b)) where
  show (x :* xs) = show x ++ " :* " ++ show xs

instance Show (Tuple TNil) where
  show TNil = "TNil"

-- | Selecting components of a tuple

-- | Designating a tuple component
newtype Skip a = Skip {unSkip :: a}
data First = First

-- | Type recursive selection based on designator
class TSelect s t where
  type TSelResult s t
  selR :: s -> Tuple t -> TSelResult s t

instance TSelect a t => TSelect (Skip a) (b :* t) where
  type TSelResult (Skip a) (b :* t) = TSelResult a t
  selR s (_ :* y) = selR (unSkip s) y

instance TSelect First (b :* t) where
  type TSelResult First (b :* t) = b
  selR _ (x :* _) = x

-- | Exposed select interface
sel :: TSelect s t => s -> Tuple t -> TSelResult s t
sel = selR

-- | Reverse a tuple

class TupleReverse a b c where
  tupleReverse :: Tuple a -> Tuple b -> Tuple c

instance TupleReverse (h :* a) t c => TupleReverse a (h :* t) c where
  tupleReverse xs (h :* t) = tupleReverse (h :* xs) t

instance a ~ c => TupleReverse a TNil c where
  tupleReverse xs TNil = xs

-- | Varargs class for building tuples
class TupleBuild a b where
  stuple :: Tuple a -> b

instance TupleBuild (b :* a) c => TupleBuild a (b -> c) where
  stuple xs x = stuple (x :* xs)

instance Tuple a ~ b => TupleBuild a (TT b) where
  stuple = TT

newtype TT a = TT a

tuple :: TupleBuild TNil b => b
tuple = stuple TNil

build :: TupleReverse TNil a b => TT (Tuple a) -> Tuple b
build (TT x) = tupleReverse TNil x

-- | Convenience functions
type NPair a b = Tuple (a :* b :* TNil)

npair :: a -> b -> NPair a b
npair x y = build $ tuple x y

nfst :: NPair a b -> a
nfst = sel First

nsnd :: NPair a b -> b
nsnd = sel $ Skip First
