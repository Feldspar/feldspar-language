{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

module Feldspar.Core.NestedTuples where

import Data.Typeable

-- | Data and type constructor for nested tuples
infixr 5 :*

-- | Types for nonempty and empty nested tuples
data a :* b
data TNil

-- | Raw tuples which can only be used in restricted ways
data RTuple a where
  (:*) :: a -> RTuple b -> RTuple (a :* b)
  TNil :: RTuple TNil

deriving instance Typeable RTuple -- Not necessary with newer versions of GHC

-- | Eq and Show instances

instance (Eq a, Eq (RTuple b)) => Eq (RTuple (a :* b)) where
  (a1 :* b1) == (a2 :* b2) = a1 == a2 && b1 == b2

instance Eq (RTuple TNil) where
  TNil == TNil = True

instance (Show a, Show (RTuple b)) => Show (RTuple (a :* b)) where
  show (x :* xs) = show x ++ " :* " ++ show xs

instance Show (RTuple TNil) where
  show TNil = "TNil"

-- | Wrap a raw tuple to make it a first class object

newtype Tuple a = Tuple {unTup :: RTuple a}

deriving instance Eq (Tuple TNil)
deriving instance (Eq a, Eq (RTuple b)) => Eq (Tuple (a :* b))

instance Show (RTuple a) => Show (Tuple a) where
  show t = "<" ++ show (unTup t) ++ ">"

tuple :: RTuple a -> Tuple a
tuple t = Tuple t

-- | Selecting components of a tuple

-- | Designating a tuple component
newtype Skip a = Skip {unSkip :: a}
data First = First

-- | Type recursive selection based on designator
class Select s t where
  type SelResult s t
  selR :: s -> RTuple t -> SelResult s t

instance Select a t => Select (Skip a) (b :* t) where
  type SelResult (Skip a) (b :* t) = SelResult a t
  selR s (_ :* y) = selR (unSkip s) y

instance Select First (b :* t) where
  type SelResult First (b :* t) = b
  selR _ (x :* _) = x

-- | Exposed select interface which extracts the raw tuple
sel :: Select s t => s -> Tuple t -> SelResult s t
sel s (Tuple t) = selR s t
