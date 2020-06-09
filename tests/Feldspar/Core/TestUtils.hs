{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--
-- Copyright (c) 2020, ERICSSON AB
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

-- | Utilities and instances for testing Feldspar

module Feldspar.Core.TestUtils where

import qualified Prelude as P

import Feldspar
import Feldspar.Range
import qualified Feldspar.SimpleVector as SV (Vector)

import Data.Hash (Hashable)
import Control.DeepSeq (NFData(..))
import System.Random (Random(..))
import Test.QuickCheck (Arbitrary(..), Testable(..), Property(..), (===), (==>))

deriving instance Arbitrary       WordN
deriving instance NFData          WordN
deriving instance Random          WordN

deriving instance Arbitrary       IntN
deriving instance NFData          IntN
deriving instance Random          IntN

--------------------------------------------------------------------------------
-- * QuickCheck
--------------------------------------------------------------------------------

instance (Type a, Arbitrary a, Hashable a) => Arbitrary (Data a)
  where
    arbitrary = fmap value arbitrary

instance Testable (Data Bool)
  where
    property = property . eval

(===>) :: Testable prop => Data Bool -> prop -> Property
a ===> b = eval a ==> b


-- | Test that two function of the same arity have the same semantics
class Equal a
  where
    (====) :: a -> a -> Property

instance {-# OVERLAPPABLE #-} (P.Eq a, Show a) => Equal a
  where
    x ==== y = x === y

instance (Show a, Arbitrary a, Equal b) => Equal (a -> b)
  where
    f ==== g = property (\x -> f x ==== g x)

instance (Arbitrary (Internal a), Syntax a, Hashable (Internal a)) => Arbitrary (SV.Vector a)
  where
    arbitrary = fmap value arbitrary
