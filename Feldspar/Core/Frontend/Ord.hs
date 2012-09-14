{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
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

{-# LANGUAGE UndecidableInstances #-}

module Feldspar.Core.Frontend.Ord where

import qualified Prelude

import Data.Int
import Data.Word

import Language.Syntactic

import Feldspar.Prelude
import Feldspar.Core.Types
import Feldspar.Core.Constructs
import Feldspar.Core.Constructs.Ord
import Feldspar.Core.Frontend.Eq

infix 4 <
infix 4 >
infix 4 <=
infix 4 >=

-- | Redefinition of the standard 'Prelude.Ord' class for Feldspar
class (Eq a, Prelude.Ord a, Prelude.Ord (Size a)) => Ord a where
  (<)  :: Data a -> Data a -> Data Bool
  (<)  =  sugarSymC LTH
  (>)  :: Data a -> Data a -> Data Bool
  (>)  =  sugarSymC GTH

  (<=) :: Data a -> Data a -> Data Bool
  (<=) =  sugarSymC LTE
  (>=) :: Data a -> Data a -> Data Bool
  (>=) =  sugarSymC GTE

  min :: Data a -> Data a -> Data a
  min = sugarSymC Min
  max :: Data a -> Data a -> Data a
  max = sugarSymC Max

instance Ord ()
instance Ord Bool
instance Ord Word8
instance Ord Int8
instance Ord Word16
instance Ord Int16
instance Ord Word32
instance Ord Int32
instance Ord Word64
instance Ord Int64
instance Ord WordN
instance Ord IntN
instance Ord Float

-- TODO Should there be more instances?

