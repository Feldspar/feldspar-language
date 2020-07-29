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
{-# OPTIONS_GHC -Wall #-}
-- The qualified prelude is required for GHCI but not the module itself so
-- disable unused import warnings.
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Interface to the essential parts of the Feldspar language. High-level
-- libraries have to be imported separately.

module Feldspar
  ( module Prelude.EDSL
  -- * Reexported standard modules
  , Complex (..)
  , module Data.Int
  , module Data.Word

  -- * Feldspar types
  , Range (..)
  , module Feldspar.Core.Types

  -- * Frontend
  , module Feldspar.Core.Frontend
  , module Feldspar.Core.Collection
  ) where

import qualified Prelude
  -- In order to be able to use the Feldspar module in GHCi without getting name
  -- clashes.

import Prelude.EDSL

import Data.Complex
import Data.Int hiding (Int)
import Data.Word

import Feldspar.Range
import Feldspar.Core.Types
import Feldspar.Core.Frontend
import Feldspar.Core.Collection
