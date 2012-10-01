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

-- | Module "Data.TypeLevel.Num.Aliases" is re-exported because
-- wrappers use type level numbers frequently
module Feldspar.Wrap
  ( Wrap(..)
  , Data'(..)
  , module Data.TypeLevel.Num.Aliases
  , D0
  , D1
  , D2
  , D3
  , D4
  , D5
  , D6
  , D7
  , D8
  , D9
  ) where


import Feldspar.Core.Constructs
import Feldspar.Core.Types

import Language.Syntactic

import Data.TypeLevel.Num.Aliases
import Data.TypeLevel.Num.Reps (D0, D1, D2, D3, D4, D5, D6, D7, D8, D9 )


-- | Wrapping Feldspar functions
class Wrap t w where
    wrap :: t -> w

-- | Basic instances to handle @Data a@ input and output.
-- Other instances are located in the concerned libraries.
instance Wrap (Data a) (Data a) where
    wrap = id

instance (Wrap t u) => Wrap (Data a -> t) (Data a -> u) where
    wrap f = wrap . f

-- | Extended 'Data' to be used in wrappers
data Data' s a =
    Data'
    { unData'   :: Data a
    }

-- Syntactic instance for 'Data''

instance Type a => Syntactic (Data' s a) FeldDomainAll
  where
    type Internal (Data' s a) = a
    desugar = desugar . unData'
    sugar   = Data' . sugar

instance Type a => Syntax (Data' s a)

