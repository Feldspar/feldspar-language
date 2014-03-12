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

module Feldspar.Core.Frontend.Eq
where

import qualified Prelude as P

import Data.Int
import Data.Word
import Data.Complex

import Feldspar.Prelude
import Feldspar.Core.Types
import Feldspar.Core.Constructs
import Feldspar.Core.Constructs.Eq

import Feldspar.Core.Frontend.Logic

infix 4 ==
infix 4 /=

-- | Redefinition of the standard 'P.Eq' class for Feldspar
class Syntax a => Eq a
  where
    (==) :: a -> a -> Data Bool
    (/=) :: a -> a -> Data Bool

instance (Type a, P.Eq a) => Eq (Data a) where
    (==) = sugarSymF Equal
    (/=) = sugarSymF NotEqual
{-
instance (Eq a, Eq b)                               => Eq (a,b)          where
  (a,b) == (c,d) = a == c && b == d

instance (Eq a, Eq b, Eq c)                         => Eq (a,b,c)
instance (Eq a, Eq b, Eq c, Eq d)                   => Eq (a,b,c,d)
instance (Eq a, Eq b, Eq c, Eq d, Eq e)             => Eq (a,b,c,d,e)
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f)       => Eq (a,b,c,d,e,f)
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g) => Eq (a,b,c,d,e,f,g)
-}
