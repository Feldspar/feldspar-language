{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
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

module Feldspar.Core.Frontend.Integral
where

import qualified Prelude as P

import Data.Int
import Data.Word

import Language.Syntactic

import Feldspar.Range
import Feldspar.Core.Types
import Feldspar.Core.Constructs
import Feldspar.Core.Constructs.Integral

import Feldspar.Core.Frontend.Condition
import Feldspar.Core.Frontend.Eq
import Feldspar.Core.Frontend.Logic
import Feldspar.Core.Frontend.Num
import Feldspar.Core.Frontend.Ord

class (Ord a, Numeric a, BoundedInt a, P.Integral a, Size a ~ Range a) => Integral a
  where
    quot :: Data a -> Data a -> Data a
    quot = sugarSymF Quot
    rem  :: Data a -> Data a -> Data a
    rem  = sugarSymF Rem
    div  :: Data a -> Data a -> Data a
    div  = divSem
    mod  :: Data a -> Data a -> Data a
    mod  = sugarSymF Mod
    (^)  :: Data a -> Data a -> Data a
    (^)  = sugarSymF Exp

-- TODO: This is a short-term hack because the compiler doesn't compile
-- the Div construct correctly. So we give the semantics of div in terms of
-- quot directly, and never use the Div construct. In the long term the
-- compiler should be fixed, but it involves writing type corrector plugins
-- and this solution was quicker.
divSem :: (Integral a)
       => Data a -> Data a -> Data a
divSem x y = (x > 0 && y < 0 || x < 0 && y > 0) && rem x y /= 0 ?
             (quot x y P.- 1,quot x y)

instance Integral Word8
instance Integral Word16
instance Integral Word32
instance Integral Word64
instance Integral WordN
instance Integral Int8
instance Integral Int16
instance Integral Int32
instance Integral Int64
instance Integral IntN

