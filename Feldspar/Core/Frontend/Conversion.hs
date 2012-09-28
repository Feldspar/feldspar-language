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

module Feldspar.Core.Frontend.Conversion
where

import Prelude hiding (Integral)

import Language.Syntactic

import Feldspar.Range
import Feldspar.Core.Types
import Feldspar.Core.Constructs
import Feldspar.Core.Constructs.Conversion
import Feldspar.Core.Frontend.Integral
import Feldspar.Core.Frontend.Num

i2f :: (Integral a, Size a ~ Range a) => Data a -> Data Float
i2f = i2n

f2i :: Integral a => Data Float -> Data a
f2i = sugarSymF F2I

i2n :: (Integral a, Numeric b, Size a ~ Range a) => Data a -> Data b
i2n = sugarSymF I2N

b2i :: Integral a => Data Bool -> Data a
b2i = sugarSymF B2I

truncate :: Integral a => Data Float -> Data a
truncate = f2i

round :: Integral a => Data Float -> Data a
round = sugarSymF Round

ceiling :: Integral a => Data Float -> Data a
ceiling = sugarSymF Ceiling

floor :: Integral a => Data Float -> Data a
floor = sugarSymF Floor

