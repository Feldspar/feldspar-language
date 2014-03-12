{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

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

module Feldspar.Core.Frontend.Num where

import Data.Complex
import Data.Int
import Data.Word

import Feldspar.Core.Types
import Feldspar.Core.Constructs
import Feldspar.Core.Constructs.Num
import Feldspar.Core.Frontend.Literal

class (Syntax a, Num a) => Numeric a
  where
    fromIntegerNum :: Integer -> a
    absNum         :: a -> a
    signumNum      :: a -> a
    addNum         :: a -> a -> a
    subNum         :: a -> a -> a
    mulNum         :: a -> a -> a

instance (Type a, Num a, Num (Size a)) => Numeric (Data a) where
  fromIntegerNum =  value . fromInteger
  absNum         =  sugarSymF Abs
  signumNum      =  sugarSymF Sign
  addNum         =  sugarSymF Add
  subNum         =  sugarSymF Sub
  mulNum         =  sugarSymF Mul

instance (Type a, Num a, Num (Size a)) => Num (Data a)
  where
    fromInteger = fromIntegerNum
    abs         = absNum
    signum      = signumNum
    (+)         = addNum
    (-)         = subNum
    (*)         = mulNum


