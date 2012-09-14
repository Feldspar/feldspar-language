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

module Feldspar.Core.Frontend.Num where

import Data.Complex
import Data.Int
import Data.Word

import Language.Syntactic

import Feldspar.Core.Types
import Feldspar.Core.Constructs
import Feldspar.Core.Constructs.Num
import Feldspar.Core.Frontend.Literal

class (Type a, Num a, Num (Size a)) => Numeric a
  where
    fromIntegerNum :: Integer -> Data a
    fromIntegerNum =  value . fromInteger
    absNum         :: Data a -> Data a
    absNum         =  sugarSymC Abs
    signumNum      :: Data a -> Data a
    signumNum      =  sugarSymC Sign
    addNum         :: Data a -> Data a -> Data a
    addNum         =  sugarSymC Add
    subNum         :: Data a -> Data a -> Data a
    subNum         =  sugarSymC Sub
    mulNum         :: Data a -> Data a -> Data a
    mulNum         =  sugarSymC Mul

instance Numeric Word8
instance Numeric Word16
instance Numeric Word32
instance Numeric Word64
instance Numeric WordN
instance Numeric Int8
instance Numeric Int16
instance Numeric Int32
instance Numeric Int64
instance Numeric IntN

instance Numeric Float

instance (Type a, RealFloat a) => Numeric (Complex a)

instance (Numeric a) => Num (Data a)
  where
    fromInteger = fromIntegerNum
    abs         = absNum
    signum      = signumNum
    (+)         = addNum
    (-)         = subNum
    (*)         = mulNum


