{-# LANGUAGE FlexibleContexts #-}
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

module Feldspar.Core.Frontend.Floating where

import qualified Prelude
import Prelude (Fractional(..))
import Prelude (Float,Double)
import Data.Complex

import Feldspar.Core.Types
import Feldspar.Core.Constructs
import Feldspar.Core.Constructs.Floating
import Feldspar.Core.Frontend.Fractional

infixr 8 **

class (Fractional a) => Floating a where
  pi        :: a
  exp       :: a -> a
  sqrt      :: a -> a
  log       :: a -> a
  (**)      :: a -> a -> a
  logBase   :: a -> a -> a
  sin       :: a -> a
  tan       :: a -> a
  cos       :: a -> a
  asin      :: a -> a
  atan      :: a -> a
  acos      :: a -> a
  sinh      :: a -> a
  tanh      :: a -> a
  cosh      :: a -> a
  asinh     :: a -> a
  atanh     :: a -> a
  acosh     :: a -> a

instance (Prelude.Floating a, Type a, Prelude.Num (Size a)) => Floating (Data a) where
  pi        =  sugarSymF Pi
  exp       =  sugarSymF Exp
  sqrt      =  sugarSymF Sqrt
  log       =  sugarSymF Log
  (**)      =  sugarSymF Pow
  logBase   =  sugarSymF LogBase
  sin       =  sugarSymF Sin
  tan       =  sugarSymF Tan
  cos       =  sugarSymF Cos
  asin      =  sugarSymF Asin
  atan      =  sugarSymF Atan
  acos      =  sugarSymF Acos
  sinh      =  sugarSymF Sinh
  tanh      =  sugarSymF Tanh
  cosh      =  sugarSymF Cosh
  asinh     =  sugarSymF Asinh
  atanh     =  sugarSymF Atanh
  acosh     =  sugarSymF Acosh

π :: Floating a => a
π = pi
