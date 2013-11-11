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
import Prelude (Float,Double)
import Data.Complex

import Feldspar.Core.Constructs
import Feldspar.Core.Constructs.Floating
import Feldspar.Core.Frontend.Fractional

-- Make new class, with "Data" in all the types

infixr 8 **

class (Fraction a, Prelude.Floating a) => Floating a where
  pi        :: Data a
  pi        =  sugarSymF Pi
  exp       :: Data a -> Data a
  exp       =  sugarSymF Exp
  sqrt      :: Data a -> Data a
  sqrt      =  sugarSymF Sqrt
  log       :: Data a -> Data a
  log       =  sugarSymF Log
  (**)      :: Data a -> Data a -> Data a
  (**)      =  sugarSymF Pow
  logBase   :: Data a -> Data a -> Data a
  logBase   =  sugarSymF LogBase
  sin       :: Data a -> Data a
  sin       =  sugarSymF Sin
  tan       :: Data a -> Data a
  tan       =  sugarSymF Tan
  cos       :: Data a -> Data a
  cos       =  sugarSymF Cos
  asin      :: Data a -> Data a
  asin      =  sugarSymF Asin
  atan      :: Data a -> Data a
  atan      =  sugarSymF Atan
  acos      :: Data a -> Data a
  acos      =  sugarSymF Acos
  sinh      :: Data a -> Data a
  sinh      =  sugarSymF Sinh
  tanh      :: Data a -> Data a
  tanh      =  sugarSymF Tanh
  cosh      :: Data a -> Data a
  cosh      =  sugarSymF Cosh
  asinh     :: Data a -> Data a
  asinh     =  sugarSymF Asinh
  atanh     :: Data a -> Data a
  atanh     =  sugarSymF Atanh
  acosh     :: Data a -> Data a
  acosh     =  sugarSymF Acosh

instance Floating Float
instance Floating Double

instance (Fraction a, Prelude.RealFloat a) => Floating (Complex a)

