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

module Feldspar.Core.Constructs.Floating
    ( FLOATING (..)
    ) where

import Language.Syntactic
import Language.Syntactic.Constructs.Binding

import Feldspar.Core.Types
import Feldspar.Core.Interpretation

data FLOATING a
  where
    Pi      :: (Type a, Floating a) => FLOATING (Full a)
    Exp     :: (Type a, Floating a) => FLOATING (a :-> Full a)
    Sqrt    :: (Type a, Floating a) => FLOATING (a :-> Full a)
    Log     :: (Type a, Floating a) => FLOATING (a :-> Full a)
    Pow     :: (Type a, Floating a) => FLOATING (a :-> a :-> Full a)
    LogBase :: (Type a, Floating a) => FLOATING (a :-> a :-> Full a)
    Sin     :: (Type a, Floating a) => FLOATING (a :-> Full a)
    Tan     :: (Type a, Floating a) => FLOATING (a :-> Full a)
    Cos     :: (Type a, Floating a) => FLOATING (a :-> Full a)
    Asin    :: (Type a, Floating a) => FLOATING (a :-> Full a)
    Atan    :: (Type a, Floating a) => FLOATING (a :-> Full a)
    Acos    :: (Type a, Floating a) => FLOATING (a :-> Full a)
    Sinh    :: (Type a, Floating a) => FLOATING (a :-> Full a)
    Tanh    :: (Type a, Floating a) => FLOATING (a :-> Full a)
    Cosh    :: (Type a, Floating a) => FLOATING (a :-> Full a)
    Asinh   :: (Type a, Floating a) => FLOATING (a :-> Full a)
    Atanh   :: (Type a, Floating a) => FLOATING (a :-> Full a)
    Acosh   :: (Type a, Floating a) => FLOATING (a :-> Full a)

instance Semantic FLOATING
  where
    semantics Pi      = Sem "pi"      Prelude.pi
    semantics Exp     = Sem "exp"     Prelude.exp
    semantics Sqrt    = Sem "sqrt"    Prelude.sqrt
    semantics Log     = Sem "log"     Prelude.log
    semantics Pow     = Sem "(**)"    (Prelude.**)
    semantics LogBase = Sem "logBase" Prelude.logBase
    semantics Sin     = Sem "sin"     Prelude.sin
    semantics Tan     = Sem "tan"     Prelude.tan
    semantics Cos     = Sem "cos"     Prelude.cos
    semantics Asin    = Sem "asin"    Prelude.asin
    semantics Atan    = Sem "atan"    Prelude.atan
    semantics Acos    = Sem "acos"    Prelude.acos
    semantics Sinh    = Sem "sinh"    Prelude.sinh
    semantics Tanh    = Sem "tanh"    Prelude.tanh
    semantics Cosh    = Sem "cosh"    Prelude.cosh
    semantics Asinh   = Sem "asinh"   Prelude.asinh
    semantics Atanh   = Sem "atanh"   Prelude.atanh
    semantics Acosh   = Sem "acosh"   Prelude.acosh

instance Equality FLOATING where equal = equalDefault; exprHash = exprHashDefault
instance Render   FLOATING where renderArgs = renderArgsDefault
instance ToTree   FLOATING
instance Eval     FLOATING where evaluate = evaluateDefault
instance EvalBind FLOATING where evalBindSym = evalBindSymDefault
--instance SizeProp FLOATING where sizeProp = sizePropDefault
instance Sharable FLOATING

instance AlphaEq dom dom dom env => AlphaEq FLOATING FLOATING dom env
  where
    alphaEqSym = alphaEqSymDefault

{-
instance (FLOATING :<: dom, Optimize dom dom) => Optimize FLOATING dom
  where
    constructFeatUnOpt = constructFeatUnOptDefault
-}

