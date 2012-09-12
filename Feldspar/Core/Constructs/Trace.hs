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

module Feldspar.Core.Constructs.Trace
    ( Trace (..)
    ) where



import Language.Syntactic
import Language.Syntactic.Constructs.Binding

import Feldspar.Core.Types
import Feldspar.Core.Interpretation

import Debug.Trace

data Trace a
  where
    Trace :: Type a => Trace (IntN :-> a :-> Full a)
  -- TODO Seems a more suitable definition might be
  --
  --          Trace :: Type a => IntN -> Trace (a :-> Full a)
  --
  --      since the front-end function will always make a literal for the label.

instance WitnessCons Trace
  where
    witnessCons Trace = ConsWit

instance WitnessSat Trace
  where
    type SatContext Trace = TypeCtx
    witnessSat Trace = SatWit

instance MaybeWitnessSat TypeCtx Trace
  where
    maybeWitnessSat = maybeWitnessSatDefault

instance Semantic Trace
  where
    semantics Trace = Sem "trace" (\i a -> trace (show i ++ ":" ++ show a) a)

instance ExprEq   Trace where exprEq = exprEqSem; exprHash = exprHashSem
instance Render   Trace where renderPart = renderPartSem
instance ToTree   Trace
instance Eval     Trace where evaluate = evaluateSem
instance EvalBind Trace where evalBindSym = evalBindSymDefault
instance Sharable Trace

instance AlphaEq dom dom dom env => AlphaEq Trace Trace dom env
  where
    alphaEqSym = alphaEqSymDefault

instance SizeProp Trace
  where
    sizeProp Trace (WrapFull _ :* WrapFull a :* Nil) = infoSize a

instance (Trace :<: dom, Optimize dom dom) => Optimize Trace dom
  where
    constructFeatUnOpt = constructFeatUnOptDefault

