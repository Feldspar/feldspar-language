{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
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

{-# LANGUAGE UndecidableInstances #-}

module Feldspar.Core.Constructs.Fractional
    ( FRACTIONAL (..)
    ) where



import Language.Syntactic
import Language.Syntactic.Constructs.Binding

import Feldspar.Core.Types
import Feldspar.Core.Interpretation
import Feldspar.Core.Constructs.Num

data FRACTIONAL a
  where
    DivFrac :: (Type a, Fractional a) => FRACTIONAL (a :-> a :-> Full a)

instance WitnessCons FRACTIONAL
  where
    witnessCons DivFrac = ConsWit

instance WitnessSat FRACTIONAL
  where
    type SatContext FRACTIONAL = TypeCtx
    witnessSat DivFrac = SatWit

instance MaybeWitnessSat TypeCtx FRACTIONAL
  where
    maybeWitnessSat = maybeWitnessSatDefault

instance Semantic FRACTIONAL
  where
    semantics DivFrac = Sem "(/)" (/)

instance ExprEq   FRACTIONAL where exprEq = exprEqSem; exprHash = exprHashSem
instance Render   FRACTIONAL where renderPart = renderPartSem
instance ToTree   FRACTIONAL
instance Eval     FRACTIONAL where evaluate = evaluateSem
instance EvalBind FRACTIONAL where evalBindSym = evalBindSymDefault
instance SizeProp FRACTIONAL where sizeProp = sizePropDefault
instance Sharable FRACTIONAL

instance AlphaEq dom dom dom env => AlphaEq FRACTIONAL FRACTIONAL dom env
  where
    alphaEqSym = alphaEqSymDefault

instance (FRACTIONAL :<: dom, NUM :<: dom, OptimizeSuper dom) => Optimize FRACTIONAL dom
  where
    constructFeatOpt DivFrac (a :* b :* Nil)
        | Just 1 <- viewLiteral b = return a
        | alphaEq a b = return $ literalDecor 1

    constructFeatOpt DivFrac ((op :$ a :$ b) :* c :* Nil)
        | Just (_,Mul) <- prjDecor op
        , alphaEq b c
        = return a

    constructFeatOpt DivFrac ((op :$ a :$ b) :* c :* Nil)
        | Just (_,Mul) <- prjDecor op
        , alphaEq a c
        = return b

    constructFeatOpt a args = constructFeatUnOpt a args

    constructFeatUnOpt = constructFeatUnOptDefault

