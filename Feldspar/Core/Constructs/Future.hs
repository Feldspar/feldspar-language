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

module Feldspar.Core.Constructs.Future where

import Language.Syntactic
import Language.Syntactic.Interpretation.Semantics


import Feldspar.Core.Types
import Feldspar.Core.Interpretation
import Feldspar.Core.Constructs.Binding

data FUTURE a where
  MkFuture :: Type a => FUTURE (a :-> Full (FVal a))
  Await    :: Type a => FUTURE (FVal a :-> Full a)

instance Semantic FUTURE
  where
    semantics MkFuture = Sem "future" FVal
    semantics Await    = Sem "await"  unFVal

instance ExprEq   FUTURE where exprEq = exprEqSem; exprHash = exprHashSem
instance Render   FUTURE where renderPart = renderPartSem
instance ToTree   FUTURE
instance Eval     FUTURE where evaluate = evaluateSem
instance EvalBind FUTURE where evalBindSym = evalBindSymDefault
instance Sharable FUTURE

instance WitnessCons FUTURE
  where
    witnessCons MkFuture = ConsWit
    witnessCons Await    = ConsWit

instance WitnessSat FUTURE
  where
    type SatContext FUTURE = TypeCtx
    witnessSat MkFuture = SatWit
    witnessSat Await    = SatWit

instance MaybeWitnessSat TypeCtx FUTURE
  where
    maybeWitnessSat _ Await = Nothing
    maybeWitnessSat a b     = maybeWitnessSatDefault a b

instance AlphaEq dom dom dom env => AlphaEq FUTURE FUTURE dom env
  where
    alphaEqSym = alphaEqSymDefault

instance SizeProp FUTURE
  where
    sizeProp MkFuture (WrapFull a :* Nil) = infoSize a
    sizeProp Await    (WrapFull a :* Nil) = infoSize a

instance ( FUTURE :<: dom
         , Optimize dom dom
         )
      => Optimize FUTURE dom
  where
    constructFeatOpt Await ((op :$ a) :* Nil)
      | Just (_,MkFuture) <- prjDecor op
      = return a

    constructFeatOpt MkFuture ((op :$ a) :* Nil)
      | Just (_,Await) <- prjDecor op
      = return a

    constructFeatOpt feature args = constructFeatUnOpt feature args

    constructFeatUnOpt MkFuture args = constructFeatUnOptDefault MkFuture args
    constructFeatUnOpt Await    args = constructFeatUnOptDefault Await args

