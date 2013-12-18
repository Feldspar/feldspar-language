{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
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

module Feldspar.Core.Constructs.Future where

import Language.Syntactic

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

semanticInstances ''FUTURE

instance EvalBind FUTURE where evalBindSym = evalBindSymDefault

instance AlphaEq dom dom dom env => AlphaEq FUTURE FUTURE dom env
  where
    alphaEqSym = alphaEqSymDefault

instance Sharable FUTURE
  where
    hoistOver MkFuture = False
    hoistOver _        = True

instance SizeProp (FUTURE :|| Type)
  where
    sizeProp (C' MkFuture) (WrapFull a :* Nil) = infoSize a
    sizeProp (C' Await)    (WrapFull a :* Nil) = infoSize a

instance ( (FUTURE :|| Type) :<: dom
         , OptimizeSuper dom
         )
      => Optimize (FUTURE :|| Type) dom
  where
    constructFeatOpt _ (C' Await) ((op :$ a) :* Nil)
      | Just (C' MkFuture) <- prjF op
      = return a

    constructFeatOpt _ (C' MkFuture) ((op :$ a) :* Nil)
      | Just (C' Await) <- prjF op
      = return a

    constructFeatOpt opts feature args = constructFeatUnOpt opts feature args

    constructFeatUnOpt opts x@(C' _) = constructFeatUnOptDefault opts x

