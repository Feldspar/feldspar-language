{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
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

instance Semantic Trace
  where
    semantics Trace = Sem "trace" (\i a -> trace (show i ++ ":" ++ show a) a)

instance Constrained Trace
    where
      type Sat Trace = Type
      exprDict Trace = Dict

instance Equality Trace where equal = equalDefault; exprHash = exprHashDefault
instance Render   Trace where renderArgs = renderArgsDefault
instance ToTree   Trace
instance Eval     Trace where evaluate = evaluateDefault
instance EvalBind Trace where evalBindSym = evalBindSymDefault
instance Sharable Trace

instance AlphaEq dom dom dom env => AlphaEq Trace Trace dom env
  where
    alphaEqSym = alphaEqSymDefault

instance SizeProp (Trace :|| Type)
  where
    sizeProp (C' Trace) (WrapFull _ :* WrapFull a :* Nil) = infoSize a

instance ( (Trace :|| Type) :<: dom
         , OptimizeSuper dom)
      => Optimize (Trace :|| Type) dom
  where
    constructFeatUnOpt opts x@(C' _) = constructFeatUnOptDefault opts x

