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

module Feldspar.Core.Constructs.Error where

import Language.Syntactic
import Language.Syntactic.Constructs.Binding

import Feldspar.Core.Types
import Feldspar.Core.Interpretation

data Error a
  where
    Undefined :: Type a => Error (Full a)
    Assert    :: Type a => String -> Error (Bool :-> a :-> Full a)

instance Semantic Error
  where
    semantics Undefined    = Sem "undefined" undefined
    semantics (Assert msg) = Sem "assert"
        (\cond a -> if cond then a else error ("Assert failed: " ++ msg))

instance Render Error
  where
    renderSym Undefined    = "undefined"
    renderSym (Assert msg) = "assert " ++ show msg

instance Equality   Error where equal = equalDefault; exprHash = exprHashDefault
instance StringTree Error
instance Eval       Error where evaluate = evaluateDefault
instance EvalBind   Error where evalBindSym = evalBindSymDefault
instance Sharable   Error

instance SizeProp (Error :|| Type)
  where
    sizeProp a@(C' _) = sizePropDefault a

instance AlphaEq dom dom dom env => AlphaEq Error Error dom env
  where
    alphaEqSym = alphaEqSymDefault

instance ((Error :|| Type) :<: dom, Optimize dom dom)
    => Optimize (Error :|| Type) dom
  where
    constructFeatUnOpt opts x@(C' _) = constructFeatUnOptDefault opts x

