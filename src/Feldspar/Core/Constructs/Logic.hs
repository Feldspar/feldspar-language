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

module Feldspar.Core.Constructs.Logic
    ( Logic (..)
    ) where



import Language.Syntactic
import Language.Syntactic.Constructs.Binding

import Feldspar.Core.Types
import Feldspar.Core.Interpretation
import Feldspar.Core.Constructs.Eq
import Feldspar.Core.Constructs.Ord


data Logic a
  where
    And :: Logic (Bool :-> Bool :-> Full Bool)
    Or  :: Logic (Bool :-> Bool :-> Full Bool)
    Not :: Logic (Bool :->          Full Bool)

instance Semantic Logic
  where
    semantics And = Sem "(&&)" (&&)
    semantics Or  = Sem "(||)" (||)
    semantics Not = Sem "not"  not

instance Equality Logic where equal = equalDefault; exprHash = exprHashDefault
instance Render   Logic where renderArgs = renderArgsDefault
instance ToTree   Logic
instance Eval     Logic where evaluate = evaluateDefault
instance EvalBind Logic where evalBindSym = evalBindSymDefault
--instance SizeProp Logic where sizeProp = sizePropDefault
instance Sharable Logic

instance AlphaEq dom dom dom env => AlphaEq Logic Logic dom env
  where
    alphaEqSym = alphaEqSymDefault

instance SizeProp (Logic :|| Type)
  where
    sizeProp a@(C' _) args = sizePropDefault a args

instance ( (Logic :|| Type) :<: dom
         , (EQ    :|| Type) :<: dom
         , (ORD   :|| Type) :<: dom
         , OptimizeSuper dom
         )
      => Optimize (Logic :|| Type) dom
  where
    constructFeatOpt (C' And) (a :* b :* Nil)
        | Just True  <- viewLiteral a = return b
        | Just False <- viewLiteral a = return a
        | Just True  <- viewLiteral b = return a
        | Just False <- viewLiteral b = return b
        | a `alphaEq` b               = return a

    constructFeatOpt (C' Or) (a :* b :* Nil)
        | Just True  <- viewLiteral a = return a
        | Just False <- viewLiteral a = return b
        | Just True  <- viewLiteral b = return b
        | Just False <- viewLiteral b = return a
        | a `alphaEq` b               = return a

    constructFeatOpt (C' Not) ((op :$ a) :* Nil)
        | Just (C' Not) <- prjC op = return a

    constructFeatOpt (C' Not) ((op :$ a :$ b) :* Nil)
        | Just (C' Equal)    <- prjC op = constructFeat (c' NotEqual) (a :* b :* Nil)
        | Just (C' NotEqual) <- prjC op = constructFeat (c' Equal)    (a :* b :* Nil)
        | Just (C' LTH)      <- prjC op = constructFeat (c' GTE)      (a :* b :* Nil)
        | Just (C' GTH)      <- prjC op = constructFeat (c' LTE)      (a :* b :* Nil)
        | Just (C' LTE)      <- prjC op = constructFeat (c' GTH)      (a :* b :* Nil)
        | Just (C' GTE)      <- prjC op = constructFeat (c' LTH)      (a :* b :* Nil)

    constructFeatOpt a args = constructFeatUnOpt a args

    constructFeatUnOpt x@(C' _) = constructFeatUnOptDefault x

