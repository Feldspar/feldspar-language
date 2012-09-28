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

module Feldspar.Core.Constructs.Eq
where

import Language.Syntactic
import Language.Syntactic.Constructs.Binding

import Feldspar.Range
import Feldspar.Core.Types
import Feldspar.Core.Interpretation

data EQ a
  where
    Equal    :: (Type a, Eq a) => EQ (a :-> a :-> Full Bool)
    NotEqual :: (Type a, Eq a) => EQ (a :-> a :-> Full Bool)

instance Semantic EQ
  where
    semantics Equal    = Sem "(==)" (==)
    semantics NotEqual = Sem "(/=)" (/=)

instance Equality EQ where equal = equalDefault; exprHash = exprHashDefault
instance Render   EQ where renderArgs = renderArgsDefault
instance ToTree   EQ
instance Eval     EQ where evaluate = evaluateDefault
instance EvalBind EQ where evalBindSym = evalBindSymDefault
instance Sharable EQ

instance AlphaEq dom dom dom env => AlphaEq EQ EQ dom env
  where
    alphaEqSym = alphaEqSymDefault

instance SizeProp (EQ :||| Type)
  where sizeProp a@(C'' _) = sizePropDefault a

instance ((EQ :||| Type) :<: dom, OptimizeSuper dom) => Optimize (EQ :||| Type) dom
  where
    constructFeatOpt (C'' Equal) (a :* b :* Nil)
        | alphaEq a b
        = return $ literalDecor True

    constructFeatOpt (C'' Equal) (a :* b :* Nil)
        | RangeSet ra <- infoRange (getInfo a)
        , RangeSet rb <- infoRange (getInfo b)
        , ra `disjoint` rb
        = return $ literalDecor False

    constructFeatOpt (C'' NotEqual) (a :* b :* Nil)
        | alphaEq a b
        = return $ literalDecor False

    constructFeatOpt (C'' NotEqual) (a :* b :* Nil)
        | RangeSet ra <- infoRange (getInfo a)
        , RangeSet rb <- infoRange (getInfo b)
        , ra `disjoint` rb
        = return $ literalDecor True

    constructFeatOpt a args = constructFeatUnOpt a args

    constructFeatUnOpt x@(C'' _) = constructFeatUnOptDefault x

