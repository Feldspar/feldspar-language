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

module Feldspar.Core.Constructs.Ord
    ( ORD (..)
    ) where



import Language.Syntactic
import Language.Syntactic.Interpretation.Semantics
import Language.Syntactic.Constructs.Binding

import Feldspar.Range
import Feldspar.Core.Types
import Feldspar.Core.Interpretation



data ORD a
  where
    LTH :: (Type a, Ord a, Ord (Size a)) => ORD (a :-> a :-> Full Bool)
    GTH :: (Type a, Ord a, Ord (Size a)) => ORD (a :-> a :-> Full Bool)
    LTE :: (Type a, Ord a, Ord (Size a)) => ORD (a :-> a :-> Full Bool)
    GTE :: (Type a, Ord a, Ord (Size a)) => ORD (a :-> a :-> Full Bool)
    Min :: (Type a, Ord a, Ord (Size a)) => ORD (a :-> a :-> Full a)
    Max :: (Type a, Ord a, Ord (Size a)) => ORD (a :-> a :-> Full a)

instance WitnessCons ORD
  where
    witnessCons LTH  = ConsWit
    witnessCons GTH = ConsWit
    witnessCons LTE = ConsWit
    witnessCons GTE = ConsWit
    witnessCons Min = ConsWit
    witnessCons Max = ConsWit

instance WitnessSat ORD
  where
    type SatContext ORD = TypeCtx
    witnessSat LTH  = SatWit
    witnessSat GTH = SatWit
    witnessSat LTE = SatWit
    witnessSat GTE = SatWit
    witnessSat Min = SatWit
    witnessSat Max = SatWit

instance MaybeWitnessSat TypeCtx ORD
  where
    maybeWitnessSat = maybeWitnessSatDefault

instance Semantic ORD
  where
    semantics LTH = Sem "(<)"  (<)
    semantics GTH = Sem "(>)"  (>)
    semantics LTE = Sem "(<=)" (<=)
    semantics GTE = Sem "(>=)" (>=)
    semantics Min = Sem "min"  min
    semantics Max = Sem "max"  max

instance ExprEq   ORD where exprEq = exprEqSem; exprHash = exprHashSem
instance Render   ORD where renderPart = renderPartSem
instance ToTree   ORD
instance Eval     ORD where evaluate = evaluateSem
instance EvalBind ORD where evalBindSym = evalBindSymDefault
instance Sharable ORD

instance AlphaEq dom dom dom env => AlphaEq ORD ORD dom env
  where
    alphaEqSym = alphaEqSymDefault

instance SizeProp ORD
  where
    sizeProp Min (WrapFull a :* WrapFull b :* Nil) = min (infoSize a) (infoSize b)
    sizeProp Max (WrapFull a :* WrapFull b :* Nil) = max (infoSize a) (infoSize b)
    sizeProp a args = sizePropDefault a args



instance
    ( ORD :<: dom
    , MaybeWitnessSat TypeCtx dom
    , OptimizeSuper dom
    ) =>
      Optimize ORD dom
  where
    constructFeatOpt LTH (a :* b :* Nil)
        | RangeSet ra <- infoRange (getInfo a)
        , RangeSet rb <- infoRange (getInfo b)
        , ra `rangeLess` rb
        = return (literalDecor True)

    constructFeatOpt LTH (a :* b :* Nil)
        | RangeSet ra <- infoRange (getInfo a)
        , RangeSet rb <- infoRange (getInfo b)
        , rb `rangeLessEq` ra
        = return (literalDecor False)

    constructFeatOpt GTH (a :* b :* Nil)
        | RangeSet ra <- infoRange (getInfo a)
        , RangeSet rb <- infoRange (getInfo b)
        , rb `rangeLess` ra
        = return (literalDecor True)

    constructFeatOpt GTH (a :* b :* Nil)
        | RangeSet ra <- infoRange (getInfo a)
        , RangeSet rb <- infoRange (getInfo b)
        , ra `rangeLessEq` rb
        = return (literalDecor False)

    constructFeatOpt LTE (a :* b :* Nil)
        | RangeSet ra <- infoRange (getInfo a)
        , RangeSet rb <- infoRange (getInfo b)
        , ra `rangeLessEq` rb
        = return (literalDecor True)

    constructFeatOpt LTE (a :* b :* Nil)
        | RangeSet ra <- infoRange (getInfo a)
        , RangeSet rb <- infoRange (getInfo b)
        , rb `rangeLess` ra
        = return (literalDecor False)

    constructFeatOpt LTE (a :* b :* Nil)
        | alphaEq a b
        = return $ literalDecor True

    constructFeatOpt GTE (a :* b :* Nil)
        | RangeSet ra <- infoRange (getInfo a)
        , RangeSet rb <- infoRange (getInfo b)
        , rb `rangeLessEq` ra
        = return (literalDecor True)

    constructFeatOpt GTE (a :* b :* Nil)
        | RangeSet ra <- infoRange (getInfo a)
        , RangeSet rb <- infoRange (getInfo b)
        , ra `rangeLess` rb
        = return (literalDecor False)

    constructFeatOpt GTE (a :* b :* Nil)
        | alphaEq a b
        = return $ literalDecor True

    constructFeatOpt Min (a :* b :* Nil)
        | RangeSet ra <- infoRange (getInfo a)
        , RangeSet rb <- infoRange (getInfo b)
        , ra `rangeLessEq` rb
        = return a

    constructFeatOpt Min (a :* b :* Nil)
        | RangeSet ra <- infoRange (getInfo a)
        , RangeSet rb <- infoRange (getInfo b)
        , rb `rangeLessEq` ra
        = return b

    constructFeatOpt Min (a :* b :* Nil)
        | alphaEq a b
        = return a

    constructFeatOpt Max (a :* b :* Nil)
        | RangeSet ra <- infoRange (getInfo a)
        , RangeSet rb <- infoRange (getInfo b)
        , ra `rangeLessEq` rb
        = return b

    constructFeatOpt Max (a :* b :* Nil)
        | RangeSet ra <- infoRange (getInfo a)
        , RangeSet rb <- infoRange (getInfo b)
        , rb `rangeLessEq` ra
        = return a

    constructFeatOpt Max (a :* b :* Nil)
        | alphaEq a b
        = return a

    constructFeatOpt a args = constructFeatUnOpt a args

    constructFeatUnOpt = constructFeatUnOptDefault

