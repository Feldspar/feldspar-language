{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
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

-- | Implementation of ordering constructs

module Feldspar.Core.Constructs.Ord
    ( ORD (..)
    ) where



import Language.Syntactic
import Language.Syntactic.Constructs.Binding

import Feldspar.Range
import Feldspar.Core.Types
import Feldspar.Core.Interpretation


-- | Ordering constructs
data ORD a
  where
    LTH :: (Type a, Ord a, Ord (Size a)) => ORD (a :-> a :-> Full Bool)
    GTH :: (Type a, Ord a, Ord (Size a)) => ORD (a :-> a :-> Full Bool)
    LTE :: (Type a, Ord a, Ord (Size a)) => ORD (a :-> a :-> Full Bool)
    GTE :: (Type a, Ord a, Ord (Size a)) => ORD (a :-> a :-> Full Bool)
    Min :: (Type a, Ord a, Ord (Size a)) => ORD (a :-> a :-> Full a)
    Max :: (Type a, Ord a, Ord (Size a)) => ORD (a :-> a :-> Full a)

instance Semantic ORD
  where
    semantics LTH = Sem "(<)"  (<)
    semantics GTH = Sem "(>)"  (>)
    semantics LTE = Sem "(<=)" (<=)
    semantics GTE = Sem "(>=)" (>=)
    semantics Min = Sem "min"  min
    semantics Max = Sem "max"  max

instance Equality ORD where equal = equalDefault; exprHash = exprHashDefault
instance Render   ORD where renderArgs = renderArgsDefault
instance ToTree   ORD
instance Eval     ORD where evaluate = evaluateDefault
instance EvalBind ORD where evalBindSym = evalBindSymDefault
instance Sharable ORD

instance AlphaEq dom dom dom env => AlphaEq ORD ORD dom env
  where
    alphaEqSym = alphaEqSymDefault

instance SizeProp (ORD :|| Type)
  where
    sizeProp (C' Min) (WrapFull a :* WrapFull b :* Nil) = min (infoSize a) (infoSize b)
    sizeProp (C' Max) (WrapFull a :* WrapFull b :* Nil) = max (infoSize a) (infoSize b)
    sizeProp a@(C' _) args = sizePropDefault a args


instance ( (ORD :|| Type) :<: dom
         , OptimizeSuper dom
         )
      => Optimize (ORD :|| Type) dom
  where
    constructFeatOpt (C' LTH) (a :* b :* Nil)
        | RangeSet ra <- infoRange (getInfo a)
        , RangeSet rb <- infoRange (getInfo b)
        , ra `rangeLess` rb
        = return (literalDecor True)

    constructFeatOpt (C' LTH) (a :* b :* Nil)
        | RangeSet ra <- infoRange (getInfo a)
        , RangeSet rb <- infoRange (getInfo b)
        , rb `rangeLessEq` ra
        = return (literalDecor False)

    constructFeatOpt (C' GTH) (a :* b :* Nil)
        | RangeSet ra <- infoRange (getInfo a)
        , RangeSet rb <- infoRange (getInfo b)
        , rb `rangeLess` ra
        = return (literalDecor True)

    constructFeatOpt (C' GTH) (a :* b :* Nil)
        | RangeSet ra <- infoRange (getInfo a)
        , RangeSet rb <- infoRange (getInfo b)
        , ra `rangeLessEq` rb
        = return (literalDecor False)

    constructFeatOpt (C' LTE) (a :* b :* Nil)
        | RangeSet ra <- infoRange (getInfo a)
        , RangeSet rb <- infoRange (getInfo b)
        , ra `rangeLessEq` rb
        = return (literalDecor True)

    constructFeatOpt (C' LTE) (a :* b :* Nil)
        | RangeSet ra <- infoRange (getInfo a)
        , RangeSet rb <- infoRange (getInfo b)
        , rb `rangeLess` ra
        = return (literalDecor False)

    constructFeatOpt (C' LTE) (a :* b :* Nil)
        | alphaEq a b
        = return $ literalDecor True

    constructFeatOpt (C' GTE) (a :* b :* Nil)
        | RangeSet ra <- infoRange (getInfo a)
        , RangeSet rb <- infoRange (getInfo b)
        , rb `rangeLessEq` ra
        = return (literalDecor True)

    constructFeatOpt (C' GTE) (a :* b :* Nil)
        | RangeSet ra <- infoRange (getInfo a)
        , RangeSet rb <- infoRange (getInfo b)
        , ra `rangeLess` rb
        = return (literalDecor False)

    constructFeatOpt (C' GTE) (a :* b :* Nil)
        | alphaEq a b
        = return $ literalDecor True

    constructFeatOpt (C' Min) (a :* b :* Nil)
        | RangeSet ra <- infoRange (getInfo a)
        , RangeSet rb <- infoRange (getInfo b)
        , ra `rangeLessEq` rb
        = return a

    constructFeatOpt (C' Min) (a :* b :* Nil)
        | RangeSet ra <- infoRange (getInfo a)
        , RangeSet rb <- infoRange (getInfo b)
        , rb `rangeLessEq` ra
        = return b

    constructFeatOpt (C' Min) (a :* b :* Nil)
        | alphaEq a b
        = return a

    constructFeatOpt s@(C' Min) (a :* (op :$ b :$ c) :* Nil)
        | Just (C' Min) <- prjF op
        , alphaEq a b = constructFeat s (a :* c :* Nil)
        | Just (C' Min) <- prjF op
        , alphaEq a c = constructFeat s (a :* b :* Nil)

    constructFeatOpt s@(C' Min) ((op :$ b :$ c) :* a :* Nil)
        | Just (C' Min) <- prjF op
        , alphaEq a b = constructFeat s (c :* a :* Nil)
        | Just (C' Min) <- prjF op
        , alphaEq a c = constructFeat s (b :* a :* Nil)

    constructFeatOpt (C' Max) (a :* b :* Nil)
        | RangeSet ra <- infoRange (getInfo a)
        , RangeSet rb <- infoRange (getInfo b)
        , ra `rangeLessEq` rb
        = return b

    constructFeatOpt (C' Max) (a :* b :* Nil)
        | RangeSet ra <- infoRange (getInfo a)
        , RangeSet rb <- infoRange (getInfo b)
        , rb `rangeLessEq` ra
        = return a

    constructFeatOpt (C' Max) (a :* b :* Nil)
        | alphaEq a b
        = return a

    constructFeatOpt s@(C' Max) (a :* (op :$ b :$ c) :* Nil)
        | Just (C' Max) <- prjF op
        , alphaEq a b = constructFeat s (a :* c :* Nil)
        | Just (C' Max) <- prjF op
        , alphaEq a c = constructFeat s (a :* b :* Nil)

    constructFeatOpt s@(C' Max) ((op :$ b :$ c) :* a :* Nil)
        | Just (C' Max) <- prjF op
        , alphaEq a b = constructFeat s (c :* a :* Nil)
        | Just (C' Max) <- prjF op
        , alphaEq a c = constructFeat s (b :* a :* Nil)

    constructFeatOpt a args = constructFeatUnOpt a args

    constructFeatUnOpt x@(C' _) = constructFeatUnOptDefault x

