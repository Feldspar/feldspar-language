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

module Feldspar.Core.Constructs.Complex
where

import Language.Syntactic
import Language.Syntactic.Interpretation.Semantics
import Language.Syntactic.Constructs.Binding

import Data.Complex

import Feldspar.Core.Types
import Feldspar.Core.Interpretation

data COMPLEX a
  where
    MkComplex :: (Type a, RealFloat a) => COMPLEX (a :-> a :-> Full (Complex a))
    RealPart  :: (Type a, RealFloat a) => COMPLEX (Complex a :-> Full a)
    ImagPart  :: (Type a, RealFloat a) => COMPLEX (Complex a :-> Full a)
    Conjugate :: (Type a, RealFloat a) => COMPLEX (Complex a :-> Full (Complex a))
    MkPolar   :: (Type a, RealFloat a) => COMPLEX (a :-> a :-> Full (Complex a))
    Magnitude :: (Type a, RealFloat a) => COMPLEX (Complex a :-> Full a)
    Phase     :: (Type a, RealFloat a) => COMPLEX (Complex a :-> Full a)
    Cis       :: (Type a, RealFloat a) => COMPLEX (a :-> Full (Complex a))

instance WitnessCons COMPLEX
  where
    witnessCons MkComplex = ConsWit
    witnessCons RealPart  = ConsWit
    witnessCons ImagPart  = ConsWit
    witnessCons Conjugate = ConsWit
    witnessCons MkPolar   = ConsWit
    witnessCons Magnitude = ConsWit
    witnessCons Phase     = ConsWit
    witnessCons Cis       = ConsWit

instance WitnessSat COMPLEX
  where
    type SatContext COMPLEX = TypeCtx
    witnessSat MkComplex = SatWit
    witnessSat RealPart  = SatWit
    witnessSat ImagPart  = SatWit
    witnessSat Conjugate = SatWit
    witnessSat MkPolar   = SatWit
    witnessSat Magnitude = SatWit
    witnessSat Phase     = SatWit
    witnessSat Cis       = SatWit

instance MaybeWitnessSat TypeCtx COMPLEX
  where
    maybeWitnessSat = maybeWitnessSatDefault

instance Semantic COMPLEX
  where
    semantics MkComplex = Sem "complex"   (:+)
    semantics RealPart  = Sem "creal"     realPart
    semantics ImagPart  = Sem "cimag"     imagPart
    semantics Conjugate = Sem "conjugate" conjugate
    semantics MkPolar   = Sem "mkPolar"   mkPolar
    semantics Magnitude = Sem "magnitude" magnitude
    semantics Phase     = Sem "phase"     phase
    semantics Cis       = Sem "cis"       cis

instance ExprEq   COMPLEX where exprEq = exprEqSem; exprHash = exprHashSem
instance Render   COMPLEX where renderPart = renderPartSem
instance ToTree   COMPLEX
instance Eval     COMPLEX where evaluate = evaluateSem
instance EvalBind COMPLEX where evalBindSym = evalBindSymDefault
instance Sharable COMPLEX
instance SizeProp COMPLEX where sizeProp = sizePropDefault

instance AlphaEq dom dom dom env => AlphaEq COMPLEX COMPLEX dom env
  where
    alphaEqSym = alphaEqSymDefault

instance (COMPLEX :<: dom, OptimizeSuper dom) => Optimize COMPLEX dom
  where
    constructFeatOpt MkComplex ((rp :$ a) :* (ip :$ b) :* Nil)
        | Just (_,RealPart) <- prjDecor rp
        , Just (_,ImagPart) <- prjDecor ip
        , alphaEq a b
        = return a

    constructFeatOpt RealPart ((mkc :$ r :$ _) :* Nil)
        | Just (_,MkComplex) <- prjDecor mkc
        = return r

    constructFeatOpt ImagPart ((mkc :$ _ :$ i) :* Nil)
        | Just (_,MkComplex) <- prjDecor mkc
        = return i

    constructFeatOpt MkPolar ((mag :$ a) :* (ph :$ b) :* Nil)
        | Just (_,Magnitude) <- prjDecor mag
        , Just (_,Phase)     <- prjDecor ph
        , alphaEq a b
        = return a

    constructFeatOpt Magnitude ((mkp :$ m :$ p) :* Nil)
        | Just (_,MkPolar) <- prjDecor mkp
        = return m

    constructFeatOpt Phase ((mkp :$ m :$ p) :* Nil)
        | Just (_,MkPolar) <- prjDecor mkp
        = return p

    constructFeatOpt Conjugate ((conj :$ a) :* Nil)
        | Just (_,Conjugate) <- prjDecor conj
        = return a

    constructFeatOpt sym args = constructFeatUnOpt sym args

    constructFeatUnOpt = constructFeatUnOptDefault

