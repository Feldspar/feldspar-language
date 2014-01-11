{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
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

module Feldspar.Core.Constructs.Complex
where

import Language.Syntactic
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

semanticInstances ''COMPLEX

instance EvalBind COMPLEX where evalBindSym = evalBindSymDefault

instance AlphaEq dom dom dom env => AlphaEq COMPLEX COMPLEX dom env
  where
    alphaEqSym = alphaEqSymDefault

instance Sharable COMPLEX

instance SizeProp (COMPLEX :|| Type)
  where
    sizeProp (C' s) = sizePropDefault s

instance ( (COMPLEX :|| Type) :<: dom
         , OptimizeSuper dom)
      => Optimize (COMPLEX :|| Type) dom
  where
    constructFeatOpt _ (C' MkComplex) ((rp :$ a) :* (ip :$ b) :* Nil)
        | Just (C' RealPart) <- prjF rp
        , Just (C' ImagPart) <- prjF ip
        , alphaEq a b
        = return a

    constructFeatOpt _ (C' RealPart) ((mkc :$ r :$ _) :* Nil)
        | Just (C' MkComplex) <- prjF mkc
        = return r

    constructFeatOpt _ (C' ImagPart) ((mkc :$ _ :$ i) :* Nil)
        | Just (C' MkComplex) <- prjF mkc
        = return i

    constructFeatOpt _ (C' MkPolar) ((mag :$ a) :* (ph :$ b) :* Nil)
        | Just (C' Magnitude) <- prjF mag
        , Just (C' Phase)     <- prjF ph
        , alphaEq a b
        = return a

    constructFeatOpt _ (C' Magnitude) ((mkp :$ m :$ _) :* Nil)
        | Just (C' MkPolar) <- prjF mkp
        = return m

    constructFeatOpt _ (C' Phase) ((mkp :$ _ :$ p) :* Nil)
        | Just (C' MkPolar) <- prjF mkp
        = return p

    constructFeatOpt _ (C' Conjugate) ((conj :$ a) :* Nil)
        | Just (C' Conjugate) <- prjF conj
        = return a

    constructFeatOpt opts sym args = constructFeatUnOpt opts sym args

    constructFeatUnOpt opts x@(C' _) = constructFeatUnOptDefault opts x

