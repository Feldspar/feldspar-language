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

module Feldspar.Core.Constructs.Num
    ( NUM (..)
    ) where



import Language.Syntactic
import Language.Syntactic.Interpretation.Semantics
import Language.Syntactic.Constructs.Binding

import Feldspar.Range
import Feldspar.Core.Types
import Feldspar.Core.Interpretation



data NUM a
  where
    Abs  :: (Type a, Num a, Num (Size a)) => NUM (a :-> Full a)
    Sign :: (Type a, Num a, Num (Size a)) => NUM (a :-> Full a)
    Add  :: (Type a, Num a, Num (Size a)) => NUM (a :-> a :-> Full a)
    Sub  :: (Type a, Num a, Num (Size a)) => NUM (a :-> a :-> Full a)
    Mul  :: (Type a, Num a, Num (Size a)) => NUM (a :-> a :-> Full a)

instance WitnessCons NUM
  where
    witnessCons Abs  = ConsWit
    witnessCons Sign = ConsWit
    witnessCons Add  = ConsWit
    witnessCons Sub  = ConsWit
    witnessCons Mul  = ConsWit

instance WitnessSat NUM
  where
    type SatContext NUM = TypeCtx
    witnessSat Abs  = SatWit
    witnessSat Sign = SatWit
    witnessSat Add  = SatWit
    witnessSat Sub  = SatWit
    witnessSat Mul  = SatWit

instance MaybeWitnessSat TypeCtx NUM
  where
    maybeWitnessSat = maybeWitnessSatDefault

instance Semantic NUM
  where
    semantics Abs  = Sem "abs" abs
    semantics Sign = Sem "signum" signum
    semantics Add  = Sem "(+)" (+)
    semantics Sub  = Sem "(-)" (-)
    semantics Mul  = Sem "(*)" (*)

instance ExprEq   NUM where exprEq = exprEqSem; exprHash = exprHashSem
instance Render   NUM where renderPart = renderPartSem
instance ToTree   NUM
instance Eval     NUM where evaluate = evaluateSem
instance EvalBind NUM where evalBindSym = evalBindSymDefault
instance Sharable NUM

instance AlphaEq dom dom dom env => AlphaEq NUM NUM dom env
  where
    alphaEqSym = alphaEqSymDefault

instance SizeProp NUM
  where
    sizeProp Abs  (WrapFull a :* Nil)               = abs (infoSize a)
    sizeProp Sign (WrapFull a :* Nil)               = signum (infoSize a)
    sizeProp Add  (WrapFull a :* WrapFull b :* Nil) = infoSize a + infoSize b
    sizeProp Sub  (WrapFull a :* WrapFull b :* Nil) = infoSize a - infoSize b
    sizeProp Mul  (WrapFull a :* WrapFull b :* Nil) = infoSize a * infoSize b



instance (NUM :<: dom, OptimizeSuper dom) => Optimize NUM dom
  where
    constructFeatOpt Abs (a :* Nil)
        | RangeSet r <- infoRange (getInfo a)
        , isNatural r
        = return a

    constructFeatOpt Sign (a :* Nil)
        | RangeSet ra <- infoRange (getInfo a)
        , 0 `rangeLess` ra
        = return (literalDecor 1)

    constructFeatOpt Sign (a :* Nil)
        | RangeSet ra <- infoRange (getInfo a)
        , ra `rangeLess` 0
        = return (literalDecor (-1))

    constructFeatOpt Add (a :* b :* Nil)
        | Just 0 <- viewLiteral b = return a
        | Just 0 <- viewLiteral a = return b
        | alphaEq a b = constructFeatOpt Mul (a :* literalDecor 2 :* Nil)

    constructFeatOpt Add (a :* (op :$ b :$ c) :* Nil)
        | Just a'      <- viewLiteral a
        , Just (_,Add) <- prjDecor op
        , Just c'      <- viewLiteral c
        = constructFeat Add (b :* literalDecor (a'+c') :* Nil)

    constructFeatOpt Add (a :* (op :$ b :$ c) :* Nil)
        | Just a'      <- viewLiteral a
        , Just (_,Sub) <- prjDecor op
        , Just c'      <- viewLiteral c
        = constructFeat Add (b :* literalDecor (a'-c') :* Nil)

    constructFeatOpt Add ((op :$ a :$ b) :* c :* Nil)
        | Just c'      <- viewLiteral c
        , Just (_,Add) <- prjDecor op
        , Just b'      <- viewLiteral b
        = constructFeat Add (a :* literalDecor (b'+c') :* Nil)

    constructFeatOpt Add ((op :$ a :$ b) :* c :* Nil)
        | Just c'      <- viewLiteral c
        , Just (_,Sub) <- prjDecor op
        , Just b'      <- viewLiteral b
        = constructFeat Add (a :* literalDecor (c'-b') :* Nil)

    constructFeatOpt Add ((op1 :$ a :$ b) :* (op2 :$ c :$ d) :* Nil)
        | Just (_,Add) <- prjDecor op1
        , Just (_,Add) <- prjDecor op2
        , Just b'      <- viewLiteral b
        , Just d'      <- viewLiteral d
        = do
            ac <- constructFeat Add (a :* c :* Nil)
            constructFeat Add (ac :* literalDecor (b'+d') :* Nil)

    constructFeatOpt Add ((op1 :$ a :$ b) :* (op2 :$ c :$ d) :* Nil)
        | Just (_,Add) <- prjDecor op1
        , Just (_,Sub) <- prjDecor op2
        , alphaEq a c
        , alphaEq b d
        = constructFeat Add (a :* c :* Nil)

    constructFeatOpt Sub ((op1 :$ a :$ b) :* (op2 :$ c :$ d) :* Nil)
        | Just (_,Add) <- prjDecor op1
        , Just (_,Sub) <- prjDecor op2
        , alphaEq a c
        , alphaEq b d
        = constructFeat Add (b :* d :* Nil)

    constructFeatOpt Sub (a :* b :* Nil)
        | Just 0 <- viewLiteral b = return a
        | alphaEq a b             = return $ literalDecor 0

    constructFeatOpt Mul (a :* b :* Nil)
        | Just 0 <- viewLiteral a = return a
        | Just 1 <- viewLiteral a = return b
        | Just 0 <- viewLiteral b = return b
        | Just 1 <- viewLiteral b = return a

    constructFeatOpt Mul (a :* (op :$ b :$ c) :* Nil)
        | Just a'      <- viewLiteral a
        , Just (_,Mul) <- prjDecor op
        , Just c'      <- viewLiteral c
        = constructFeat Mul (b :* literalDecor (a'*c') :* Nil)

    constructFeatOpt Mul ((op :$ a :$ b) :* c :* Nil)
        | Just c'      <- viewLiteral c
        , Just (_,Mul) <- prjDecor op
        , Just b'      <- viewLiteral b
        = constructFeat Mul (a :* literalDecor (b'*c') :* Nil)

    constructFeatOpt Mul ((op1 :$ a :$ b) :* (op2 :$ c :$ d) :* Nil)
        | Just (_,Mul) <- prjDecor op1
        , Just (_,Mul) <- prjDecor op2
        , Just b'      <- viewLiteral b
        , Just d'      <- viewLiteral d
        = do
            ac <- constructFeat Mul (a :* c :* Nil)
            constructFeat Mul (ac :* literalDecor (b'*d') :* Nil)

    -- Cases to make sure literals end up to the right:
    constructFeatOpt Add (a :* b :* Nil)
        | Just a' <- viewLiteral a = constructFeatUnOpt Add (b :* a :* Nil)

    constructFeatOpt Mul (a :* b :* Nil)
        | Just a' <- viewLiteral a = constructFeatUnOpt Mul (b :* a :* Nil)

    constructFeatOpt a args = constructFeatUnOpt a args

    constructFeatUnOpt = constructFeatUnOptDefault

