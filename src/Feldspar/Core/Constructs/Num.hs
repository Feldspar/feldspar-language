{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
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

module Feldspar.Core.Constructs.Num
    ( NUM (..)
    ) where



import Language.Syntactic
import Language.Syntactic.Constructs.Binding

import Data.Complex (Complex(..))

import Feldspar.Range
import Feldspar.Core.Types
import Feldspar.Core.Interpretation
import Feldspar.Core.Constructs.Literal
import Feldspar.Core.Constructs.Complex


data NUM a
  where
    Abs  :: (Type a, Num a, Num (Size a)) => NUM (a :-> Full a)
    Sign :: (Type a, Num a, Num (Size a)) => NUM (a :-> Full a)
    Add  :: (Type a, Num a, Num (Size a)) => NUM (a :-> a :-> Full a)
    Sub  :: (Type a, Num a, Num (Size a)) => NUM (a :-> a :-> Full a)
    Mul  :: (Type a, Num a, Num (Size a)) => NUM (a :-> a :-> Full a)

instance Semantic NUM
  where
    semantics Abs  = Sem "abs" abs
    semantics Sign = Sem "signum" signum
    semantics Add  = Sem "(+)" (+)
    semantics Sub  = Sem "(-)" (-)
    semantics Mul  = Sem "(*)" (*)

instance Equality NUM where equal = equalDefault; exprHash = exprHashDefault
instance Render   NUM where renderArgs = renderArgsDefault
instance ToTree   NUM
instance Eval     NUM where evaluate = evaluateDefault
instance EvalBind NUM where evalBindSym = evalBindSymDefault
instance Sharable NUM

instance AlphaEq dom dom dom env => AlphaEq NUM NUM dom env
  where
    alphaEqSym = alphaEqSymDefault

instance SizeProp (NUM :|| Type)
  where
    sizeProp (C' Abs)  (WrapFull a :* Nil)               = abs (infoSize a)
    sizeProp (C' Sign) (WrapFull a :* Nil)               = signum (infoSize a)
    sizeProp (C' Add)  (WrapFull a :* WrapFull b :* Nil) = infoSize a + infoSize b
    sizeProp (C' Sub)  (WrapFull a :* WrapFull b :* Nil) = infoSize a - infoSize b
    sizeProp (C' Mul)  (WrapFull a :* WrapFull b :* Nil) = infoSize a * infoSize b


instance ( (NUM     :|| Type) :<: dom
         , (Literal :|| Type) :<: dom
         , (COMPLEX :|| Type) :<: dom
         , OptimizeSuper dom
         )
      => Optimize (NUM :|| Type) dom
  where
    constructFeatOpt _ (C' Abs) (a :* Nil)
        | RangeSet r <- infoRange (getInfo a)
        , isNatural r
        = return a

    constructFeatOpt _ (C' Sign) (a :* Nil)
        | RangeSet ra <- infoRange (getInfo a)
        , 0 `rangeLess` ra
        = return (literalDecor 1)

    constructFeatOpt opts (C' Sign) (a :* Nil)
        | RangeSet ra <- infoRange (getInfo a)
        , ra `rangeLess` 0
        = return (literalDecor (-1))

    constructFeatOpt opts (C' Add) (a :* b :* Nil)
        | Just 0 <- viewLiteral b = return a
        | Just 0 <- viewLiteral a = return b
        | alphaEq a b = constructFeatOpt opts (c' Mul) (a :* literalDecor 2 :* Nil)

    constructFeatOpt opts s@(C' Add) (a :* (op :$ b :$ c) :* Nil)
        | Just a'      <- viewLiteral a
        , Just (C' Add) <- prjF op
        , Just c'      <- viewLiteral c
        = constructFeat opts s (b :* literalDecor (a'+c') :* Nil)

    constructFeatOpt opts s@(C' Add) (a :* (op :$ b :$ c) :* Nil)
        | Just a'      <- viewLiteral a
        , Just (C' Sub) <- prjF op
        , Just c'      <- viewLiteral c
        = constructFeat opts s (b :* literalDecor (a'-c') :* Nil)

    constructFeatOpt opts s@(C' Add) ((op :$ a :$ b) :* c :* Nil)
        | Just c'      <- viewLiteral c
        , Just (C' Add) <- prjF op
        , Just b'      <- viewLiteral b
        = constructFeat opts s (a :* literalDecor (b'+c') :* Nil)

    constructFeatOpt opts s@(C' Add) ((op :$ a :$ b) :* c :* Nil)
        | Just c'      <- viewLiteral c
        , Just (C' Sub) <- prjF op
        , Just b'      <- viewLiteral b
        = constructFeat opts s (a :* literalDecor (c'-b') :* Nil)

    constructFeatOpt opts (C' Add) ((op1 :$ a :$ b) :* (op2 :$ c :$ d) :* Nil)
        | Just (C' Add) <- prjF op1
        , Just (C' Add) <- prjF op2
        , Just b'      <- viewLiteral b
        , Just d'      <- viewLiteral d
        = do
            ac <- constructFeat opts (c' Add) (a :* c :* Nil)
            constructFeat opts (c' Add) (ac :* literalDecor (b'+d') :* Nil)

    constructFeatOpt opts (C' Add) ((op1 :$ a :$ b) :* (op2 :$ c :$ d) :* Nil)
        | Just (C' Add) <- prjF op1
        , Just (C' Sub) <- prjF op2
        , alphaEq a c
        , alphaEq b d
        = constructFeat opts (c' Add) (a :* c :* Nil)

    -- literal a - (b + literal c) ==> literal (a-c) - b
    -- constructFeatOpt opts s@(C' Sub) (a :* (op :$ b :$ c) :* Nil)
    --     | Just a'      <- viewLiteral a
    --     , Just (C' Add) <- prjF op
    --     , Just c'      <- viewLiteral c
    --     = constructFeat opts s (literalDecor (a'-c') :* b :* Nil)

    -- literal a - (b - literal c) ==> literal (a+c) - b
    -- constructFeatOpt opts s@(C' Sub) (a :* (op :$ b :$ c) :* Nil)
    --     | Just a'      <- viewLiteral a
    --     , Just (C' Sub) <- prjF op
    --     , Just c'      <- viewLiteral c
    --     = constructFeat opts s (literalDecor (a'+c') :* b :* Nil)

    -- (a + literal b) - literal c ==> a + literal (b - c)
    constructFeatOpt opts (C' Sub) ((op :$ a :$ b) :* c :* Nil)
        | Just c'      <- viewLiteral c
        , Just s@(C' Add) <- prjF op
        , Just b'      <- viewLiteral b
        = constructFeat opts s (a :* literalDecor (b'-c') :* Nil)

    -- (a - literal b) - literal c ==> a - literal (b + c)
    constructFeatOpt opts s@(C' Sub) ((op :$ a :$ b) :* c :* Nil)
        | Just c'      <- viewLiteral c
        , Just (C' Sub) <- prjF op
        , Just b'      <- viewLiteral b
        = constructFeat opts s (a :* literalDecor (b'+c') :* Nil)

    constructFeatOpt opts (C' Sub) ((op1 :$ a :$ b) :* (op2 :$ c :$ d) :* Nil)
        | Just (C' Add) <- prjF op1
        , Just (C' Sub) <- prjF op2
        , alphaEq a c
        , alphaEq b d
        = constructFeat opts (c' Add) (b :* d :* Nil)

    constructFeatOpt _ (C' Sub) (a :* b :* Nil)
        | Just 0 <- viewLiteral b = return a
        | alphaEq a b             = return $ literalDecor 0

    -- (x + yi) * i ==> -y + xi; (x + yi) * (-i) ==> y - xi
    constructFeatOpt opts (C' Mul) (a :* iunit :* Nil)
        | ComplexType FloatType <- infoType (getInfo iunit)
        , Just (0 :+ k)         <- viewLiteral iunit
        , abs k == 1
        = do
             ra <- constructFeat opts (c' RealPart) (a :* Nil)
             ia <- constructFeat opts (c' ImagPart) (a :* Nil)
             iainv <- constructFeatOpt opts (c' Mul) (literalDecor (-k) :* ia :* Nil)
             rainv <- constructFeatOpt opts (c' Mul) (literalDecor k :* ra :* Nil)
             constructFeatOpt opts (c' MkComplex) (iainv :* rainv :* Nil)


    constructFeatOpt _ (C' Mul) (a :* b :* Nil)
        | Just 0 <- viewLiteral a = return a
        | Just 1 <- viewLiteral a = return b
        | Just 0 <- viewLiteral b = return b
        | Just 1 <- viewLiteral b = return a

    constructFeatOpt opts s@(C' Mul) (a :* (op :$ b :$ c) :* Nil)
        | Just a'      <- viewLiteral a
        , Just (C' Mul) <- prjF op
        , Just c'      <- viewLiteral c
        = constructFeat opts s (b :* literalDecor (a'*c') :* Nil)

    constructFeatOpt opts s@(C' Mul) ((op :$ a :$ b) :* c :* Nil)
        | Just c'      <- viewLiteral c
        , Just (C' Mul) <- prjF op
        , Just b'      <- viewLiteral b
        = constructFeat opts s (a :* literalDecor (b'*c') :* Nil)

    constructFeatOpt opts (C' Mul) ((op1 :$ a :$ b) :* (op2 :$ c :$ d) :* Nil)
        | Just (C' Mul) <- prjF op1
        , Just (C' Mul) <- prjF op2
        , Just b'      <- viewLiteral b
        , Just d'      <- viewLiteral d
        = do
            ac <- constructFeat opts (c' Mul) (a :* c :* Nil)
            constructFeat opts (c' Mul) (ac :* literalDecor (b'*d') :* Nil)

    -- Cases to make sure literals end up to the right:
    constructFeatOpt opts (C' Add) (a :* b :* Nil)
        | Just _ <- viewLiteral a = constructFeatUnOpt opts (c' Add) (b :* a :* Nil)

    constructFeatOpt opts (C' Mul) (a :* b :* Nil)
        | Just _ <- viewLiteral a = constructFeatUnOpt opts (c' Mul) (b :* a :* Nil)

    constructFeatOpt opts a args = constructFeatUnOpt opts a args

    constructFeatUnOpt opts x@(C' _) = constructFeatUnOptDefault opts x
