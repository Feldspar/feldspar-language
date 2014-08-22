{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
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
import Feldspar.Core.Constructs.Eq
import Feldspar.Core.Constructs.Ord
import Feldspar.Core.Constructs.Bits
import Feldspar.Core.Constructs.Logic
import Feldspar.Core.Constructs.Literal
import Feldspar.Core.Constructs.Integral
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

semanticInstances ''NUM

instance EvalBind NUM where evalBindSym = evalBindSymDefault

instance AlphaEq dom dom dom env => AlphaEq NUM NUM dom env
  where
    alphaEqSym = alphaEqSymDefault

instance Sharable NUM

instance Cumulative NUM

instance SizeProp (NUM :|| Type)
  where
    sizeProp (C' Abs)  (WrapFull a :* Nil)               = abs (infoSize a)
    sizeProp (C' Sign) (WrapFull a :* Nil)               = signum (infoSize a)
    sizeProp (C' Add)  (WrapFull a :* WrapFull b :* Nil) = infoSize a + infoSize b
    sizeProp (C' Sub)  (WrapFull a :* WrapFull b :* Nil) = infoSize a - infoSize b
    sizeProp (C' Mul)  (WrapFull a :* WrapFull b :* Nil) = infoSize a * infoSize b


instance ( Cumulative dom
         , (NUM      :|| Type) :<: dom
         , (ORD      :|| Type) :<: dom
         , (EQ       :|| Type) :<: dom
         , (BITS     :|| Type) :<: dom
         , (Logic    :|| Type) :<: dom
         , (Literal  :|| Type) :<: dom
         , (INTEGRAL :|| Type) :<: dom
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

    constructFeatOpt _ (C' Sign) (a :* Nil)
        | RangeSet ra <- infoRange (getInfo a)
        , ra `rangeLess` 0
        = return (literalDecor (-1))

    constructFeatOpt opts (C' Add) (a :* b :* Nil)
        | Just 0 <- viewLiteral b = return a
        | Just 0 <- viewLiteral a = return b
        | alphaEq a b = constructFeatOpt opts (c' Mul) (a :* literalDecor 2 :* Nil)

    constructFeatOpt opts s@(C' Add) (a :* (op :$ b :$ c) :* Nil)
        | Just al       <- viewLiteral a
        , Just (C' Add) <- prjF op
        , Just cl       <- viewLiteral c
        = constructFeat opts s (b :* literalDecor (al+cl) :* Nil)

    constructFeatOpt opts s@(C' Add) (a :* (op :$ b :$ c) :* Nil)
        | Just al       <- viewLiteral a
        , Just (C' Sub) <- prjF op
        , Just cl       <- viewLiteral c
        = constructFeat opts s (b :* literalDecor (al-cl) :* Nil)

    constructFeatOpt opts s@(C' Add) ((op :$ a :$ b) :* c :* Nil)
        | Just cl       <- viewLiteral c
        , Just (C' Add) <- prjF op
        , Just bl       <- viewLiteral b
        = constructFeat opts s (a :* literalDecor (bl+cl) :* Nil)

    constructFeatOpt opts s@(C' Add) ((op :$ a :$ b) :* c :* Nil)
        | Just cl       <- viewLiteral c
        , Just (C' Sub) <- prjF op
        , Just bl       <- viewLiteral b
        = constructFeat opts s (a :* literalDecor (cl-bl) :* Nil)

    constructFeatOpt opts (C' Add) ((op1 :$ a :$ b) :* (op2 :$ c :$ d) :* Nil)
        | Just (C' Add) <- prjF op1
        , Just (C' Add) <- prjF op2
        , Just bl       <- viewLiteral b
        , Just dl       <- viewLiteral d
        = do
            ac <- constructFeat opts (c' Add) (a :* c :* Nil)
            constructFeat opts (c' Add) (ac :* literalDecor (bl+dl) :* Nil)

    constructFeatOpt opts (C' Add) ((op1 :$ a :$ b) :* (op2 :$ c :$ d) :* Nil)
        | Just (C' Add) <- prjF op1
        , Just (C' Sub) <- prjF op2
        , alphaEq a c
        , alphaEq b d
        = constructFeat opts (c' Add) (a :* c :* Nil)

    -- x `mod` y + y * (x `div` y) ==> x
    -- Partial index calculations materialized from contractT . expandT 2
    -- in MultiDim.hs, which is a no-op.
    constructFeatOpt opts (C' Add) ((rem :$ a :$ b) :* (mul :$ c :$ (quot :$ d :$ e)) :* Nil)
        | Just (C' Rem)  <- prjF rem
        , Just (C' Mul)  <- prjF mul
        , Just (C' Quot) <- prjF quot
        , alphaEq a d
        , alphaEq c e
        , alphaEq b e
        = return a

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
        | Just cl         <- viewLiteral c
        , Just s@(C' Add) <- prjF op
        , Just bl         <- viewLiteral b
        = constructFeat opts s (a :* literalDecor (bl-cl) :* Nil)

    -- (a - literal b) - literal c ==> a - literal (b + c)
    constructFeatOpt opts s@(C' Sub) ((op :$ a :$ b) :* c :* Nil)
        | Just cl       <- viewLiteral c
        , Just (C' Sub) <- prjF op
        , Just bl       <- viewLiteral b
        = constructFeat opts s (a :* literalDecor (bl+cl) :* Nil)

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


    constructFeatOpt opts (C' Mul) (a :* b :* Nil)
        | Just 0 <- viewLiteral a = return a
        | Just 1 <- viewLiteral a = return b
        | Just 0 <- viewLiteral b = return b
        | Just 1 <- viewLiteral b = return a
        | IntType{} <- infoType (getInfo a)
        , Just a' <- viewLiteral a
        , Just k <- log2 (fromIntegral a')
        = do
             constructFeatOpt opts (c' ShiftL) (b :* literalDecor (fromIntegral k) :* Nil)
        | IntType{} <- infoType (getInfo b)
        , Just b' <- viewLiteral b
        , Just k <- log2 (fromIntegral b')
        = do
             constructFeatOpt opts (c' ShiftL) (a :* literalDecor (fromIntegral k) :* Nil)


    constructFeatOpt opts s@(C' Mul) (a :* (op :$ b :$ c) :* Nil)
        | Just al       <- viewLiteral a
        , Just (C' Mul) <- prjF op
        , Just cl       <- viewLiteral c
        = constructFeat opts s (b :* literalDecor (al*cl) :* Nil)

    constructFeatOpt opts s@(C' Mul) ((op :$ a :$ b) :* c :* Nil)
        | Just cl       <- viewLiteral c
        , Just (C' Mul) <- prjF op
        , Just bl       <- viewLiteral b
        = constructFeat opts s (a :* literalDecor (bl*cl) :* Nil)

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

log2 :: Integer -> Maybe Integer
log2 n
    | n == 2 Prelude.^ l = Just l
    | otherwise          = Nothing
  where
    l = toInteger $ length $ takeWhile (<n) $ iterate (*2) 1
