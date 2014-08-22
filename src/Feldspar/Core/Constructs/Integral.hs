{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
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

module Feldspar.Core.Constructs.Integral
    ( INTEGRAL (..)
    ) where

import Data.Bits

import Language.Syntactic
import Language.Syntactic.Constructs.Binding
import Language.Syntactic.Constructs.Condition

import Feldspar.Range
import Feldspar.Core.Types
import Feldspar.Core.Interpretation
import Feldspar.Core.Constructs.Bits
import Feldspar.Core.Constructs.Eq
import Feldspar.Core.Constructs.Ord
import Feldspar.Core.Constructs.Logic
import Feldspar.Core.Constructs.Complex

data INTEGRAL a
  where
    Quot :: (Type a, BoundedInt a, Size a ~ Range a) => INTEGRAL (a :-> a :-> Full a)
    Rem  :: (Type a, BoundedInt a, Size a ~ Range a) => INTEGRAL (a :-> a :-> Full a)
    Div  :: (Type a, BoundedInt a, Size a ~ Range a) => INTEGRAL (a :-> a :-> Full a)
    Mod  :: (Type a, BoundedInt a, Size a ~ Range a) => INTEGRAL (a :-> a :-> Full a)
    Exp  :: (Type a, BoundedInt a, Size a ~ Range a) => INTEGRAL (a :-> a :-> Full a)

instance Semantic INTEGRAL
  where
    semantics Quot = Sem "quot" quot
    semantics Rem  = Sem "rem"  rem
    semantics Div  = Sem "div"  div
    semantics Mod  = Sem "mod"  mod
    semantics Exp  = Sem "(^)"  (^)

semanticInstances ''INTEGRAL

instance EvalBind INTEGRAL where evalBindSym = evalBindSymDefault

instance AlphaEq dom dom dom env => AlphaEq INTEGRAL INTEGRAL dom env
  where
    alphaEqSym = alphaEqSymDefault

instance Sharable INTEGRAL

instance Cumulative INTEGRAL

instance SizeProp (INTEGRAL :|| Type)
  where
    sizeProp (C' Quot) (WrapFull a :* WrapFull b :* Nil) = rangeQuot (infoSize a) (infoSize b)
    sizeProp (C' Rem)  (WrapFull a :* WrapFull b :* Nil) = rangeRem (infoSize a) (infoSize b)
    sizeProp (C' Div)  (WrapFull a :* WrapFull b :* Nil) = rangeDiv (infoSize a) (infoSize b)
    sizeProp (C' Mod)  (WrapFull a :* WrapFull b :* Nil) = rangeMod (infoSize a) (infoSize b)
    sizeProp (C' Exp)  (WrapFull a :* WrapFull b :* Nil) = rangeExp (infoSize a) (infoSize b)

instance
    ( (INTEGRAL  :||Type) :<: dom
    , (BITS      :||Type) :<: dom
    , (EQ        :||Type) :<: dom
    , (ORD       :||Type) :<: dom
    , (COMPLEX :|| Type) :<: dom
    , (Condition :||Type) :<: dom
    , (Logic     :||Type) :<: dom
    , Cumulative dom
    , OptimizeSuper dom
    , Optimize (Condition :|| Type) dom
    ) =>
      Optimize (INTEGRAL :|| Type) dom
  where
    constructFeatOpt _ (C' Quot) (a :* b :* Nil)
        | Just 1 <- viewLiteral b = return a
{-
    -- TODO: Rule disabled because of cyclic imports.
    constructFeatOpt opts (C' Quot) (a :* b :* Nil)
        | Just b' <- viewLiteral b
        , b' > 0
        , isPowerOfTwo b'
        , let l    = log2 b'
        , let lLit = literalDecor l
        = if isNatural $ infoSize $ getInfo a
            then constructFeat opts (c' ShiftR) (a :* lLit :* Nil)
            else do
                aIsNeg  <- constructFeat opts (c' LTH) (a :* literalDecor 0 :* Nil)
                a'      <- constructFeat opts (c' Add) (a :* literalDecor (2^l-1) :* Nil)
                negCase <- constructFeat opts (c' ShiftR) (a' :* lLit :* Nil)
                posCase <- constructFeat opts (c' ShiftR) (a :* lLit :* Nil)
                constructFeat opts (c' Condition)
                    (aIsNeg :* negCase :* posCase :* Nil)
      -- TODO This rule should also fire when `b` is `2^l` but not a literal.
      -- TODO Make a case for `isNegative $ infoSize $ getInfo a`. Note that
      --      `isNegative /= (not . isNatural)`
      -- TODO Or maybe both `isNegative` and `isPositive` are handled by the
      --      size-based optimization of `Condition`?
-}
    constructFeatOpt _ (C' Rem) (a :* b :* Nil)
        | rangeLess sza szb
        , isNatural sza
        = return a
      where
        sza = infoSize $ getInfo a
        szb = infoSize $ getInfo b

    constructFeatOpt _ (C' Div) (a :* b :* Nil)
        | Just 1 <- viewLiteral b = return a

    constructFeatOpt opts (C' Div) (a :* b :* Nil)
        | IntType U _ <- infoType $ getInfo a
        , Just b' <- viewLiteral b
        , b' > 0
        , isPowerOfTwo b'
        = constructFeat opts (c' ShiftRU) (a :* literalDecor (log2 b') :* Nil)

    constructFeatOpt opts (C' Div) (a :* b :* Nil)
        | sameSign (infoSize (getInfo a)) (infoSize (getInfo b))
        = constructFeat opts (c' Quot) (a :* b :* Nil)

    constructFeatOpt _ (C' Mod) (a :* b :* Nil)
        | rangeLess sza szb
        , isNatural sza
        = return a
      where
        sza = infoSize $ getInfo a
        szb = infoSize $ getInfo b

    constructFeatOpt opts (C' Mod) (a :* b :* Nil)
        | sameSign (infoSize (getInfo a)) (infoSize (getInfo b))
        = constructFeat opts (c' Rem) (a :* b :* Nil)

    constructFeatOpt _ (C' Exp) (a :* b :* Nil)
        | Just 1 <- viewLiteral a = return $ literalDecor 1
        | Just 0 <- viewLiteral a = return $ literalDecor 0
        | Just 1 <- viewLiteral b = return a
        | Just 0 <- viewLiteral b = return $ literalDecor 1

    constructFeatOpt opts (C' Exp) (a :* b :* Nil)
        | Just (-1) <- viewLiteral a = do
            bLSB    <- constructFeat opts (c' BAnd) (b :* literalDecor 1 :* Nil)
            bIsEven <- constructFeat opts (c' Equal) (bLSB :* literalDecor 0 :* Nil)  -- TODO Use testBit? (remove EQ :<: dom and import)
            constructFeat opts (c' Condition)
                (bIsEven :* literalDecor 1 :* literalDecor (-1) :* Nil)

    constructFeatOpt opts a args = constructFeatUnOpt opts a args

    constructFeatUnOpt opts x@(C' _) = constructFeatUnOptDefault opts x

-- Auxiliary functions

-- shouldn't be used for negative numbers
isPowerOfTwo :: (Num a, Bits a) => a -> Bool
isPowerOfTwo x = x .&. (x - 1) == 0 && (x /= 0)

log2 :: (BoundedInt a, Integral b) => a -> b
log2 v | v <= 1 = 0
log2 v = 1 + log2 (shiftR v 1)

sameSign :: BoundedInt a => Range a -> Range a -> Bool
sameSign ra rb
    =  isNatural  ra && isNatural  rb
    || isNegative ra && isNegative rb

