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

module Feldspar.Core.Constructs.Bits
    ( BITS (..)
    ) where

import Language.Syntactic
import Language.Syntactic.Constructs.Binding
import Language.Syntactic.Constructs.Literal

import Feldspar.Range
import Feldspar.Core.Types
import Feldspar.Core.Interpretation
import Feldspar.Core.Constructs.Logic
import Feldspar.Core.Constructs.Eq
import Feldspar.Core.Constructs.Ord

import Data.Bits

data BITS a
  where
    BAnd          :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => BITS (a :-> a :-> Full a)
    BOr           :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => BITS (a :-> a :-> Full a)
    BXor          :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => BITS (a :-> a :-> Full a)
    Complement    :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => BITS (a :->       Full a)

    Bit           :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => BITS (Index :->       Full a)
    SetBit        :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => BITS (a :-> Index :-> Full a)
    ClearBit      :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => BITS (a :-> Index :-> Full a)
    ComplementBit :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => BITS (a :-> Index :-> Full a)
    TestBit       :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => BITS (a :-> Index :-> Full Bool)

    ShiftLU       :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => BITS (a :-> Index :-> Full a)
    ShiftRU       :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => BITS (a :-> Index :-> Full a)
    ShiftL        :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => BITS (a :-> IntN  :-> Full a)
    ShiftR        :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => BITS (a :-> IntN  :-> Full a)
    RotateLU      :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => BITS (a :-> Index :-> Full a)
    RotateRU      :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => BITS (a :-> Index :-> Full a)
    RotateL       :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => BITS (a :-> IntN  :-> Full a)
    RotateR       :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => BITS (a :-> IntN  :-> Full a)
    ReverseBits   :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => BITS (a :->           Full a)

    BitScan       :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => BITS (a :-> Full Index)
    BitCount      :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => BITS (a :-> Full Index)

    IsSigned      :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => BITS (a :-> Full Bool)

instance Semantic BITS
  where
    semantics BAnd          = Sem "(.&.)"      (.&.)
    semantics BOr           = Sem "(.|.)"      (.|.)
    semantics BXor          = Sem "xor"        xor
    semantics Complement    = Sem "complement" complement

    semantics Bit           = Sem "bit"           (bit . fromIntegral)
    semantics SetBit        = Sem "setBit"        (liftIntWord setBit)
    semantics ClearBit      = Sem "clearBit"      (liftIntWord clearBit)
    semantics ComplementBit = Sem "complementBit" (liftIntWord complementBit)
    semantics TestBit       = Sem "testBit"       (liftIntWord testBit)

    semantics ShiftLU       = Sem "shiftL"      (liftIntWord shiftL)
    semantics ShiftRU       = Sem "shiftR"      (liftIntWord shiftR)
    semantics ShiftL        = Sem "shiftL"      (liftInt shiftL)
    semantics ShiftR        = Sem "shiftR"      (liftInt shiftR)
    semantics RotateLU      = Sem "rotateL"     (liftIntWord rotateL)
    semantics RotateRU      = Sem "rotateR"     (liftIntWord rotateR)
    semantics RotateL       = Sem "rotateL"     (liftInt rotateL)
    semantics RotateR       = Sem "rotateR"     (liftInt rotateR)
    semantics ReverseBits   = Sem "reverseBits" evalReverseBits

    semantics BitScan       = Sem "bitScan"  evalBitScan
    semantics BitCount      = Sem "bitCount" evalBitCount

    semantics IsSigned      = Sem "isSigned" isSigned

liftIntWord :: (a -> Int -> b) -> (a -> WordN -> b)
liftIntWord f x = f x . fromIntegral

liftInt :: (a -> Int -> b) -> (a -> IntN -> b)
liftInt f x = f x . fromIntegral

evalReverseBits :: (Num b, Bits b) => b -> b
evalReverseBits b = revLoop b 0 (0 `asTypeOf` b)
  where
    bSz = bitSize b
    revLoop x i n | i >= bSz    = n
                  | testBit x i = revLoop x (i+1) (setBit n (bSz - i - 1))
                  | otherwise   = revLoop x (i+1) n

evalBitScan :: Bits b => b -> WordN
evalBitScan b =
   if isSigned b
   then scanLoop b (testBit b (bitSize b - 1)) (bitSize b - 2) 0
   else scanLoop b False (bitSize b - 1) 0
  where
    scanLoop x t i n | i Prelude.< 0            = n
                     | testBit x i Prelude./= t = n
                     | otherwise                = scanLoop x t (i-1) (n+1)

evalBitCount :: Bits b => b -> WordN
evalBitCount b = loop b (bitSize b - 1) 0
  where
    loop x i n | i Prelude.< 0 = n
               | testBit x i   = loop x (i-1) (n+1)
               | otherwise     = loop x (i-1) n

instance Equality BITS where equal = equalDefault; exprHash = exprHashDefault
instance Render   BITS where renderArgs = renderArgsDefault
instance ToTree   BITS
instance Eval     BITS where evaluate = evaluateDefault
instance EvalBind BITS where evalBindSym = evalBindSymDefault
instance Sharable BITS

instance AlphaEq dom dom dom env => AlphaEq BITS BITS dom env
  where
    alphaEqSym = alphaEqSymDefault

{-
instance SizeProp BITS
  where
    sizeProp BAnd (WrapFull a :* WrapFull b :* Nil) = rangeAnd (infoSize a) (infoSize b)
    sizeProp BOr  (WrapFull a :* WrapFull b :* Nil) = rangeOr (infoSize a) (infoSize b)
    sizeProp BXor (WrapFull a :* WrapFull b :* Nil) = rangeXor (infoSize a) (infoSize b)

    sizeProp ShiftLU (WrapFull a :* WrapFull b :* Nil) = rangeShiftLU (infoSize a) (infoSize b)
    sizeProp ShiftRU (WrapFull a :* WrapFull b :* Nil) = rangeShiftRU (infoSize a) (infoSize b)

    sizeProp Complement (WrapFull a :* Nil) = rangeComplement (infoSize a)

    sizeProp a args = sizePropDefault a args


instance ( BITS  :<: dom
         , Logic :<: dom
         , EQ    :<: dom
         , ORD   :<: dom
         , OptimizeSuper dom
         )
      => Optimize BITS dom
  where
    constructFeatOpt BAnd (a :* b :* Nil)
        | Just 0 <- viewLiteral a              = return a
        | Just x <- viewLiteral a, isAllOnes x = return b
        | Just 0 <- viewLiteral b              = return b
        | Just x <- viewLiteral b, isAllOnes x = return a

    constructFeatOpt BOr (a :* b :* Nil)
        | Just 0 <- viewLiteral a              = return b
        | Just x <- viewLiteral a, isAllOnes x = return a
        | Just 0 <- viewLiteral b              = return a
        | Just x <- viewLiteral b, isAllOnes x = return b

    constructFeatOpt BXor (a :* b :* Nil)
        | Just 0 <- viewLiteral a              = return b
        | Just x <- viewLiteral a, isAllOnes x = constructFeat Complement (b :* Nil)
        | Just 0 <- viewLiteral b              = return a
        | Just x <- viewLiteral b, isAllOnes x = constructFeat Complement (a :* Nil)

    constructFeatOpt BXor ((xo :$ v1 :$ v2) :* v3 :* Nil)
        | Just (_,BXor) <- prjDecor xo
        , alphaEq v2 v3
        = return v1

    constructFeatOpt TestBit ((xo :$ v1 :$ v2) :* v3 :* Nil)
        | Just (_,BXor) <- prjDecor xo
        , Just a <- viewLiteral v2
        , Just b <- viewLiteral v3
        , a == 2 ^ b
        = do tb <- constructFeat TestBit (v1 :* v3 :* Nil)
             constructFeat Not (tb :* Nil)

    constructFeatOpt ShiftLU  args = optZero ShiftLU  args
    constructFeatOpt ShiftRU  args = optZero ShiftRU  args
    constructFeatOpt ShiftL   args = optZero ShiftL   args
    constructFeatOpt ShiftR   args = optZero ShiftR   args
    constructFeatOpt RotateLU args = optZero RotateLU args
    constructFeatOpt RotateRU args = optZero RotateRU args
    constructFeatOpt RotateL  args = optZero RotateL  args
    constructFeatOpt RotateR  args = optZero RotateR  args

    constructFeatOpt feat args = constructFeatUnOpt feat args

    constructFeatUnOpt = constructFeatUnOptDefault


isAllOnes :: Bits a => a -> Bool
isAllOnes x = x Prelude.== complement 0

optZero :: ( Eq b, Num b
           , Literal TypeCtx :<: dom
           , Optimize feature dom
           )
        => feature (a :-> (b :-> Full a))
        -> Args (AST (Decor Info dom)) (a :-> (b :-> Full a))
        -> Opt (AST (Decor Info dom) (Full a))
optZero f (a :* b :* Nil)
    | Just 0 <- viewLiteral b = return a
    | otherwise               = constructFeatUnOpt f (a :* b :* Nil)
-}

