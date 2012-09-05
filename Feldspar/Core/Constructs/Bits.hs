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



import Data.Hash

import Language.Syntactic
import Language.Syntactic.Interpretation.Semantics
import Language.Syntactic.Constructs.Binding

import Feldspar.Range
import Feldspar.Lattice
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


instance WitnessCons BITS
  where
    witnessCons BAnd          = ConsWit
    witnessCons BOr           = ConsWit
    witnessCons BXor          = ConsWit
    witnessCons Complement    = ConsWit

    witnessCons Bit           = ConsWit
    witnessCons SetBit        = ConsWit
    witnessCons ClearBit      = ConsWit
    witnessCons ComplementBit = ConsWit
    witnessCons TestBit       = ConsWit

    witnessCons ShiftLU       = ConsWit
    witnessCons ShiftRU       = ConsWit
    witnessCons ShiftL        = ConsWit
    witnessCons ShiftR        = ConsWit
    witnessCons RotateLU      = ConsWit
    witnessCons RotateRU      = ConsWit
    witnessCons RotateL       = ConsWit
    witnessCons RotateR       = ConsWit
    witnessCons ReverseBits   = ConsWit

    witnessCons BitScan       = ConsWit
    witnessCons BitCount      = ConsWit

    witnessCons IsSigned      = ConsWit


instance WitnessSat BITS
  where
    type SatContext BITS = TypeCtx
    witnessSat BAnd          = SatWit
    witnessSat BOr           = SatWit
    witnessSat BXor          = SatWit
    witnessSat Complement    = SatWit

    witnessSat Bit           = SatWit
    witnessSat SetBit        = SatWit
    witnessSat ClearBit      = SatWit
    witnessSat ComplementBit = SatWit
    witnessSat TestBit       = SatWit

    witnessSat ShiftLU       = SatWit
    witnessSat ShiftRU       = SatWit
    witnessSat ShiftL        = SatWit
    witnessSat ShiftR        = SatWit
    witnessSat RotateLU      = SatWit
    witnessSat RotateRU      = SatWit
    witnessSat RotateL       = SatWit
    witnessSat RotateR       = SatWit
    witnessSat ReverseBits   = SatWit

    witnessSat BitScan       = SatWit
    witnessSat BitCount      = SatWit

    witnessSat IsSigned      = SatWit


instance MaybeWitnessSat TypeCtx BITS
  where
    maybeWitnessSat = maybeWitnessSatDefault


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

evalReverseBits :: Bits b => b -> b
evalReverseBits b = revLoop b 0 (0 `asTypeOf` b)
  where
    bSz = bitSize b
    revLoop b i n | i >= bSz    = n
    revLoop b i n | testBit b i = revLoop b (i+1) (setBit n (bSz - i - 1))
    revLoop b i n | otherwise   = revLoop b (i+1) n

evalBitScan :: Bits b => b -> WordN
evalBitScan b =
   if isSigned b
   then scanLoop b (testBit b (bitSize b - 1)) (bitSize b - 2) 0
   else scanLoop b False (bitSize b - 1) 0
  where
    scanLoop b bit i n | i Prelude.< 0              = n
    scanLoop b bit i n | testBit b i Prelude./= bit = n
    scanLoop b bit i n | otherwise                  = scanLoop b bit (i-1) (n+1)

evalBitCount :: Bits b => b -> WordN
evalBitCount b = loop b (bitSize b - 1) 0
  where
    loop b i n | i Prelude.< 0 = n
    loop b i n | testBit b i   = loop b (i-1) (n+1)
    loop b i n | otherwise     = loop b (i-1) n

instance ExprEq   BITS where exprEq = exprEqSem; exprHash = exprHashSem
instance Render   BITS where renderPart = renderPartSem
instance ToTree   BITS
instance Eval     BITS where evaluate = evaluateSem
instance EvalBind BITS where evalBindSym = evalBindSymDefault
instance Sharable BITS

instance AlphaEq dom dom dom env => AlphaEq BITS BITS dom env
  where
    alphaEqSym = alphaEqSymDefault

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

optZero f (a :* b :* Nil)
    | Just 0 <- viewLiteral b = return a
    | otherwise               = constructFeatUnOpt f (a :* b :* Nil)

