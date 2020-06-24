{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
-- FIXME: Current data structures make incomplete checking impossible.
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

--
-- Copyright (c) 2019, ERICSSON AB
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

module Feldspar.Core.SizeProp (sizeProp) where

import Feldspar.Core.Representation
import Feldspar.Core.Types (Size, Type, (:>)(..))
import qualified Feldspar.Core.Types as T
import Feldspar.Range
import Feldspar.Lattice (Lattice, top, bot, (\/), (/\))

import qualified Data.Map.Strict as M (empty)

sizeProp :: AExpr a -> AExpr a
sizeProp = spA M.empty

spA :: BindEnv -> AExpr a -> AExpr a
-- | Variables and literals
spA vm (_ :& Variable v) = lookupBE "SizeProp.look" vm v
spA _ (_ :& Operator l'@(Literal l)) = Info (T.sizeOf l) :& Operator l'
-- Top level lambda
spA vm (_ :& Lambda v e)
  | e1@(Info i1 :& _) <- spA (extendBE vm (CBind v $ Info top :& Variable v)) e
  = Info (top, i1) :& Lambda v e1
-- | Applications and lambdas based on head operator
-- | Array
spA vm (_ :& Operator        Parallel :@ a :@ (_ :& Lambda v e))
  | a1@(i1@(Info ai1) :& _) <- spA vm a
  , e1@(Info ei1 :& _) <- spA (extendBE vm (CBind v $ i1 :& Variable v)) e
  = Info (ai1 :> ei1) :& Operator Parallel :@ a1 :@ (Info (ai1, ei1) :& Lambda v e1)
spA vm (_ :& Operator      Sequential :@ a :@ b :@ (_ :& Lambda v (_ :& Lambda w e)))
  | a1@(Info ai1 :& _) <- spA vm a
  , b1 <- spA vm b
  , vm' <- extendBE vm (CBind v $ Info ai1 :& Variable v)
  , e1@(Info ei1@(s, _) :& _) <- spA (extendBE vm' (CBind w $ Info top :& Variable w)) e
  = Info (ai1 :> s) :& Operator Sequential :@ a1 :@ b1 :@ (Info (ai1, (top, ei1)) :& Lambda v (Info (top, ei1) :& Lambda w e1))
spA vm (_ :& Operator          Append :@ a :@ b)
  | a1@(Info (alen :> aelem) :& _) <- spA vm a
  , b1@(Info (blen :> belem) :& _) <- spA vm b
  = Info (alen + blen :> aelem \/ belem) :& Operator Append :@ a1 :@ b1
spA vm (_ :& Operator           GetIx :@ a :@ b)
  | a1@(Info (_ :> ai) :& _) <- spA vm a
  = Info ai :& Operator GetIx :@ a1 :@ spA vm b
spA vm (_ :& Operator           SetIx :@ a :@ b :@ c)
  | a1@(Info (l :> i) :& _) <- spA vm a
  , c1@(Info ci1 :& _) <- spA vm c
  = Info (l :> i \/ ci1) :& Operator SetIx :@ a1 :@ spA vm b :@ c1
spA vm (_ :& Operator       GetLength :@ a)
  | a1@(Info (l :> _) :& _) <- spA vm a
  = Info l :& Operator GetLength :@ a1
spA vm (_ :& Operator       SetLength :@ a :@ b)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info (_ :> bi) :& _) <- spA vm b
  = Info (ai1 :> bi) :& Operator SetLength :@ a1 :@ b1

-- | Binding
spA vm (_ :& Operator             Let :@ a :@ (_ :& Lambda v e))
  | a1@(Info ai1 :& _) <- spA vm a
  , e1@(Info ei1 :& _) <- spA (extendBE vm (CBind v $ Info ai1 :& Variable v)) e
  = Info ei1 :& Operator Let :@ a1 :@ (Info (ai1, ei1) :& Lambda v e1)

-- | Bits
spA vm (_ :& Operator            BAnd :@ a :@ b)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  = Info (rangeAnd ai1 bi1) :& Operator BAnd :@ a1 :@ b1
spA vm (_ :& Operator             BOr :@ a :@ b)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  = Info (rangeOr ai1 bi1) :& Operator BOr :@ a1 :@ b1
spA vm (_ :& Operator            BXor :@ a :@ b)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  = Info (rangeXor ai1 bi1) :& Operator BXor :@ a1 :@ b1
spA vm (_ :& Operator      Complement :@ a)
  | a1@(Info ai1 :& _) <- spA vm a
  = Info (rangeComplement ai1) :& Operator Complement :@ a1
spA vm (_ :& Operator             Bit :@ a)
  = Info top :& Operator Bit :@ spA vm a
spA vm (_ :& Operator          SetBit :@ a :@ b)
  = Info top :& Operator SetBit :@ spA vm a :@ spA vm b
spA vm (_ :& Operator        ClearBit :@ a :@ b)
  = Info top :& Operator ClearBit :@ spA vm a :@ spA vm b
spA vm (_ :& Operator   ComplementBit :@ a :@ b)
  = Info top :& Operator ComplementBit :@ spA vm a :@ spA vm b
spA vm (_ :& Operator         TestBit :@ a :@ b)
  = Info top :& Operator TestBit :@ spA vm a :@ spA vm b
spA vm (_ :& Operator         ShiftLU :@ a :@ b)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  = Info (rangeShiftLU ai1 bi1) :& Operator ShiftLU :@ a1 :@ b1
spA vm (_ :& Operator         ShiftRU :@ a :@ b)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  = Info (rangeShiftRU ai1 bi1) :& Operator ShiftRU :@ a1 :@ b1
spA vm (_ :& Operator          ShiftL :@ a :@ b)
  = Info top :& Operator ShiftL :@ spA vm a :@ spA vm b
spA vm (_ :& Operator          ShiftR :@ a :@ b)
  = Info top :& Operator ShiftR :@ spA vm a :@ spA vm b
spA vm (_ :& Operator        RotateLU :@ a :@ b)
  = Info top :& Operator RotateLU :@ spA vm a :@ spA vm b
spA vm (_ :& Operator        RotateRU :@ a :@ b)
  = Info top :& Operator RotateRU :@ spA vm a :@ spA vm b
spA vm (_ :& Operator         RotateL :@ a :@ b)
  = Info top :& Operator RotateL :@ spA vm a :@ spA vm b
spA vm (_ :& Operator         RotateR :@ a :@ b)
  = Info top :& Operator RotateR :@ spA vm a :@ spA vm b
spA vm (_ :& Operator     ReverseBits :@ a)
  = Info top :& Operator ReverseBits :@ spA vm a
spA vm (_ :& Operator         BitScan :@ a)
  = Info top :& Operator BitScan :@ spA vm a
spA vm (_ :& Operator        BitCount :@ a)
  = Info top :& Operator BitCount :@ spA vm a -- Info can be improved.

-- | Complex
spA vm (_ :& Operator       MkComplex :@ a :@ b)
  = Info top :& Operator MkComplex :@ spA vm a :@ spA vm b
spA vm (_ :& Operator        RealPart :@ a)
  = Info top :& Operator RealPart :@ spA vm a
spA vm (_ :& Operator        ImagPart :@ a)
  = Info top :& Operator ImagPart :@ spA vm a
spA vm (_ :& Operator       Conjugate :@ a)
  = Info top :& Operator Conjugate :@ spA vm a
spA vm (_ :& Operator         MkPolar :@ a :@ b)
  = Info top :& Operator MkPolar :@ spA vm a :@ spA vm b
spA vm (_ :& Operator       Magnitude :@ a)
  = Info top :& Operator Magnitude :@ spA vm a
spA vm (_ :& Operator           Phase :@ a)
  = Info top :& Operator Phase :@ spA vm a
spA vm (_ :& Operator             Cis :@ a)
  = Info top :& Operator Cis :@ spA vm a

-- | Condition
spA vm (_ :& Operator       Condition :@ a :@ b :@ c)
  | b1@(Info bi1 :& _) <- spA vm b
  , c1@(Info ci1 :& _) <- spA vm c
  = Info (bi1 \/ ci1) :& Operator Condition :@ spA vm a :@ b1 :@ c1

-- | Conversion
spA vm (_ :& Operator             F2I :@ a)
  = Info top :& Operator F2I :@ spA vm a
spA vm (_ :& Operator          op@I2N :@ a)
  | a1@(Info ai1 :& _) <- spA vm a
  = Info (spI2N op ai1) :& Operator I2N :@ a1
spA vm (_ :& Operator          op@B2I :@ a)
  | a1@(Info ai1 :& _) <- spA vm a
  = Info (spB2I op ai1) :& Operator B2I :@ a1
spA vm (_ :& Operator           Round :@ a)
  = Info top :& Operator Round :@ spA vm a
spA vm (_ :& Operator         Ceiling :@ a)
  = Info top :& Operator Ceiling :@ spA vm a
spA vm (_ :& Operator           Floor :@ a)
  = Info top :& Operator Floor :@ spA vm a

-- | Elements
spA vm (_ :& Operator    EMaterialize :@ a :@ b)
  | b1@(Info bi1 :& _) <- spA vm b
  = Info bi1 :& Operator EMaterialize :@ spA vm a :@ b1
spA vm (_ :& Operator          EWrite :@ a :@ b)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  = Info (ai1 :> bi1) :& Operator EWrite :@ a1 :@ b1
spA _ (_ :& Operator           ESkip)
  = Info bot :& Operator ESkip
spA vm (_ :& Operator            EPar :@ a :@ b)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  = Info (ai1 \/ bi1) :& Operator EPar :@ a1 :@ b1
spA vm (_ :& Operator         EparFor :@ a :@ (_ :& Lambda v e))
  | a1@(i1@(Info ai1) :& _) <- spA vm a
  , e1@(Info ei1 :& _) <- spA (extendBE vm (CBind v $ i1 :& Variable v)) e
  = Info ei1 :& Operator EparFor :@ a1 :@ (Info (ai1, ei1) :& Lambda v e1)

-- | Eq
spA vm (_ :& Operator           Equal :@ a :@ b)
  = Info top :& Operator Equal :@ spA vm a :@ spA vm b
spA vm (_ :& Operator        NotEqual :@ a :@ b)
  = Info top :& Operator NotEqual :@ spA vm a :@ spA vm b

-- | Error
spA _ (_ :& Operator       Undefined)
  = Info bot :& Operator Undefined
spA vm (_ :& Operator      (Assert s) :@ a :@ b)
  | b1@(Info bi1 :& _) <- spA vm b
  = Info bi1 :& Operator (Assert s) :@ spA vm a :@ b1

-- | Floating
spA _ (_ :& Operator              Pi)
  = Info top :& Operator Pi
spA vm (_ :& Operator             Exp :@ a)
  = Info top :& Operator Exp :@ spA vm a
spA vm (_ :& Operator            Sqrt :@ a)
  = Info top :& Operator Sqrt :@ spA vm a
spA vm (_ :& Operator             Log :@ a)
  = Info top :& Operator Log :@ spA vm a
spA vm (_ :& Operator             Pow :@ a :@ b)
  = Info top :& Operator Pow :@ spA vm a :@ spA vm b
spA vm (_ :& Operator         LogBase :@ a :@ b)
  = Info top :& Operator LogBase :@ spA vm a :@ spA vm b
spA vm (_ :& Operator             Sin :@ a)
  = Info top :& Operator Sin :@ spA vm a
spA vm (_ :& Operator             Tan :@ a)
  = Info top :& Operator Tan :@ spA vm a
spA vm (_ :& Operator             Cos :@ a)
  = Info top :& Operator Cos :@ spA vm a
spA vm (_ :& Operator            Asin :@ a)
  = Info top :& Operator Asin :@ spA vm a
spA vm (_ :& Operator            Atan :@ a)
  = Info top :& Operator Atan :@ spA vm a
spA vm (_ :& Operator            Acos :@ a)
  = Info top :& Operator Acos :@ spA vm a
spA vm (_ :& Operator            Sinh :@ a)
  = Info top :& Operator Sinh :@ spA vm a
spA vm (_ :& Operator            Tanh :@ a)
  = Info top :& Operator Tanh :@ spA vm a
spA vm (_ :& Operator            Cosh :@ a)
  = Info top :& Operator Cosh :@ spA vm a
spA vm (_ :& Operator           Asinh :@ a)
  = Info top :& Operator Asinh :@ spA vm a
spA vm (_ :& Operator           Atanh :@ a)
  = Info top :& Operator Atanh :@ spA vm a
spA vm (_ :& Operator           Acosh :@ a)
  = Info top :& Operator Acosh :@ spA vm a

-- | Fractional
spA vm (_ :& Operator         DivFrac :@ a :@ b)
  = Info top :& Operator DivFrac :@ spA vm a :@ spA vm b

-- | Future
spA vm (_ :& Operator        MkFuture :@ a)
  | a1@(Info ai1 :& _) <- spA vm a
  = Info ai1 :& Operator MkFuture :@ a1
spA vm (_ :& Operator           Await :@ a)
  | a1@(Info ai1 :& _) <- spA vm a
  = Info ai1 :& Operator Await :@ a1

-- | Integral
spA vm (_ :& Operator            Quot :@ a :@ b)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  = Info (rangeQuot ai1 bi1) :& Operator Quot :@ a1 :@ b1
spA vm (_ :& Operator             Rem :@ a :@ b)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  = Info (rangeRem ai1 bi1) :& Operator Rem :@ a1 :@ b1
spA vm (_ :& Operator             Div :@ a :@ b)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  = Info (rangeDiv ai1 bi1) :& Operator Div :@ a1 :@ b1
spA vm (_ :& Operator             Mod :@ a :@ b)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  = Info (rangeMod ai1 bi1) :& Operator Mod :@ a1 :@ b1
spA vm (_ :& Operator            IExp :@ a :@ b)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  = Info (rangeExp ai1 bi1) :& Operator IExp :@ a1 :@ b1

-- | Logic
spA vm (_ :& Operator             And :@ a :@ b)
  = Info top :& Operator And :@ spA vm a :@ spA vm b
spA vm (_ :& Operator              Or :@ a :@ b)
  = Info top :& Operator Or :@ spA vm a :@ spA vm b
spA vm (_ :& Operator             Not :@ a)
  = Info top :& Operator Not :@ spA vm a

-- | Loop
spA vm (_ :& Operator ForLoop :@ a :@ b :@ (_ :& Lambda v (_ :& Lambda w e)))
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  , vm' <- extendBE vm (CBind v $ Info ai1 :& Variable v)
  , e1@(Info ei1 :& _) <- spA (extendBE vm' (CBind w $ Info top :& Variable w)) e
  , r1 <- Info (ai1, (top, ei1)) :& Lambda v (Info (top, ei1) :& Lambda w e1)
  = Info (bi1 \/ ei1):& Operator ForLoop :@ a1 :@ b1 :@ r1

spA vm (_ :& Operator  WhileLoop :@ a :@ (_ :& Lambda v1 e1) :@ (_ :& Lambda v2 e2))
  | a1@(Info ai1 :& _) <- spA vm a
  , e1'@(Info ei1' :& _) <- spA (extendBE vm (CBind v1 $ Info top :& Variable v1)) e1
  , e2'@(Info ei2' :& _) <- spA (extendBE vm (CBind v2 $ Info top :& Variable v2)) e2
  , b1 <- Info (top, ei1') :& Lambda v1 e1'
  , c1 <- Info (top, ei2') :& Lambda v2 e2'
  = Info (ai1 \/ ei2') :& Operator WhileLoop :@ a1 :@ b1 :@ c1

-- | Mutable
spA vm (_ :& Operator             Run :@ a)
  | a1@(Info ai1 :& _) <- spA vm a
  = Info ai1 :& Operator Run :@ a1

-- | MutableArray
spA vm (_ :& Operator          NewArr :@ a :@ b)
  | a1@(Info ai1 :& _) <- spA vm a
  = Info (ai1 :> top) :& Operator NewArr :@ a1 :@ spA vm b
spA vm (_ :& Operator         NewArr_ :@ a)
  | a1@(Info ai1 :& _) <- spA vm a
  = Info (ai1 :> top) :& Operator NewArr_ :@ a1
spA vm (_ :& Operator          GetArr :@ a :@ b)
  = Info top :& Operator GetArr :@ spA vm a :@ spA vm b
spA vm (_ :& Operator          SetArr :@ a :@ b :@ c)
  = Info top :& Operator SetArr :@ spA vm a :@ spA vm b :@ spA vm c
spA vm (_ :& Operator       ArrLength :@ a)
  | a1@(Info (l :> _) :& _) <- spA vm a
  = Info l :& Operator ArrLength :@ a1

-- | MutableToPure
spA vm (_ :& Operator RunMutableArray :@ a)
  | a1@(Info ai1 :& _) <- spA vm a
  = Info ai1 :& Operator RunMutableArray :@ a1
spA vm (_ :& Operator       WithArray :@ a :@ (_ :& Lambda v e))
  | a1@(Info ai1 :& _) <- spA vm a
  , e1@(Info ei1 :& _) <- spA (extendBE vm (CBind v $ Info ai1 :& Variable v)) e
  = Info ei1:& Operator WithArray :@ a1 :@ (Info (ai1, ei1) :& Lambda v e1)

-- | MutableReference
spA vm (_ :& Operator          NewRef :@ a)
  = Info top :& Operator NewRef :@ spA vm a
spA vm (_ :& Operator          GetRef :@ a)
  = Info top :& Operator GetRef :@ spA vm a
spA vm (_ :& Operator          SetRef :@ a :@ b)
  = Info top :& Operator SetRef :@ spA vm a :@ spA vm b
spA vm (_ :& Operator          ModRef :@ a :@ b)
  = Info top :& Operator ModRef :@ spA vm a :@ spA vm b

-- | Nested tuples
spA vm (_ :& Operator            Cons :@ a :@ b)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  = Info (ai1, bi1) :& Operator Cons :@ a1 :@ b1
spA _ (_ :& Operator             Nil)
  = Info top :& Operator Nil
spA vm (_ :& Operator             Car :@ a)
  | a1@(Info (ai1, _) :& _) <- spA vm a
  = Info ai1 :& Operator Car :@ a1
spA vm (_ :& Operator             Cdr :@ a)
  | a1@(Info (_, ai1) :& _) <- spA vm a
  = Info ai1 :& Operator Cdr :@ a1
spA vm (_ :& Operator             Tup :@ a)
  | a1@(Info ai1 :& _) <- spA vm a
  = Info ai1 :& Operator Tup :@ a1

-- | NoInline
spA vm (_ :& Operator        NoInline :@ a)
  | a1@(Info ai1 :& _) <- spA vm a
  = Info ai1 :& Operator NoInline :@ a1

-- | Num
spA vm (_ :& Operator             Abs :@ a)
  | a1@(Info ai1 :& _) <- spA vm a
  = Info (abs ai1) :& Operator Abs :@ a1
spA vm (_ :& Operator            Sign :@ a)
  | a1@(Info ai1 :& _) <- spA vm a
  = Info (signum ai1) :& Operator Sign :@ a1
spA vm (_ :& Operator             Add :@ a :@ b)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  = Info (ai1 + bi1) :& Operator Add :@ a1 :@ b1
spA vm (_ :& Operator             Sub :@ a :@ b)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  = Info (ai1 - bi1) :& Operator Sub :@ a1 :@ b1
spA vm (_ :& Operator             Mul :@ a :@ b)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  = Info (ai1 * bi1) :& Operator Mul :@ a1 :@ b1

-- | Ord
spA vm (_ :& Operator             LTH :@ a :@ b)
  = Info top :& Operator LTH :@ spA vm a :@ spA vm b
spA vm (_ :& Operator             GTH :@ a :@ b)
  = Info top :& Operator GTH :@ spA vm a :@ spA vm b
spA vm (_ :& Operator             LTE :@ a :@ b)
  = Info top :& Operator LTE :@ spA vm a :@ spA vm b
spA vm (_ :& Operator             GTE :@ a :@ b)
  = Info top :& Operator GTE :@ spA vm a :@ spA vm b
spA vm (_ :& Operator             Min :@ a :@ b)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  = Info (min ai1 bi1) :& Operator Min :@ a1 :@ b1
spA vm (_ :& Operator             Max :@ a :@ b)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  = Info (max ai1 bi1) :& Operator Max :@ a1 :@ b1

-- | Par
spA vm (_ :& Operator          ParRun :@ a)
  | a1@(Info ai1 :& _) <- spA vm a
  = Info ai1 :& Operator ParRun :@ a1
spA _ (_ :& Operator          ParNew)
  = Info top :& Operator ParNew
spA vm (_ :& Operator          ParGet :@ a)
  = Info top :& Operator ParGet :@ spA vm a
spA vm (_ :& Operator          ParPut :@ a :@ b)
  = Info top :& Operator ParPut :@ spA vm a :@ spA vm b
spA vm (_ :& Operator         ParFork :@ a)
  = Info top :& Operator ParFork :@ spA vm a
spA _ (_ :& Operator        ParYield)
  = Info top :& Operator ParYield

-- | RealFloat
spA vm (_ :& Operator           Atan2 :@ a :@ b)
  = Info top :& Operator Atan2 :@ spA vm a :@ spA vm b

-- | Save
spA vm (_ :& Operator            Save :@ a)
  | a1@(Info ai1 :& _) <- spA vm a
  = Info ai1 :& Operator Save :@ a1

-- PropSize
spA vm (_ :& Operator    (PropSize f) :@ a :@ b)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  = Info (unEqBox f ai1 /\ bi1) :& Operator (PropSize f) :@ a1 :@ b1

-- | Switch
spA vm (_ :& Operator          Switch :@ a)
  | a1@(Info ai1 :& _) <- spA vm a
  = Info ai1 :& Operator Switch :@ a1

-- | ConditionM
spA vm (_ :& Operator      ConditionM :@ a :@ b :@ c)
  | b1@(Info bi1 :& _) <- spA vm b
  , c1@(Info ci1 :& _) <- spA vm c
  = Info (bi1 \/ ci1) :& Operator ConditionM :@ spA vm a :@ b1 :@ c1

-- | LoopM
spA vm (_ :& Operator           While :@ a :@ b)
  = Info top :& Operator While :@ spA vm a :@ spA vm b
spA vm (_ :& Operator             For :@ a :@ b)
  = Info top :& Operator For :@ spA vm a :@ spA vm b

-- | Mutable
spA vm (_ :& Operator          Return :@ a)
  | a1@(Info ai1 :& _) <- spA vm a
  = Info ai1 :& Operator Return :@ a1
spA vm (_ :& Operator            Bind :@ a :@ (_ :& Lambda v e))
  | a1@(Info ai1 :& _) <- spA vm a
  , e1@(Info ei1 :& _) <- spA (extendBE vm (CBind v $ Info ai1 :& Variable v)) e
  = Info ei1 :& Operator Bind :@ a1 :@ (Info (ai1, ei1) :& Lambda v e1)
spA vm (_ :& Operator            Then :@ a :@ b)
  | b1@(Info bi1 :& _) <- spA vm b
  = Info bi1 :& Operator Then :@ spA vm a :@ b1
spA vm (_ :& Operator            When :@ a :@ b)
  = Info top :& Operator When :@ spA vm a :@ spA vm b

-- | Support functions

spB2I :: Type b => Op (Bool -> b) -> Range Bool -> Size b
spB2I op r = rangeToSize (resultType op) $ mapMonotonic (toInteger . fromEnum) r

spI2N :: (Type b, Integral a) => Op (a -> b) -> Range a -> Size b
spI2N op r = rangeToSize (resultType op) $ mapMonotonic toInteger r

rangeToSize :: Lattice (Size a) => T.TypeRep a -> Range Integer -> Size a
rangeToSize (T.IntType _ _) r = rangeProp r
rangeToSize _               _ = top

rangeProp :: forall a . (Bounded a, Integral a) => Range Integer -> Range a
rangeProp (Range l u)
    | withinBounds l && withinBounds u
        = range (fromIntegral l) (fromIntegral u)
    | otherwise = range minBound maxBound
  where withinBounds i = toInteger (minBound :: a) <= i &&
                         i <= toInteger (maxBound :: a)

resultType :: Type b => Op (a -> b) -> T.TypeRep b
resultType _ = T.typeRep
