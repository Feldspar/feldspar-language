{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}
-- Compilation times are measured in minutes without these warnings
-- disabled in GHC 8.6 and later.
-- https://gitlab.haskell.org/ghc/ghc/-/issues/14987
{-# OPTIONS_GHC -Wno-overlapping-patterns -Wno-incomplete-record-updates #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-incomplete-uni-patterns #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 806
{-# OPTIONS_GHC -Wno-inaccessible-code #-}
#endif

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
import Feldspar.Lattice (Lattice, top, bot, (\/), (/\), universal)

import qualified Data.Map.Strict as M (empty)

sizeProp :: AExpr a -> AExpr a
sizeProp = spA M.empty

spA :: BindEnv -> AExpr a -> AExpr a
-- | Variables and literals
spA vm (_ :& Sym (Variable v)) = lookupBE "SizeProp.look" vm v
spA _ (_ :& Sym l'@(Literal l)) = Info (T.sizeOf l) :& Sym l'
-- Top level lambda
spA vm (_ :& Sym (Lambda v) :@ e)
  | e1@(Info i1 :& _) <- spA (extendBE vm (CBind v $ Info top :& Sym (Variable v))) e
  = Info (top, i1) :& Sym (Lambda v) :@ e1
-- | Applications and lambdas based on head operator
-- | Array
spA vm (_ :& Sym Parallel :@ a :@ (_ :& Sym (Lambda v) :@ e))
  | a1@(i1@(Info ai1) :& _) <- spA vm a
  , e1@(Info ei1 :& _) <- spA (extendBE vm (CBind v $ i1 :& Sym (Variable v))) e
  = Info (ai1 :> ei1) :& Sym Parallel :@ a1 :@ (Info (ai1, ei1) :& Sym (Lambda v) :@ e1)
spA vm (_ :& Sym Sequential :@ a :@ b :@ (_ :& Sym (Lambda v) :@ (_ :& Sym (Lambda w) :@ e)))
  | a1@(Info ai1 :& _) <- spA vm a
  , b1 <- spA vm b
  , vm' <- extendBE vm (CBind v $ Info ai1 :& Sym (Variable v))
  , e1@(Info ei1@(s, _) :& _) <- spA (extendBE vm' (CBind w $ Info top :& Sym (Variable w))) e
  = Info (ai1 :> s) :& Sym Sequential :@ a1 :@ b1 :@ (Info (ai1, (top, ei1)) :& Sym (Lambda v) :@ (Info (top, ei1) :& Sym (Lambda w) :@ e1))
spA vm (_ :& Sym Append :@ a :@ b)
  | a1@(Info (alen :> aelem) :& _) <- spA vm a
  , b1@(Info (blen :> belem) :& _) <- spA vm b
  = Info (alen + blen :> aelem \/ belem) :& Sym Append :@ a1 :@ b1
spA vm (_ :& Sym GetIx :@ a :@ b)
  | a1@(Info (_ :> ai) :& _) <- spA vm a
  = Info ai :& Sym GetIx :@ a1 :@ spA vm b
spA vm (_ :& Sym SetIx :@ a :@ b :@ c)
  | a1@(Info (l :> i) :& _) <- spA vm a
  , c1@(Info ci1 :& _) <- spA vm c
  = Info (l :> i \/ ci1) :& Sym SetIx :@ a1 :@ spA vm b :@ c1
spA vm (_ :& Sym GetLength :@ a)
  | a1@(Info (l :> _) :& _) <- spA vm a
  = Info l :& Sym GetLength :@ a1
spA vm (_ :& Sym SetLength :@ a :@ b)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info (_ :> bi) :& _) <- spA vm b
  = Info (ai1 :> bi) :& Sym SetLength :@ a1 :@ b1

-- | Binding
spA vm (_ :& Sym Let :@ a :@ (_ :& Sym (Lambda v) :@ e))
  | a1@(Info ai1 :& _) <- spA vm a
  , e1@(Info ei1 :& _) <- spA (extendBE vm (CBind v $ Info ai1 :& Sym (Variable v))) e
  = Info ei1 :& Sym Let :@ a1 :@ (Info (ai1, ei1) :& Sym (Lambda v) :@ e1)

-- | Bits
spA vm (_ :& Sym BAnd :@ a :@ b)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  = Info (rangeAnd ai1 bi1) :& Sym BAnd :@ a1 :@ b1
spA vm (_ :& Sym BOr :@ a :@ b)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  = Info (rangeOr ai1 bi1) :& Sym BOr :@ a1 :@ b1
spA vm (_ :& Sym BXor :@ a :@ b)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  = Info (rangeXor ai1 bi1) :& Sym BXor :@ a1 :@ b1
spA vm (_ :& Sym Complement :@ a)
  | a1@(Info ai1 :& _) <- spA vm a
  = Info (rangeComplement ai1) :& Sym Complement :@ a1
spA vm (_ :& Sym Bit :@ a)
  = Info top :& Sym Bit :@ spA vm a
spA vm (_ :& Sym SetBit :@ a :@ b)
  = Info top :& Sym SetBit :@ spA vm a :@ spA vm b
spA vm (_ :& Sym ClearBit :@ a :@ b)
  = Info top :& Sym ClearBit :@ spA vm a :@ spA vm b
spA vm (_ :& Sym ComplementBit :@ a :@ b)
  = Info top :& Sym ComplementBit :@ spA vm a :@ spA vm b
spA vm (_ :& Sym TestBit :@ a :@ b)
  = Info top :& Sym TestBit :@ spA vm a :@ spA vm b
spA vm (_ :& Sym ShiftLU :@ a :@ b)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  = Info (rangeShiftLU ai1 bi1) :& Sym ShiftLU :@ a1 :@ b1
spA vm (_ :& Sym ShiftRU :@ a :@ b)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  = Info (rangeShiftRU ai1 bi1) :& Sym ShiftRU :@ a1 :@ b1
spA vm (_ :& Sym ShiftL :@ a :@ b)
  = Info top :& Sym ShiftL :@ spA vm a :@ spA vm b
spA vm (_ :& Sym ShiftR :@ a :@ b)
  = Info top :& Sym ShiftR :@ spA vm a :@ spA vm b
spA vm (_ :& Sym RotateLU :@ a :@ b)
  = Info top :& Sym RotateLU :@ spA vm a :@ spA vm b
spA vm (_ :& Sym RotateRU :@ a :@ b)
  = Info top :& Sym RotateRU :@ spA vm a :@ spA vm b
spA vm (_ :& Sym RotateL :@ a :@ b)
  = Info top :& Sym RotateL :@ spA vm a :@ spA vm b
spA vm (_ :& Sym RotateR :@ a :@ b)
  = Info top :& Sym RotateR :@ spA vm a :@ spA vm b
spA vm (_ :& Sym ReverseBits :@ a)
  = Info top :& Sym ReverseBits :@ spA vm a
spA vm (_ :& Sym BitScan :@ a)
  = Info top :& Sym BitScan :@ spA vm a
spA vm (_ :& Sym BitCount :@ a)
  = Info top :& Sym BitCount :@ spA vm a -- Info can be improved.

-- | Complex
spA vm (_ :& Sym MkComplex :@ a :@ b)
  = Info top :& Sym MkComplex :@ spA vm a :@ spA vm b
spA vm (_ :& Sym RealPart :@ a)
  = Info top :& Sym RealPart :@ spA vm a
spA vm (_ :& Sym ImagPart :@ a)
  = Info top :& Sym ImagPart :@ spA vm a
spA vm (_ :& Sym Conjugate :@ a)
  = Info top :& Sym Conjugate :@ spA vm a
spA vm (_ :& Sym MkPolar :@ a :@ b)
  = Info top :& Sym MkPolar :@ spA vm a :@ spA vm b
spA vm (_ :& Sym Magnitude :@ a)
  = Info top :& Sym Magnitude :@ spA vm a
spA vm (_ :& Sym Phase :@ a)
  = Info top :& Sym Phase :@ spA vm a
spA vm (_ :& Sym Cis :@ a)
  = Info top :& Sym Cis :@ spA vm a

-- | Condition
spA vm (_ :& Sym Condition :@ a :@ b :@ c)
  | b1@(Info bi1 :& _) <- spA vm b
  , c1@(Info ci1 :& _) <- spA vm c
  = Info (bi1 \/ ci1) :& Sym Condition :@ spA vm a :@ b1 :@ c1

-- | Conversion
spA vm (_ :& Sym F2I :@ a)
  = Info top :& Sym F2I :@ spA vm a
spA vm (_ :& Sym op@I2N :@ a)
  | a1@(Info ai1 :& _) <- spA vm a
  = Info (rangeToSize (resultType op) $ mapMonotonic toInteger ai1)
    :& Sym I2N :@ a1
spA vm (_ :& Sym op@B2I :@ a)
  | a1@(Info ai1 :& _) <- spA vm a
  = Info (rangeToSize (resultType op) $ mapMonotonic (toInteger . fromEnum) ai1)
    :& Sym B2I :@ a1
spA vm (_ :& Sym Round :@ a)
  = Info top :& Sym Round :@ spA vm a
spA vm (_ :& Sym Ceiling :@ a)
  = Info top :& Sym Ceiling :@ spA vm a
spA vm (_ :& Sym Floor :@ a)
  = Info top :& Sym Floor :@ spA vm a

-- | Elements
spA vm (_ :& Sym EMaterialize :@ a :@ b)
  | b1@(Info bi1 :& _) <- spA vm b
  = Info bi1 :& Sym EMaterialize :@ spA vm a :@ b1
spA vm (_ :& Sym EWrite :@ a :@ b)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  = Info (ai1 :> bi1) :& Sym EWrite :@ a1 :@ b1
spA _ (_ :& Sym ESkip)
  = Info bot :& Sym ESkip
spA vm (_ :& Sym EPar :@ a :@ b)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  = Info (ai1 \/ bi1) :& Sym EPar :@ a1 :@ b1
spA vm (_ :& Sym EparFor :@ a :@ (_ :& Sym (Lambda v) :@ e))
  | a1@(i1@(Info ai1) :& _) <- spA vm a
  , e1@(Info ei1 :& _) <- spA (extendBE vm (CBind v $ i1 :& Sym (Variable v))) e
  = Info ei1 :& Sym EparFor :@ a1 :@ (Info (ai1, ei1) :& Sym (Lambda v) :@ e1)

-- | Eq
spA vm (_ :& Sym Equal :@ a :@ b)
  = Info top :& Sym Equal :@ spA vm a :@ spA vm b
spA vm (_ :& Sym NotEqual :@ a :@ b)
  = Info top :& Sym NotEqual :@ spA vm a :@ spA vm b

-- | Error
spA _ (_ :& Sym Undefined)
  = Info bot :& Sym Undefined
spA vm (_ :& Sym (Assert s) :@ a :@ b)
  | b1@(Info bi1 :& _) <- spA vm b
  = Info bi1 :& Sym (Assert s) :@ spA vm a :@ b1

-- | Floating
spA _ (_ :& Sym Pi)
  = Info top :& Sym Pi
spA vm (_ :& Sym Exp :@ a)
  = Info top :& Sym Exp :@ spA vm a
spA vm (_ :& Sym Sqrt :@ a)
  = Info top :& Sym Sqrt :@ spA vm a
spA vm (_ :& Sym Log :@ a)
  = Info top :& Sym Log :@ spA vm a
spA vm (_ :& Sym Pow :@ a :@ b)
  = Info top :& Sym Pow :@ spA vm a :@ spA vm b
spA vm (_ :& Sym LogBase :@ a :@ b)
  = Info top :& Sym LogBase :@ spA vm a :@ spA vm b
spA vm (_ :& Sym Sin :@ a)
  = Info top :& Sym Sin :@ spA vm a
spA vm (_ :& Sym Tan :@ a)
  = Info top :& Sym Tan :@ spA vm a
spA vm (_ :& Sym Cos :@ a)
  = Info top :& Sym Cos :@ spA vm a
spA vm (_ :& Sym Asin :@ a)
  = Info top :& Sym Asin :@ spA vm a
spA vm (_ :& Sym Atan :@ a)
  = Info top :& Sym Atan :@ spA vm a
spA vm (_ :& Sym Acos :@ a)
  = Info top :& Sym Acos :@ spA vm a
spA vm (_ :& Sym Sinh :@ a)
  = Info top :& Sym Sinh :@ spA vm a
spA vm (_ :& Sym Tanh :@ a)
  = Info top :& Sym Tanh :@ spA vm a
spA vm (_ :& Sym Cosh :@ a)
  = Info top :& Sym Cosh :@ spA vm a
spA vm (_ :& Sym Asinh :@ a)
  = Info top :& Sym Asinh :@ spA vm a
spA vm (_ :& Sym Atanh :@ a)
  = Info top :& Sym Atanh :@ spA vm a
spA vm (_ :& Sym Acosh :@ a)
  = Info top :& Sym Acosh :@ spA vm a

-- | Fractional
spA vm (_ :& Sym DivFrac :@ a :@ b)
  = Info top :& Sym DivFrac :@ spA vm a :@ spA vm b

-- | Future
spA vm (_ :& Sym MkFuture :@ a)
  | a1@(Info ai1 :& _) <- spA vm a
  = Info ai1 :& Sym MkFuture :@ a1
spA vm (_ :& Sym Await :@ a)
  | a1@(Info ai1 :& _) <- spA vm a
  = Info ai1 :& Sym Await :@ a1

-- | Integral
spA vm (_ :& Sym Quot :@ a :@ b)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  = Info (rangeQuot ai1 bi1) :& Sym Quot :@ a1 :@ b1
spA vm (_ :& Sym Rem :@ a :@ b)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  = Info (rangeRem ai1 bi1) :& Sym Rem :@ a1 :@ b1
spA vm (_ :& Sym Div :@ a :@ b)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  = Info (rangeDiv ai1 bi1) :& Sym Div :@ a1 :@ b1
spA vm (_ :& Sym Mod :@ a :@ b)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  = Info (rangeMod ai1 bi1) :& Sym Mod :@ a1 :@ b1
spA vm (_ :& Sym IExp :@ a :@ b)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  = Info (rangeExp ai1 bi1) :& Sym IExp :@ a1 :@ b1

-- | Logic
spA vm (_ :& Sym And :@ a :@ b)
  = Info top :& Sym And :@ spA vm a :@ spA vm b
spA vm (_ :& Sym Or :@ a :@ b)
  = Info top :& Sym Or :@ spA vm a :@ spA vm b
spA vm (_ :& Sym Not :@ a)
  = Info top :& Sym Not :@ spA vm a

-- | Loop
spA vm (_ :& Sym ForLoop :@ a :@ b :@ (_ :& Sym (Lambda v) :@ (_ :& Sym (Lambda w) :@ e)))
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  , vm' <- extendBE vm (CBind v $ Info ai1 :& Sym (Variable v))
  , e1@(Info ei1 :& _) <- spA (extendBE vm' (CBind w $ Info top :& Sym (Variable w))) e
  , r1 <- Info (ai1, (top, ei1)) :& Sym (Lambda v) :@ (Info (top, ei1) :& Sym (Lambda w) :@ e1)
  = Info (bi1 \/ ei1):& Sym ForLoop :@ a1 :@ b1 :@ r1

spA vm (_ :& Sym WhileLoop :@ a :@ (_ :& Sym (Lambda v1) :@ e1) :@ (_ :& Sym (Lambda v2) :@ e2))
  | a1@(Info ai1 :& _) <- spA vm a
  , e1'@(Info ei1' :& _) <- spA (extendBE vm (CBind v1 $ Info top :& Sym (Variable v1))) e1
  , e2'@(Info ei2' :& _) <- spA (extendBE vm (CBind v2 $ Info top :& Sym (Variable v2))) e2
  , b1 <- Info (top, ei1') :& Sym (Lambda v1) :@ e1'
  , c1 <- Info (top, ei2') :& Sym (Lambda v2) :@ e2'
  = Info (ai1 \/ ei2') :& Sym WhileLoop :@ a1 :@ b1 :@ c1

-- | Mutable
spA vm (_ :& Sym Run :@ a)
  | a1@(Info ai1 :& _) <- spA vm a
  = Info ai1 :& Sym Run :@ a1

-- | MutableArray
spA vm (_ :& Sym NewArr :@ a :@ b)
  | a1@(Info ai1 :& _) <- spA vm a
  = Info (ai1 :> top) :& Sym NewArr :@ a1 :@ spA vm b
spA vm (_ :& Sym NewArr_ :@ a)
  | a1@(Info ai1 :& _) <- spA vm a
  = Info (ai1 :> top) :& Sym NewArr_ :@ a1
spA vm (_ :& Sym GetArr :@ a :@ b)
  = Info top :& Sym GetArr :@ spA vm a :@ spA vm b
spA vm (_ :& Sym SetArr :@ a :@ b :@ c)
  = Info top :& Sym SetArr :@ spA vm a :@ spA vm b :@ spA vm c
spA vm (_ :& Sym ArrLength :@ a)
  | a1@(Info (l :> _) :& _) <- spA vm a
  = Info l :& Sym ArrLength :@ a1

-- | MutableToPure
spA vm (_ :& Sym RunMutableArray :@ a)
  | a1@(Info ai1 :& _) <- spA vm a
  = Info ai1 :& Sym RunMutableArray :@ a1
spA vm (_ :& Sym WithArray :@ a :@ (_ :& Sym (Lambda v) :@ e))
  | a1@(Info ai1 :& _) <- spA vm a
  , e1@(Info ei1 :& _) <- spA (extendBE vm (CBind v $ Info ai1 :& Sym (Variable v))) e
  = Info ei1:& Sym WithArray :@ a1 :@ (Info (ai1, ei1) :& Sym (Lambda v) :@ e1)

-- | MutableReference
spA vm (_ :& Sym NewRef :@ a)
  = Info top :& Sym NewRef :@ spA vm a
spA vm (_ :& Sym GetRef :@ a)
  = Info top :& Sym GetRef :@ spA vm a
spA vm (_ :& Sym SetRef :@ a :@ b)
  = Info top :& Sym SetRef :@ spA vm a :@ spA vm b
spA vm (_ :& Sym ModRef :@ a :@ b)
  = Info top :& Sym ModRef :@ spA vm a :@ spA vm b

-- | Nested tuples
spA vm (_ :& Sym Cons :@ a :@ b)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  = Info (ai1, bi1) :& Sym Cons :@ a1 :@ b1
spA _ (_ :& Sym Nil)
  = Info top :& Sym Nil
spA vm (_ :& Sym Car :@ a)
  | a1@(Info (ai1, _) :& _) <- spA vm a
  = Info ai1 :& Sym Car :@ a1
spA vm (_ :& Sym Cdr :@ a)
  | a1@(Info (_, ai1) :& _) <- spA vm a
  = Info ai1 :& Sym Cdr :@ a1
spA vm (_ :& Sym Tup :@ a)
  | a1@(Info ai1 :& _) <- spA vm a
  = Info ai1 :& Sym Tup :@ a1

-- | NoInline
spA vm (_ :& Sym NoInline :@ a)
  | a1@(Info ai1 :& _) <- spA vm a
  = Info ai1 :& Sym NoInline :@ a1

-- | Num
spA vm (_ :& Sym Abs :@ a)
  | a1@(Info ai1 :& _) <- spA vm a
  = Info (abs ai1) :& Sym Abs :@ a1
spA vm (_ :& Sym Sign :@ a)
  | a1@(Info ai1 :& _) <- spA vm a
  = Info (signum ai1) :& Sym Sign :@ a1
spA vm (_ :& Sym Add :@ a :@ b)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  = Info (ai1 + bi1) :& Sym Add :@ a1 :@ b1
spA vm (_ :& Sym Sub :@ a :@ b)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  = Info (ai1 - bi1) :& Sym Sub :@ a1 :@ b1
spA vm (_ :& Sym Mul :@ a :@ b)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  = Info (ai1 * bi1) :& Sym Mul :@ a1 :@ b1

-- | Ord
spA vm (_ :& Sym LTH :@ a :@ b)
  = Info top :& Sym LTH :@ spA vm a :@ spA vm b
spA vm (_ :& Sym GTH :@ a :@ b)
  = Info top :& Sym GTH :@ spA vm a :@ spA vm b
spA vm (_ :& Sym LTE :@ a :@ b)
  = Info top :& Sym LTE :@ spA vm a :@ spA vm b
spA vm (_ :& Sym GTE :@ a :@ b)
  = Info top :& Sym GTE :@ spA vm a :@ spA vm b
spA vm (_ :& Sym Min :@ a :@ b)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  = Info (min ai1 bi1) :& Sym Min :@ a1 :@ b1
spA vm (_ :& Sym Max :@ a :@ b)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  = Info (max ai1 bi1) :& Sym Max :@ a1 :@ b1

-- | Par
spA vm (_ :& Sym ParRun :@ a)
  | a1@(Info ai1 :& _) <- spA vm a
  = Info ai1 :& Sym ParRun :@ a1
spA _ (_ :& Sym ParNew)
  = Info top :& Sym ParNew
spA vm (_ :& Sym ParGet :@ a)
  = Info top :& Sym ParGet :@ spA vm a
spA vm (_ :& Sym ParPut :@ a :@ b)
  = Info top :& Sym ParPut :@ spA vm a :@ spA vm b
spA vm (_ :& Sym ParFork :@ a)
  = Info top :& Sym ParFork :@ spA vm a
spA _ (_ :& Sym ParYield)
  = Info top :& Sym ParYield

-- | RealFloat
spA vm (_ :& Sym Atan2 :@ a :@ b)
  = Info top :& Sym Atan2 :@ spA vm a :@ spA vm b

-- | Save
spA vm (_ :& Sym Save :@ a)
  | a1@(Info ai1 :& _) <- spA vm a
  = Info ai1 :& Sym Save :@ a1

-- PropSize
spA vm (_ :& Sym (PropSize f) :@ a :@ b)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  = Info (unEqBox f ai1 /\ bi1) :& Sym (PropSize f) :@ a1 :@ b1

-- | Switch
spA vm (_ :& Sym Switch :@ a)
  | a1@(Info ai1 :& _) <- spA vm a
  = Info ai1 :& Sym Switch :@ a1

-- | ConditionM
spA vm (_ :& Sym ConditionM :@ a :@ b :@ c)
  | b1@(Info bi1 :& _) <- spA vm b
  , c1@(Info ci1 :& _) <- spA vm c
  = Info (bi1 \/ ci1) :& Sym ConditionM :@ spA vm a :@ b1 :@ c1

-- | LoopM
spA vm (_ :& Sym While :@ a :@ b)
  = Info top :& Sym While :@ spA vm a :@ spA vm b
spA vm (_ :& Sym For :@ a :@ b)
  = Info top :& Sym For :@ spA vm a :@ spA vm b

-- | Mutable
spA vm (_ :& Sym Return :@ a)
  | a1@(Info ai1 :& _) <- spA vm a
  = Info ai1 :& Sym Return :@ a1
spA vm (_ :& Sym Bind :@ a :@ (_ :& Sym (Lambda v) :@ e))
  | a1@(Info ai1 :& _) <- spA vm a
  , e1@(Info ei1 :& _) <- spA (extendBE vm (CBind v $ Info ai1 :& Sym (Variable v))) e
  = Info ei1 :& Sym Bind :@ a1 :@ (Info (ai1, ei1) :& Sym (Lambda v) :@ e1)
spA vm (_ :& Sym Then :@ a :@ b)
  | b1@(Info bi1 :& _) <- spA vm b
  = Info bi1 :& Sym Then :@ spA vm a :@ b1
spA vm (_ :& Sym When :@ a :@ b)
  = Info top :& Sym When :@ spA vm a :@ spA vm b

-- | Support functions

rangeToSize :: forall a . Lattice (Size a) => T.TypeRep a -> Range Integer -> Size a
rangeToSize (T.IntType _ _) (Range l u)
    | withinBounds l && withinBounds u
        = range (fromIntegral l) (fromIntegral u)
    | otherwise = universal
  where withinBounds i = toInteger (minBound :: a) <= i &&
                         i <= toInteger (maxBound :: a)
rangeToSize _               _ = top

resultType :: Type b => Op (a -> b) -> T.TypeRep b
resultType _ = T.typeRep
