{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TypeFamilies #-}

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
import Feldspar.Core.Types (Size(..), Type, TypeF, (:>)(..), Index, Length)
import qualified Feldspar.Core.Types as T
import Feldspar.Core.Tuple
import Feldspar.Range
import Feldspar.Lattice

import Data.Typeable (Typeable)
import qualified Data.Map as M (empty)

sizeProp :: AExpr a -> AExpr a
sizeProp = spA M.empty

spA :: BindEnv -> AExpr a -> AExpr a
-- | Variables and literals
spA vm (_ :& Variable v) = lookupBE "SizeProp.look" vm v
spA vm (_ :& Literal l)  = Info (T.sizeOf l) :& Literal l
-- Top level lambda
spA vm (_ :& Lambda v e)
  | e1@(Info i1 :& _) <- spA (extendBE vm (CBind v $ Info top :& Variable v)) e
  = Info (top, i1) :& Lambda v e1
-- | Applications and lambdas based on head operator
-- | Array
spA vm (_ :& Operator        Parallel :@ a :@ b)      = spLoI  vm        Parallel (:>) a b
spA vm (_ :& Operator      Sequential :@ a :@ b :@ c)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  , ((s,_), c1) <- spLambda2 vm ai1 top c
  = Info (ai1 :> s) :& Operator Sequential :@ a1 :@ b1 :@ c1
spA vm (_ :& Operator          Append :@ a :@ b)      = spApp2 vm          Append appF a b
  where appF (alen :> aelem) (blen :> belem) = alen + blen :> aelem \/ belem
spA vm (_ :& Operator           GetIx :@ a :@ b)      = spApp2 vm           GetIx (\ (_ :> i) _ -> i) a b
spA vm (_ :& Operator           SetIx :@ a :@ b :@ c) = spApp3 vm           SetIx (\ (l :> i) _ j -> l :> i \/ j) a b c
spA vm (_ :& Operator       GetLength :@ a)           = spApp1 vm       GetLength (\ (l :> _) -> l) a
spA vm (_ :& Operator       SetLength :@ a :@ b)      = spApp2 vm       SetLength (\ l (_ :> i) -> l :> i) a b

-- | Binding
spA vm (_ :& Operator             Let :@ a :@ b@(_ :& Lambda v e))
  | a1@(Info ai1 :& _) <- spA vm a
  , e1@(Info ei1 :& _) <- spA (extendBE vm (CBind v $ Info ai1 :& Variable v)) e
  = Info ei1 :& Operator Let :@ a1 :@ (Info (ai1, ei1) :& Lambda v e1)

-- | Bits
spA vm (_ :& Operator            BAnd :@ a :@ b)      = spApp2 vm            BAnd rangeAnd a b
spA vm (_ :& Operator             BOr :@ a :@ b)      = spApp2 vm             BOr rangeOr  a b
spA vm (_ :& Operator            BXor :@ a :@ b)      = spApp2 vm            BXor rangeXor a b
spA vm (_ :& Operator      Complement :@ a)           = spApp1 vm      Complement rangeComplement a
spA vm (_ :& Operator             Bit :@ a)           = spApp1 vm             Bit topF1 a
spA vm (_ :& Operator          SetBit :@ a :@ b)      = spApp2 vm          SetBit topF2 a b
spA vm (_ :& Operator        ClearBit :@ a :@ b)      = spApp2 vm        ClearBit topF2 a b
spA vm (_ :& Operator   ComplementBit :@ a :@ b)      = spApp2 vm   ComplementBit topF2 a b
spA vm (_ :& Operator         TestBit :@ a :@ b)      = spApp2 vm         TestBit topF2 a b
spA vm (_ :& Operator         ShiftLU :@ a :@ b)      = spApp2 vm         ShiftLU rangeShiftLU a b
spA vm (_ :& Operator         ShiftRU :@ a :@ b)      = spApp2 vm         ShiftRU rangeShiftRU a b
spA vm (_ :& Operator          ShiftL :@ a :@ b)      = spApp2 vm          ShiftL topF2 a b
spA vm (_ :& Operator          ShiftR :@ a :@ b)      = spApp2 vm          ShiftR topF2 a b
spA vm (_ :& Operator        RotateLU :@ a :@ b)      = spApp2 vm        RotateLU topF2 a b
spA vm (_ :& Operator        RotateRU :@ a :@ b)      = spApp2 vm        RotateRU topF2 a b
spA vm (_ :& Operator         RotateL :@ a :@ b)      = spApp2 vm         RotateL topF2 a b
spA vm (_ :& Operator         RotateR :@ a :@ b)      = spApp2 vm         RotateR topF2 a b
spA vm (_ :& Operator     ReverseBits :@ a)           = spApp1 vm     ReverseBits topF1 a
spA vm (_ :& Operator         BitScan :@ a)           = spApp1 vm         BitScan topF1 a
spA vm (_ :& Operator        BitCount :@ a)           = spApp1 vm        BitCount topF1 a -- We could do better

-- | Complex
spA vm (_ :& Operator       MkComplex :@ a :@ b)      = spApp2 vm       MkComplex topF2 a b
spA vm (_ :& Operator        RealPart :@ a)           = spApp1 vm        RealPart topF1 a
spA vm (_ :& Operator        ImagPart :@ a)           = spApp1 vm        ImagPart topF1 a
spA vm (_ :& Operator       Conjugate :@ a)           = spApp1 vm       Conjugate topF1 a
spA vm (_ :& Operator         MkPolar :@ a :@ b)      = spApp2 vm         MkPolar topF2 a b
spA vm (_ :& Operator       Magnitude :@ a)           = spApp1 vm       Magnitude topF1 a
spA vm (_ :& Operator           Phase :@ a)           = spApp1 vm           Phase topF1 a
spA vm (_ :& Operator             Cis :@ a)           = spApp1 vm             Cis topF1 a

-- | Condition
spA vm (_ :& Operator       Condition :@ a :@ b :@ c) = spApp3 vm       Condition (const (\/)) a b c

-- | Conversion
spA vm (_ :& Operator             F2I :@ a)           = spApp1 vm             F2I topF1 a
spA vm (_ :& Operator          op@I2N :@ a)           = spApp1 vm             I2N (spI2N op) a
spA vm (_ :& Operator          op@B2I :@ a)           = spApp1 vm             B2I (spB2I op) a
spA vm (_ :& Operator           Round :@ a)           = spApp1 vm           Round topF1 a
spA vm (_ :& Operator         Ceiling :@ a)           = spApp1 vm         Ceiling topF1 a
spA vm (_ :& Operator           Floor :@ a)           = spApp1 vm           Floor topF1 a

-- | Elements
spA vm (_ :& Operator    EMaterialize :@ a :@ b)      = spApp2 vm    EMaterialize (\ _ s -> s) a b
spA vm (_ :& Operator          EWrite :@ a :@ b)      = spApp2 vm          EWrite (:>) a b
spA vm (_ :& Operator           ESkip)
  = Info bot :& Operator ESkip
spA vm (_ :& Operator            EPar :@ a :@ b)      = spApp2 vm            EPar (\/) a b
spA vm (_ :& Operator         EparFor :@ a :@ b)      = spLoI  vm         EparFor (\ _ s -> s) a b

-- | Eq
spA vm (_ :& Operator           Equal :@ a :@ b)      = spApp2 vm           Equal topF2 a b
spA vm (_ :& Operator        NotEqual :@ a :@ b)      = spApp2 vm        NotEqual topF2 a b

-- | Error
spA vm (_ :& Operator       Undefined)
  = Info bot :& Operator Undefined
spA vm (_ :& Operator      (Assert s) :@ a :@ b)      = spApp2 vm      (Assert s) (\ _ s -> s) a b

-- | Floating
spA vm (_ :& Operator              Pi)
  = Info top :& Operator Pi
spA vm (_ :& Operator             Exp :@ a)           = spApp1 vm             Exp topF1 a
spA vm (_ :& Operator            Sqrt :@ a)           = spApp1 vm            Sqrt topF1 a
spA vm (_ :& Operator             Log :@ a)           = spApp1 vm             Log topF1 a
spA vm (_ :& Operator             Pow :@ a :@ b)      = spApp2 vm             Pow topF2 a b
spA vm (_ :& Operator         LogBase :@ a :@ b)      = spApp2 vm         LogBase topF2 a b
spA vm (_ :& Operator             Sin :@ a)           = spApp1 vm             Sin topF1 a
spA vm (_ :& Operator             Tan :@ a)           = spApp1 vm             Tan topF1 a
spA vm (_ :& Operator             Cos :@ a)           = spApp1 vm             Cos topF1 a
spA vm (_ :& Operator            Asin :@ a)           = spApp1 vm            Asin topF1 a
spA vm (_ :& Operator            Atan :@ a)           = spApp1 vm            Atan topF1 a
spA vm (_ :& Operator            Acos :@ a)           = spApp1 vm            Acos topF1 a
spA vm (_ :& Operator            Sinh :@ a)           = spApp1 vm            Sinh topF1 a
spA vm (_ :& Operator            Tanh :@ a)           = spApp1 vm            Tanh topF1 a
spA vm (_ :& Operator            Cosh :@ a)           = spApp1 vm            Cosh topF1 a
spA vm (_ :& Operator           Asinh :@ a)           = spApp1 vm           Asinh topF1 a
spA vm (_ :& Operator           Atanh :@ a)           = spApp1 vm           Atanh topF1 a
spA vm (_ :& Operator           Acosh :@ a)           = spApp1 vm           Acosh topF1 a

-- | Fractional
spA vm (_ :& Operator         DivFrac :@ a :@ b)      = spApp2 vm         DivFrac topF2 a b

-- | Future
spA vm (_ :& Operator        MkFuture :@ a)           = spApp1 vm        MkFuture id a
spA vm (_ :& Operator           Await :@ a)           = spApp1 vm           Await id a

-- | Integral
spA vm (_ :& Operator            Quot :@ a :@ b)      = spApp2 vm            Quot rangeQuot a b
spA vm (_ :& Operator             Rem :@ a :@ b)      = spApp2 vm             Rem rangeRem a b
spA vm (_ :& Operator             Div :@ a :@ b)      = spApp2 vm             Div rangeDiv a b
spA vm (_ :& Operator             Mod :@ a :@ b)      = spApp2 vm             Mod rangeMod a b
spA vm (_ :& Operator            IExp :@ a :@ b)      = spApp2 vm            IExp rangeExp a b

-- | Logic
spA vm (_ :& Operator             And :@ a :@ b)      = spApp2 vm             And topF2 a b
spA vm (_ :& Operator              Or :@ a :@ b)      = spApp2 vm              Or topF2 a b
spA vm (_ :& Operator             Not :@ a)           = spApp1 vm             Not topF1 a

-- | Loop
spA vm (_ :& Operator ForLoop :@ a :@ b :@ c)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  , (s, c1) <- spLambda2 vm ai1 top c
  = Info (bi1 \/ s) :& Operator ForLoop :@ a1 :@ b1 :@ c1

spA vm (_ :& Operator  WhileLoop :@ a :@ b@(_ :& Lambda v1 e1) :@ c@(_ :& Lambda v2 e2))
  | a1@(Info ai1 :& _) <- spA vm a
  , e1'@(Info ei1' :& _) <- spA (extendBE vm (CBind v1 $ Info top :& Variable v1)) e1
  , e2'@(Info ei2' :& _) <- spA (extendBE vm (CBind v2 $ Info top :& Variable v2)) e2
  , b1 <- Info (top, ei1') :& Lambda v1 e1'
  , c1 <- Info (top, ei2') :& Lambda v2 e2'
  = Info (ai1 \/ ei2') :& Operator WhileLoop :@ a1 :@ b1 :@ c1

-- | Mutable
spA vm (_ :& Operator             Run :@ a)           = spApp1 vm             Run id a

-- | MutableArray
spA vm (_ :& Operator          NewArr :@ a :@ b)      = spApp2 vm          NewArr (\ s _ -> s :> top) a b
spA vm (_ :& Operator         NewArr_ :@ a)           = spApp1 vm         NewArr_ (\ s -> s :> top) a
spA vm (_ :& Operator          GetArr :@ a :@ b)      = spApp2 vm          GetArr topF2 a b
spA vm (_ :& Operator          SetArr :@ a :@ b :@ c) = spApp3 vm          SetArr topF3 a b c
spA vm (_ :& Operator       ArrLength :@ a)           = spApp1 vm       ArrLength (\ (l :> _) -> l) a

-- | MutableToPure
spA vm (_ :& Operator RunMutableArray :@ a)           = spApp1 vm RunMutableArray id a
spA vm (_ :& Operator       WithArray :@ a :@ f)      = i :& Operator WithArray :@ a1 :@ f1
  where (i,a1,f1) = spBind vm a f

-- | MutableReference
spA vm (_ :& Operator          NewRef :@ a)           = spApp1 vm          NewRef topF1 a
spA vm (_ :& Operator          GetRef :@ a)           = spApp1 vm          GetRef topF1 a
spA vm (_ :& Operator          SetRef :@ a :@ b)      = spApp2 vm          SetRef topF2 a b
spA vm (_ :& Operator          ModRef :@ a :@ b)      = spApp2 vm          ModRef topF2 a b

-- | Nested tuples
spA vm (_ :& Operator            Cons :@ a :@ b)      = spApp2 vm            Cons (,) a b
spA vm (_ :& Operator             Nil)
  = Info top :& Operator Nil
spA vm (_ :& Operator             Car :@ a)           = spApp1 vm             Car fst a
spA vm (_ :& Operator             Cdr :@ a)           = spApp1 vm             Cdr snd a
spA vm (_ :& Operator             Tup :@ a)           = spApp1 vm             Tup id  a
spA vm (_ :& Operator           UnTup :@ a)           = spApp1 vm           UnTup id  a

-- | NoInline
spA vm (_ :& Operator        NoInline :@ a)           = spApp1 vm        NoInline topF1 a -- Could we not have 'id'?

-- | Num
spA vm (_ :& Operator             Abs :@ a)           = spApp1 vm             Abs abs a
spA vm (_ :& Operator            Sign :@ a)           = spApp1 vm            Sign signum a
spA vm (_ :& Operator             Add :@ a :@ b)      = spApp2 vm             Add (+) a b
spA vm (_ :& Operator             Sub :@ a :@ b)      = spApp2 vm             Sub (-) a b
spA vm (_ :& Operator             Mul :@ a :@ b)      = spApp2 vm             Mul (*) a b

-- | Ord
spA vm (_ :& Operator             LTH :@ a :@ b)      = spApp2 vm             LTH topF2 a b
spA vm (_ :& Operator             GTH :@ a :@ b)      = spApp2 vm             GTH topF2 a b
spA vm (_ :& Operator             LTE :@ a :@ b)      = spApp2 vm             LTE topF2 a b
spA vm (_ :& Operator             GTE :@ a :@ b)      = spApp2 vm             GTE topF2 a b
spA vm (_ :& Operator             Min :@ a :@ b)      = spApp2 vm             Min min a b
spA vm (_ :& Operator             Max :@ a :@ b)      = spApp2 vm             Max max a b

-- | Par
spA vm (_ :& Operator          ParRun :@ a)           = spApp1 vm          ParRun id a
spA vm (_ :& Operator          ParNew)
  = Info top :& Operator ParNew
spA vm (_ :& Operator          ParGet :@ a)           = spApp1 vm          ParGet topF1 a
spA vm (_ :& Operator          ParPut :@ a :@ b)      = spApp2 vm          ParPut topF2 a b
spA vm (_ :& Operator         ParFork :@ a)           = spApp1 vm         ParFork topF1 a
spA vm (_ :& Operator        ParYield)
  = Info top :& Operator ParYield

-- | RealFloat
spA vm (_ :& Operator           Atan2 :@ a :@ b)      = spApp2 vm           Atan2 topF2 a b

-- | Save
spA vm (_ :& Operator            Save :@ a)           = spApp1 vm            Save id a

-- PropSize
spA vm (_ :& Operator    (PropSize f) :@ a :@ b)      = spApp2 vm    (PropSize f) (\ s t -> unEqBox f s /\ t) a b

-- | Switch
spA vm (_ :& Operator          Switch :@ a)           = spApp1 vm          Switch id a

-- | Tuple
spA vm (_ :& Operator Tup0)
  = Info bot :& Operator Tup0

spA vm (_ :& Operator Tup2 :@ a :@ b)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  = Info (ai1, bi1)
    :& Operator Tup2 :@ a1 :@ b1

spA vm (_ :& Operator Tup3 :@ a :@ b :@ c)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  , c1@(Info ci1 :& _) <- spA vm c
  = Info (ai1, bi1, ci1)
    :& Operator Tup3 :@ a1 :@ b1 :@ c1

spA vm (_ :& Operator Tup4 :@ a :@ b :@ c :@ d)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  , c1@(Info ci1 :& _) <- spA vm c
  , d1@(Info di1 :& _) <- spA vm d
  = Info (ai1, bi1, ci1, di1)
    :& Operator Tup4 :@ a1 :@ b1 :@ c1 :@ d1

spA vm (_ :& Operator Tup5 :@ a :@ b :@ c :@ d :@ e)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  , c1@(Info ci1 :& _) <- spA vm c
  , d1@(Info di1 :& _) <- spA vm d
  , e1@(Info ei1 :& _) <- spA vm e
  = Info (ai1, bi1, ci1, di1, ei1)
    :& Operator Tup5 :@ a1 :@ b1 :@ c1 :@ d1 :@ e1

spA vm (_ :& Operator Tup6 :@ a :@ b :@ c :@ d :@ e :@ f)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  , c1@(Info ci1 :& _) <- spA vm c
  , d1@(Info di1 :& _) <- spA vm d
  , e1@(Info ei1 :& _) <- spA vm e
  , f1@(Info fi1 :& _) <- spA vm f
  = Info (ai1, bi1, ci1, di1, ei1, fi1)
    :& Operator Tup6 :@ a1 :@ b1 :@ c1 :@ d1 :@ e1 :@ f1

spA vm (_ :& Operator Tup7 :@ a :@ b :@ c :@ d :@ e :@ f :@ g)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  , c1@(Info ci1 :& _) <- spA vm c
  , d1@(Info di1 :& _) <- spA vm d
  , e1@(Info ei1 :& _) <- spA vm e
  , f1@(Info fi1 :& _) <- spA vm f
  , g1@(Info gi1 :& _) <- spA vm g
  = Info (ai1, bi1, ci1, di1, ei1, fi1, gi1)
    :& Operator Tup7 :@ a1 :@ b1 :@ c1 :@ d1 :@ e1 :@ f1 :@ g1

spA vm (_ :& Operator Tup8 :@ a :@ b :@ c :@ d :@ e :@ f :@ g :@ h)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  , c1@(Info ci1 :& _) <- spA vm c
  , d1@(Info di1 :& _) <- spA vm d
  , e1@(Info ei1 :& _) <- spA vm e
  , f1@(Info fi1 :& _) <- spA vm f
  , g1@(Info gi1 :& _) <- spA vm g
  , h1@(Info hi1 :& _) <- spA vm h
  = Info (ai1, bi1, ci1, di1, ei1, fi1, gi1, hi1)
    :& Operator Tup8 :@ a1 :@ b1 :@ c1 :@ d1 :@ e1 :@ f1 :@ g1 :@ h1

spA vm (_ :& Operator Tup9 :@ a :@ b :@ c :@ d :@ e :@ f :@ g :@ h :@ i)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  , c1@(Info ci1 :& _) <- spA vm c
  , d1@(Info di1 :& _) <- spA vm d
  , e1@(Info ei1 :& _) <- spA vm e
  , f1@(Info fi1 :& _) <- spA vm f
  , g1@(Info gi1 :& _) <- spA vm g
  , h1@(Info hi1 :& _) <- spA vm h
  , i1@(Info ii1 :& _) <- spA vm i
  = Info (ai1, bi1, ci1, di1, ei1, fi1, gi1, hi1, ii1)
    :& Operator Tup9 :@ a1 :@ b1 :@ c1 :@ d1 :@ e1 :@ f1 :@ g1 :@ h1 :@ i1

spA vm (_ :& Operator Tup10 :@ a :@ b :@ c :@ d :@ e :@ f :@ g :@ h :@ i :@ j)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  , c1@(Info ci1 :& _) <- spA vm c
  , d1@(Info di1 :& _) <- spA vm d
  , e1@(Info ei1 :& _) <- spA vm e
  , f1@(Info fi1 :& _) <- spA vm f
  , g1@(Info gi1 :& _) <- spA vm g
  , h1@(Info hi1 :& _) <- spA vm h
  , i1@(Info ii1 :& _) <- spA vm i
  , j1@(Info ji1 :& _) <- spA vm j
  = Info (ai1, bi1, ci1, di1, ei1, fi1, gi1, hi1, ii1, ji1)
    :& Operator Tup10 :@ a1 :@ b1 :@ c1 :@ d1 :@ e1 :@ f1 :@ g1 :@ h1 :@ i1 :@ j1

spA vm (_ :& Operator Tup11 :@ a :@ b :@ c :@ d :@ e :@ f :@ g :@ h :@ i :@ j :@ k)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  , c1@(Info ci1 :& _) <- spA vm c
  , d1@(Info di1 :& _) <- spA vm d
  , e1@(Info ei1 :& _) <- spA vm e
  , f1@(Info fi1 :& _) <- spA vm f
  , g1@(Info gi1 :& _) <- spA vm g
  , h1@(Info hi1 :& _) <- spA vm h
  , i1@(Info ii1 :& _) <- spA vm i
  , j1@(Info ji1 :& _) <- spA vm j
  , k1@(Info ki1 :& _) <- spA vm k
  = Info (ai1, bi1, ci1, di1, ei1, fi1, gi1, hi1, ii1, ji1, ki1)
    :& Operator Tup11 :@ a1 :@ b1 :@ c1 :@ d1 :@ e1 :@ f1 :@ g1 :@ h1 :@ i1 :@ j1 :@ k1

spA vm (_ :& Operator Tup12 :@ a :@ b :@ c :@ d :@ e :@ f :@ g :@ h :@ i :@ j :@ k :@ l)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  , c1@(Info ci1 :& _) <- spA vm c
  , d1@(Info di1 :& _) <- spA vm d
  , e1@(Info ei1 :& _) <- spA vm e
  , f1@(Info fi1 :& _) <- spA vm f
  , g1@(Info gi1 :& _) <- spA vm g
  , h1@(Info hi1 :& _) <- spA vm h
  , i1@(Info ii1 :& _) <- spA vm i
  , j1@(Info ji1 :& _) <- spA vm j
  , k1@(Info ki1 :& _) <- spA vm k
  , l1@(Info li1 :& _) <- spA vm l
  = Info (ai1, bi1, ci1, di1, ei1, fi1, gi1, hi1, ii1, ji1, ki1, li1)
    :& Operator Tup12 :@ a1 :@ b1 :@ c1 :@ d1 :@ e1 :@ f1 :@ g1 :@ h1 :@ i1 :@ j1 :@ k1 :@ l1

spA vm (_ :& Operator Tup13 :@ a :@ b :@ c :@ d :@ e :@ f :@ g :@ h :@ i :@ j :@ k :@ l :@ m)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  , c1@(Info ci1 :& _) <- spA vm c
  , d1@(Info di1 :& _) <- spA vm d
  , e1@(Info ei1 :& _) <- spA vm e
  , f1@(Info fi1 :& _) <- spA vm f
  , g1@(Info gi1 :& _) <- spA vm g
  , h1@(Info hi1 :& _) <- spA vm h
  , i1@(Info ii1 :& _) <- spA vm i
  , j1@(Info ji1 :& _) <- spA vm j
  , k1@(Info ki1 :& _) <- spA vm k
  , l1@(Info li1 :& _) <- spA vm l
  , m1@(Info mi1 :& _) <- spA vm m  
  = Info (ai1, bi1, ci1, di1, ei1, fi1, gi1, hi1, ii1, ji1, ki1, li1, mi1)
    :& Operator Tup13 :@ a1 :@ b1 :@ c1 :@ d1 :@ e1 :@ f1 :@ g1 :@ h1 :@ i1 :@ j1 :@ k1 :@ l1 :@ m1

spA vm (_ :& Operator Tup14 :@ a :@ b :@ c :@ d :@ e :@ f :@ g :@ h :@ i :@ j :@ k :@ l :@ m :@ n)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  , c1@(Info ci1 :& _) <- spA vm c
  , d1@(Info di1 :& _) <- spA vm d
  , e1@(Info ei1 :& _) <- spA vm e
  , f1@(Info fi1 :& _) <- spA vm f
  , g1@(Info gi1 :& _) <- spA vm g
  , h1@(Info hi1 :& _) <- spA vm h
  , i1@(Info ii1 :& _) <- spA vm i
  , j1@(Info ji1 :& _) <- spA vm j
  , k1@(Info ki1 :& _) <- spA vm k
  , l1@(Info li1 :& _) <- spA vm l
  , m1@(Info mi1 :& _) <- spA vm m
  , n1@(Info ni1 :& _) <- spA vm n
  = Info (ai1, bi1, ci1, di1, ei1, fi1, gi1, hi1, ii1, ji1, ki1, li1, mi1, ni1)
    :& Operator Tup14 :@ a1 :@ b1 :@ c1 :@ d1 :@ e1 :@ f1 :@ g1 :@ h1 :@ i1 :@ j1 :@ k1 :@ l1 :@ m1 :@ n1

spA vm (_ :& Operator Tup15 :@ a :@ b :@ c :@ d :@ e :@ f :@ g :@ h :@ i :@ j :@ k :@ l :@ m :@ n :@ o)
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  , c1@(Info ci1 :& _) <- spA vm c
  , d1@(Info di1 :& _) <- spA vm d
  , e1@(Info ei1 :& _) <- spA vm e
  , f1@(Info fi1 :& _) <- spA vm f
  , g1@(Info gi1 :& _) <- spA vm g
  , h1@(Info hi1 :& _) <- spA vm h
  , i1@(Info ii1 :& _) <- spA vm i
  , j1@(Info ji1 :& _) <- spA vm j
  , k1@(Info ki1 :& _) <- spA vm k
  , l1@(Info li1 :& _) <- spA vm l
  , m1@(Info mi1 :& _) <- spA vm m
  , n1@(Info ni1 :& _) <- spA vm n
  , o1@(Info oi1 :& _) <- spA vm o
  = Info (ai1, bi1, ci1, di1, ei1, fi1, gi1, hi1, ii1, ji1, ki1, li1, mi1, ni1, oi1)
    :& Operator Tup15 :@ a1 :@ b1 :@ c1 :@ d1 :@ e1 :@ f1 :@ g1 :@ h1 :@ i1 :@ j1 :@ k1 :@ l1 :@ m1 :@ n1 :@ o1

spA vm (_ :& Operator            Sel1 :@ a)           = spApp1 vm            Sel1 sel1 a
spA vm (_ :& Operator            Sel2 :@ a)           = spApp1 vm            Sel2 sel2 a
spA vm (_ :& Operator            Sel3 :@ a)           = spApp1 vm            Sel3 sel3 a
spA vm (_ :& Operator            Sel4 :@ a)           = spApp1 vm            Sel4 sel4 a
spA vm (_ :& Operator            Sel5 :@ a)           = spApp1 vm            Sel5 sel5 a
spA vm (_ :& Operator            Sel6 :@ a)           = spApp1 vm            Sel6 sel6 a
spA vm (_ :& Operator            Sel7 :@ a)           = spApp1 vm            Sel7 sel7 a
spA vm (_ :& Operator            Sel8 :@ a)           = spApp1 vm            Sel8 sel8 a
spA vm (_ :& Operator            Sel9 :@ a)           = spApp1 vm            Sel9 sel9 a
spA vm (_ :& Operator           Sel10 :@ a)           = spApp1 vm           Sel10 sel10 a
spA vm (_ :& Operator           Sel11 :@ a)           = spApp1 vm           Sel11 sel11 a
spA vm (_ :& Operator           Sel12 :@ a)           = spApp1 vm           Sel12 sel12 a
spA vm (_ :& Operator           Sel13 :@ a)           = spApp1 vm           Sel13 sel13 a
spA vm (_ :& Operator           Sel14 :@ a)           = spApp1 vm           Sel14 sel14 a
spA vm (_ :& Operator           Sel15 :@ a)           = spApp1 vm           Sel15 sel15 a

-- | ConditionM
spA vm (_ :& Operator      ConditionM :@ a :@ b :@ c) = spApp3 vm      ConditionM (const (\/)) a b c

-- | LoopM
spA vm (_ :& Operator           While :@ a :@ b)      = spApp2 vm           While topF2 a b
spA vm (_ :& Operator             For :@ a :@ b)      = spApp2 vm             For topF2 a b

-- | Mutable
spA vm (_ :& Operator          Return :@ a)           = spApp1 vm          Return id a
spA vm (_ :& Operator            Bind :@ a :@ f@(_ :& Lambda v e))
  | a1@(Info ai1 :& _) <- spA vm a
  , e1@(Info ei1 :& _) <- spA (extendBE vm (CBind v $ Info ai1 :& Variable v)) e
  = Info ei1 :& Operator Bind :@ a1 :@ (Info (ai1, ei1) :& Lambda v e1)
spA vm (_ :& Operator            Then :@ a :@ b)      = spApp2 vm            Then (flip const) a b
spA vm (_ :& Operator            When :@ a :@ b)      = spApp2 vm            When (\ _ _ -> top) a b

-- | Help functions

topF1 :: Lattice u => a -> u
topF1 _ = top

topF2 :: Lattice u => a -> b -> u
topF2 _ _ = top

topF3 :: Lattice u => a -> b -> c -> u
topF3 _ _ _ = top

-- | Indexed loops without state (Parallel, EparFor, ...)
spLoI :: TypeF u
      => BindEnv -> Op (Length -> (Index -> b) -> u)
      -> (Size Index -> Size b -> Size u)
      -> AExpr Length -> AExpr (Index -> b)
      -> AExpr u
spLoI vm op f a (_ :& Lambda v e)
  | a1@(i1@(Info ai1) :& _) <- spA vm a
  , e1@(Info ei1 :& _) <- spA (extendBE vm (CBind v $ i1 :& Variable v)) e
  = Info (f ai1 ei1) :& Operator op :@ a1 :@ (Info (ai1, ei1) :& Lambda v e1)

-- | Helper for binds
spBind :: Size a ~ Size b
        => BindEnv -> AExpr a -> AExpr (b -> c) -> (Info c, AExpr a, AExpr (b -> c))
spBind vm a f@(_ :& Lambda v e)
  | a1@(Info i1 :& _) <- spA vm a
  , e1@(Info ei1 :& _) <- spA (extendBE vm (CBind v $ Info i1 :& Variable v)) e
  = (Info ei1, a1, Info (i1, ei1) :& Lambda v e1)

-- | Helper for two levels of lambdas
spLambda2 :: BindEnv -> Size a -> Size b -> AExpr (a -> b -> c) -> (Size c, AExpr (a -> b -> c))
spLambda2 vm s t (_ :& Lambda v (_ :& Lambda w e))
  | vm' <- extendBE vm (CBind v $ Info s :& Variable v)
  , e1@(Info ei1 :& _) <- spA (extendBE vm' (CBind w $ Info t :& Variable w)) e
  = (ei1, Info (s, (t, ei1)) :& Lambda v (Info (t, ei1) :& Lambda w e1))
spLambda2 _  _ _ _ = error "SizeProp.spLambda2: not a lambda abstraction."

-- | Unary applications
spApp1 :: TypeF u
       => BindEnv -> Op (a -> u) -> (Size a -> Size u) -> AExpr a -> AExpr u
spApp1 vm op f a
  | a1@(Info ai1 :& _) <- spA vm a
  = Info (f ai1) :& Operator op :@ a1
 
-- | Binary applications
spApp2 :: TypeF u
       => BindEnv -> Op (a -> b -> u) -> (Size a -> Size b -> Size u)
       -> AExpr a -> AExpr b -> AExpr u
spApp2 vm op f a b
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  = Info (f ai1 bi1) :& Operator op :@ a1 :@ b1

-- | Ternary applications
spApp3 :: TypeF u
       => BindEnv -> Op (a -> b -> c -> u)
       -> (Size a -> Size b -> Size c -> Size u)
       -> AExpr a -> AExpr b -> AExpr c -> AExpr u
spApp3 vm op f a b c
  | a1@(Info ai1 :& _) <- spA vm a
  , b1@(Info bi1 :& _) <- spA vm b
  , c1@(Info ci1 :& _) <- spA vm c
  = Info (f ai1 bi1 ci1) :& Operator op :@ a1 :@ b1 :@ c1

-- | Support functions

spB2I :: Type b => Op (Bool -> b) -> a -> Size b
spB2I op _ = rangeToSize (resultType1 op) $ range 0 1

spI2N :: (Type b, Integral a) => Op (a -> b) -> Range a -> Size b
spI2N op r = rangeToSize (resultType1 op) $ mapMonotonic toInteger r

rangeToSize :: Lattice (Size a) => T.TypeRep a -> Range Integer -> Size a
rangeToSize (T.IntType _ _) r = rangeProp r
rangeToSize _               _ = universal

rangeProp :: forall a . (Bounded a, Integral a) => Range Integer -> Range a
rangeProp (Range l u)
    | withinBounds l && withinBounds u
        = range (fromIntegral l) (fromIntegral u)
    | otherwise = range minBound maxBound
  where withinBounds i = toInteger (minBound :: a) <= i &&
                         i <= toInteger (maxBound :: a)

resultType1 :: Type b => Op (a -> b) -> T.TypeRep b
resultType1 _ = T.typeRep
