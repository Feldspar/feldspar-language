{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

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

look :: Typeable a => BindEnv -> Var a -> AExpr a
look vm v = lookupBE "SizeProp.look" vm v

extend :: TypeF a => BindEnv -> Var a -> Info a -> BindEnv
extend vm v info = extendBE vm $ CBind v $ info :& Variable v

sizeProp :: (Show (Size a), Lattice (Size a), Typeable a) => AExpr a -> AExpr a
sizeProp = spA M.empty

spA :: (Show (Size a), Lattice (Size a), Typeable a)
    => BindEnv -> AExpr a -> AExpr a
spA vm (_ :& e) = spApp vm e

spApp :: (Show (Size a), Lattice (Size a), Typeable a) => BindEnv -> Expr a -> AExpr a
-- | Variables and literals
spApp vm (Variable v) = look vm v
spApp vm (Literal l)  = literal l
-- Top level lambda
spApp vm (Lambda v e) = snd $ spLambda vm top (Info top :& Lambda v e)
-- | Applications and lambdas based on head operator
-- | Array
spApp vm (Operator        Parallel :@ a :@ b)      = spLoI  vm        Parallel (:>) a b
spApp vm (Operator      Sequential :@ a :@ b :@ c) = Info (exprSize a1 :> s) :& Operator Sequential :@ a1 :@ b1 :@ c1
  where a1 = spA vm a
        b1 = spA vm b
        ((s,_),c1) = spLambda2 vm (exprSize a1) top c
spApp vm (Operator          Append :@ a :@ b)      = spApp2 vm          Append appF a b
  where appF (alen :> aelem) (blen :> belem) = (alen + blen :> aelem \/ belem)
spApp vm (Operator           GetIx :@ a :@ b)      = spApp2 vm           GetIx (\ (_ :> i) _ -> i) a b
spApp vm (Operator           SetIx :@ a :@ b :@ c) = spApp3 vm           SetIx (\ (l :> i) _ j -> l :> i \/ j) a b c
spApp vm (Operator       GetLength :@ a)           = spApp1 vm       GetLength (\ (l :> _) -> l) a
spApp vm (Operator       SetLength :@ a :@ b)      = spApp2 vm       SetLength (\ l (_ :> i) -> l :> i) a b

-- | Binding
spApp vm (Operator             Let :@ a :@ b)      = i :& Operator Let :@ a1 :@ b1
  where (i,a1,b1) = spBind vm a b

-- | Bits
spApp vm (Operator            BAnd :@ a :@ b)      = spApp2 vm            BAnd rangeAnd a b
spApp vm (Operator             BOr :@ a :@ b)      = spApp2 vm             BOr rangeOr  a b
spApp vm (Operator            BXor :@ a :@ b)      = spApp2 vm            BXor rangeXor a b
spApp vm (Operator      Complement :@ a)           = spApp1 vm      Complement rangeComplement a
spApp vm (Operator             Bit :@ a)           = spApp1 vm             Bit topF1 a
spApp vm (Operator          SetBit :@ a :@ b)      = spApp2 vm          SetBit topF2 a b
spApp vm (Operator        ClearBit :@ a :@ b)      = spApp2 vm        ClearBit topF2 a b
spApp vm (Operator   ComplementBit :@ a :@ b)      = spApp2 vm   ComplementBit topF2 a b
spApp vm (Operator         TestBit :@ a :@ b)      = spApp2 vm         TestBit topF2 a b
spApp vm (Operator         ShiftLU :@ a :@ b)      = spApp2 vm         ShiftLU rangeShiftLU a b
spApp vm (Operator         ShiftRU :@ a :@ b)      = spApp2 vm         ShiftRU rangeShiftRU a b
spApp vm (Operator          ShiftL :@ a :@ b)      = spApp2 vm          ShiftL topF2 a b
spApp vm (Operator          ShiftR :@ a :@ b)      = spApp2 vm          ShiftR topF2 a b
spApp vm (Operator        RotateLU :@ a :@ b)      = spApp2 vm        RotateLU topF2 a b
spApp vm (Operator        RotateRU :@ a :@ b)      = spApp2 vm        RotateRU topF2 a b
spApp vm (Operator         RotateL :@ a :@ b)      = spApp2 vm         RotateL topF2 a b
spApp vm (Operator         RotateR :@ a :@ b)      = spApp2 vm         RotateR topF2 a b
spApp vm (Operator     ReverseBits :@ a)           = spApp1 vm     ReverseBits topF1 a
spApp vm (Operator         BitScan :@ a)           = spApp1 vm         BitScan topF1 a
spApp vm (Operator        BitCount :@ a)           = spApp1 vm        BitCount topF1 a -- We could do better

-- | Complex
spApp vm (Operator       MkComplex :@ a :@ b)      = spApp2 vm       MkComplex topF2 a b
spApp vm (Operator        RealPart :@ a)           = spApp1 vm        RealPart topF1 a
spApp vm (Operator        ImagPart :@ a)           = spApp1 vm        ImagPart topF1 a
spApp vm (Operator       Conjugate :@ a)           = spApp1 vm       Conjugate topF1 a
spApp vm (Operator         MkPolar :@ a :@ b)      = spApp2 vm         MkPolar topF2 a b
spApp vm (Operator       Magnitude :@ a)           = spApp1 vm       Magnitude topF1 a
spApp vm (Operator           Phase :@ a)           = spApp1 vm           Phase topF1 a
spApp vm (Operator             Cis :@ a)           = spApp1 vm             Cis topF1 a

-- | Condition
spApp vm (Operator       Condition :@ a :@ b :@ c) = spApp3 vm       Condition (const (\/)) a b c

-- | Conversion
spApp vm (Operator             F2I :@ a)           = spApp1 vm             F2I topF1 a
spApp vm (Operator          op@I2N :@ a)           = spApp1 vm             I2N (spI2N op) a
spApp vm (Operator          op@B2I :@ a)           = spApp1 vm             B2I (spB2I op) a
spApp vm (Operator           Round :@ a)           = spApp1 vm           Round topF1 a
spApp vm (Operator         Ceiling :@ a)           = spApp1 vm         Ceiling topF1 a
spApp vm (Operator           Floor :@ a)           = spApp1 vm           Floor topF1 a

-- | Elements
spApp vm (Operator    EMaterialize :@ a :@ b)      = spApp2 vm    EMaterialize (\ _ s -> s) a b
spApp vm (Operator          EWrite :@ a :@ b)      = spApp2 vm          EWrite (:>) a b
spApp vm (Operator           ESkip)                = spApp0 vm           ESkip bot
spApp vm (Operator            EPar :@ a :@ b)      = spApp2 vm            EPar (\/) a b
spApp vm (Operator         EparFor :@ a :@ b)      = spLoI  vm         EparFor (\ _ s -> s) a b

-- | Eq
spApp vm (Operator           Equal :@ a :@ b)      = spApp2 vm           Equal topF2 a b
spApp vm (Operator        NotEqual :@ a :@ b)      = spApp2 vm        NotEqual topF2 a b

-- | Error
spApp vm (Operator       Undefined)                = spApp0 vm       Undefined bot
spApp vm (Operator      (Assert s) :@ a :@ b)      = spApp2 vm      (Assert s) (\ _ s -> s) a b

-- | Floating
spApp vm (Operator              Pi)                = spApp0 vm              Pi top
spApp vm (Operator             Exp :@ a)           = spApp1 vm             Exp topF1 a
spApp vm (Operator            Sqrt :@ a)           = spApp1 vm            Sqrt topF1 a
spApp vm (Operator             Log :@ a)           = spApp1 vm             Log topF1 a
spApp vm (Operator             Pow :@ a :@ b)      = spApp2 vm             Pow topF2 a b
spApp vm (Operator         LogBase :@ a :@ b)      = spApp2 vm         LogBase topF2 a b
spApp vm (Operator             Sin :@ a)           = spApp1 vm             Sin topF1 a
spApp vm (Operator             Tan :@ a)           = spApp1 vm             Tan topF1 a
spApp vm (Operator             Cos :@ a)           = spApp1 vm             Cos topF1 a
spApp vm (Operator            Asin :@ a)           = spApp1 vm            Asin topF1 a
spApp vm (Operator            Atan :@ a)           = spApp1 vm            Atan topF1 a
spApp vm (Operator            Acos :@ a)           = spApp1 vm            Acos topF1 a
spApp vm (Operator            Sinh :@ a)           = spApp1 vm            Sinh topF1 a
spApp vm (Operator            Tanh :@ a)           = spApp1 vm            Tanh topF1 a
spApp vm (Operator            Cosh :@ a)           = spApp1 vm            Cosh topF1 a
spApp vm (Operator           Asinh :@ a)           = spApp1 vm           Asinh topF1 a
spApp vm (Operator           Atanh :@ a)           = spApp1 vm           Atanh topF1 a
spApp vm (Operator           Acosh :@ a)           = spApp1 vm           Acosh topF1 a

-- | Fractional
spApp vm (Operator         DivFrac :@ a :@ b)      = spApp2 vm         DivFrac topF2 a b

-- | Future
spApp vm (Operator        MkFuture :@ a)           = spApp1 vm        MkFuture id a
spApp vm (Operator           Await :@ a)           = spApp1 vm           Await id a

-- | Integral
spApp vm (Operator            Quot :@ a :@ b)      = spApp2 vm            Quot rangeQuot a b
spApp vm (Operator             Rem :@ a :@ b)      = spApp2 vm             Rem rangeRem a b
spApp vm (Operator             Div :@ a :@ b)      = spApp2 vm             Div rangeDiv a b
spApp vm (Operator             Mod :@ a :@ b)      = spApp2 vm             Mod rangeMod a b
spApp vm (Operator            IExp :@ a :@ b)      = spApp2 vm            IExp rangeExp a b

-- | Logic
spApp vm (Operator             And :@ a :@ b)      = spApp2 vm             And topF2 a b
spApp vm (Operator              Or :@ a :@ b)      = spApp2 vm              Or topF2 a b
spApp vm (Operator             Not :@ a)           = spApp1 vm             Not topF1 a

-- | Loop
spApp vm (Operator ForLoop :@ a :@ b :@ c) = finalInfo b1 s :& Operator ForLoop :@ a1 :@ b1 :@ c1
  where a1 = spA vm a
        b1 = spA vm b
        (s,c1) = spLambda2 vm (exprSize a1) top c

spApp vm (Operator  WhileLoop :@ a :@ b :@ c) = finalInfo a1 s :& Operator WhileLoop :@ a1 :@ b1 :@ c1
  where a1 = spA vm a
        (_,b1) = spLambda vm top b
        (s,c1) = spLambda vm top c

-- | Mutable
spApp vm (Operator             Run :@ a)           = spApp1 vm             Run id a

-- | MutableArray
spApp vm (Operator          NewArr :@ a :@ b)      = spApp2 vm          NewArr (\ s _ -> s :> top) a b
spApp vm (Operator         NewArr_ :@ a)           = spApp1 vm         NewArr_ (\ s -> s :> top) a
spApp vm (Operator          GetArr :@ a :@ b)      = spApp2 vm          GetArr topF2 a b
spApp vm (Operator          SetArr :@ a :@ b :@ c) = spApp3 vm          SetArr topF3 a b c
spApp vm (Operator       ArrLength :@ a)           = spApp1 vm       ArrLength (\ (l :> _) -> l) a

-- | MutableToPure
spApp vm (Operator RunMutableArray :@ a)           = spApp1 vm RunMutableArray id a
spApp vm (Operator       WithArray :@ a :@ f)      = i :& Operator WithArray :@ a1 :@ f1
  where (i,a1,f1) = spBind vm a f

-- | MutableReference
spApp vm (Operator          NewRef :@ a)           = spApp1 vm          NewRef topF1 a
spApp vm (Operator          GetRef :@ a)           = spApp1 vm          GetRef topF1 a
spApp vm (Operator          SetRef :@ a :@ b)      = spApp2 vm          SetRef topF2 a b
spApp vm (Operator          ModRef :@ a :@ b)      = spApp2 vm          ModRef topF2 a b

-- | Nested tuples
spApp vm (Operator            Cons :@ a :@ b)      = spApp2 vm            Cons (,) a b
spApp vm (Operator             Nil)                = spApp0 vm             Nil top
spApp vm (Operator             Car :@ a)           = spApp1 vm             Car fst a
spApp vm (Operator             Cdr :@ a)           = spApp1 vm             Cdr snd a
spApp vm (Operator             Tup :@ a)           = spApp1 vm             Tup id  a
spApp vm (Operator           UnTup :@ a)           = spApp1 vm           UnTup id  a

-- | NoInline
spApp vm (Operator        NoInline :@ a)           = spApp1 vm        NoInline topF1 a -- Could we not have 'id'?

-- | Num
spApp vm (Operator             Abs :@ a)           = spApp1 vm             Abs abs a
spApp vm (Operator            Sign :@ a)           = spApp1 vm            Sign signum a
spApp vm (Operator             Add :@ a :@ b)      = spApp2 vm             Add (+) a b
spApp vm (Operator             Sub :@ a :@ b)      = spApp2 vm             Sub (-) a b
spApp vm (Operator             Mul :@ a :@ b)      = spApp2 vm             Mul (*) a b

-- | Ord
spApp vm (Operator             LTH :@ a :@ b)      = spApp2 vm             LTH topF2 a b
spApp vm (Operator             GTH :@ a :@ b)      = spApp2 vm             GTH topF2 a b
spApp vm (Operator             LTE :@ a :@ b)      = spApp2 vm             LTE topF2 a b
spApp vm (Operator             GTE :@ a :@ b)      = spApp2 vm             GTE topF2 a b
spApp vm (Operator             Min :@ a :@ b)      = spApp2 vm             Min min a b
spApp vm (Operator             Max :@ a :@ b)      = spApp2 vm             Max max a b

-- | Par
spApp vm (Operator          ParRun :@ a)           = spApp1 vm          ParRun id a
spApp vm (Operator          ParNew)                = spApp0 vm          ParNew top
spApp vm (Operator          ParGet :@ a)           = spApp1 vm          ParGet topF1 a
spApp vm (Operator          ParPut :@ a :@ b)      = spApp2 vm          ParPut topF2 a b
spApp vm (Operator         ParFork :@ a)           = spApp1 vm         ParFork topF1 a
spApp vm (Operator        ParYield)                = spApp0 vm        ParYield top

-- | RealFloat
spApp vm (Operator           Atan2 :@ a :@ b)      = spApp2 vm           Atan2 topF2 a b

-- | Save
spApp vm (Operator            Save :@ a)           = spApp1 vm            Save id a

-- PropSize
spApp vm (Operator    (PropSize f) :@ a :@ b)      = spApp2 vm    (PropSize f) (\ s t -> unEqBox f s /\ t) a b

-- | Switch
spApp vm (Operator          Switch :@ a)           = spApp1 vm          Switch id a

-- | Tuple
spApp vm (Operator Tup0) = Info bot :& Operator Tup0

spApp vm (Operator Tup2 :@ a :@ b)
  = Info (s a1, s b1)
    :& Operator Tup2 :@ a1 :@ b1
  where a1 = spA vm a
        b1 = spA vm b
        s e = exprSize e

spApp vm (Operator Tup3 :@ a :@ b :@ c)
  = Info (s a1, s b1, s c1)
    :& Operator Tup3 :@ a1 :@ b1 :@ c1
  where a1 = spA vm a
        b1 = spA vm b
        c1 = spA vm c
        s e = exprSize e

spApp vm (Operator Tup4 :@ a :@ b :@ c :@ d)
  = Info (s a1, s b1, s c1, s d1)
    :& Operator Tup4 :@ a1 :@ b1 :@ c1 :@ d1
  where a1 = spA vm a
        b1 = spA vm b
        c1 = spA vm c
        d1 = spA vm d
        s e = exprSize e

spApp vm (Operator Tup5 :@ a :@ b :@ c :@ d :@ e)
  = Info (s a1, s b1, s c1, s d1, s e1)
    :& Operator Tup5 :@ a1 :@ b1 :@ c1 :@ d1 :@ e1
  where a1 = spA vm a
        b1 = spA vm b
        c1 = spA vm c
        d1 = spA vm d
        e1 = spA vm e
        s e = exprSize e

spApp vm (Operator Tup6 :@ a :@ b :@ c :@ d :@ e :@ f)
  = Info (s a1, s b1, s c1, s d1, s e1, s f1)
    :& Operator Tup6 :@ a1 :@ b1 :@ c1 :@ d1 :@ e1 :@ f1
  where a1 = spA vm a
        b1 = spA vm b
        c1 = spA vm c
        d1 = spA vm d
        e1 = spA vm e
        f1 = spA vm f
        s e = exprSize e

spApp vm (Operator Tup7 :@ a :@ b :@ c :@ d :@ e :@ f :@ g)
  = Info (s a1, s b1, s c1, s d1, s e1, s f1, s g1)
    :& Operator Tup7 :@ a1 :@ b1 :@ c1 :@ d1 :@ e1 :@ f1 :@ g1
  where a1 = spA vm a
        b1 = spA vm b
        c1 = spA vm c
        d1 = spA vm d
        e1 = spA vm e
        f1 = spA vm f
        g1 = spA vm g
        s e = exprSize e

spApp vm (Operator Tup8 :@ a :@ b :@ c :@ d :@ e :@ f :@ g :@ h)
  = Info (s a1, s b1, s c1, s d1, s e1, s f1, s g1, s h1)
    :& Operator Tup8 :@ a1 :@ b1 :@ c1 :@ d1 :@ e1 :@ f1 :@ g1 :@ h1
  where a1 = spA vm a
        b1 = spA vm b
        c1 = spA vm c
        d1 = spA vm d
        e1 = spA vm e
        f1 = spA vm f
        g1 = spA vm g
        h1 = spA vm h
        s e = exprSize e

spApp vm (Operator Tup9 :@ a :@ b :@ c :@ d :@ e :@ f :@ g :@ h :@ i)
  = Info (s a1, s b1, s c1, s d1, s e1, s f1, s g1, s h1, s i1)
    :& Operator Tup9 :@ a1 :@ b1 :@ c1 :@ d1 :@ e1 :@ f1 :@ g1 :@ h1 :@ i1
  where a1 = spA vm a
        b1 = spA vm b
        c1 = spA vm c
        d1 = spA vm d
        e1 = spA vm e
        f1 = spA vm f
        g1 = spA vm g
        h1 = spA vm h
        i1 = spA vm i
        s e = exprSize e

spApp vm (Operator Tup10 :@ a :@ b :@ c :@ d :@ e :@ f :@ g :@ h :@ i :@ j)
  = Info (s a1, s b1, s c1, s d1, s e1, s f1, s g1, s h1, s i1, s j1)
    :& Operator Tup10 :@ a1 :@ b1 :@ c1 :@ d1 :@ e1 :@ f1 :@ g1 :@ h1 :@ i1 :@ j1
  where a1 = spA vm a
        b1 = spA vm b
        c1 = spA vm c
        d1 = spA vm d
        e1 = spA vm e
        f1 = spA vm f
        g1 = spA vm g
        h1 = spA vm h
        i1 = spA vm i
        j1 = spA vm j
        s e = exprSize e

spApp vm (Operator Tup11 :@ a :@ b :@ c :@ d :@ e :@ f :@ g :@ h :@ i :@ j :@ k)
  = Info (s a1, s b1, s c1, s d1, s e1, s f1, s g1, s h1, s i1, s j1, s k1)
    :& Operator Tup11 :@ a1 :@ b1 :@ c1 :@ d1 :@ e1 :@ f1 :@ g1 :@ h1 :@ i1 :@ j1 :@ k1
  where a1 = spA vm a
        b1 = spA vm b
        c1 = spA vm c
        d1 = spA vm d
        e1 = spA vm e
        f1 = spA vm f
        g1 = spA vm g
        h1 = spA vm h
        i1 = spA vm i
        j1 = spA vm j
        k1 = spA vm k
        s e = exprSize e

spApp vm (Operator Tup12 :@ a :@ b :@ c :@ d :@ e :@ f :@ g :@ h :@ i :@ j :@ k :@ l)
  = Info (s a1, s b1, s c1, s d1, s e1, s f1, s g1, s h1, s i1, s j1, s k1, s l1)
    :& Operator Tup12 :@ a1 :@ b1 :@ c1 :@ d1 :@ e1 :@ f1 :@ g1 :@ h1 :@ i1 :@ j1 :@ k1 :@ l1
  where a1 = spA vm a
        b1 = spA vm b
        c1 = spA vm c
        d1 = spA vm d
        e1 = spA vm e
        f1 = spA vm f
        g1 = spA vm g
        h1 = spA vm h
        i1 = spA vm i
        j1 = spA vm j
        k1 = spA vm k
        l1 = spA vm l
        s e = exprSize e

spApp vm (Operator Tup13 :@ a :@ b :@ c :@ d :@ e :@ f :@ g :@ h :@ i :@ j :@ k :@ l :@ m)
  = Info (s a1, s b1, s c1, s d1, s e1, s f1, s g1, s h1, s i1, s j1, s k1, s l1, s m1)
    :& Operator Tup13 :@ a1 :@ b1 :@ c1 :@ d1 :@ e1 :@ f1 :@ g1 :@ h1 :@ i1 :@ j1 :@ k1 :@ l1 :@ m1
  where a1 = spA vm a
        b1 = spA vm b
        c1 = spA vm c
        d1 = spA vm d
        e1 = spA vm e
        f1 = spA vm f
        g1 = spA vm g
        h1 = spA vm h
        i1 = spA vm i
        j1 = spA vm j
        k1 = spA vm k
        l1 = spA vm l
        m1 = spA vm m
        s e = exprSize e

spApp vm (Operator Tup14 :@ a :@ b :@ c :@ d :@ e :@ f :@ g :@ h :@ i :@ j :@ k :@ l :@ m :@ n)
  = Info (s a1, s b1, s c1, s d1, s e1, s f1, s g1, s h1, s i1, s j1, s k1, s l1, s m1, s n1)
    :& Operator Tup14 :@ a1 :@ b1 :@ c1 :@ d1 :@ e1 :@ f1 :@ g1 :@ h1 :@ i1 :@ j1 :@ k1 :@ l1 :@ m1 :@ n1
  where a1 = spA vm a
        b1 = spA vm b
        c1 = spA vm c
        d1 = spA vm d
        e1 = spA vm e
        f1 = spA vm f
        g1 = spA vm g
        h1 = spA vm h
        i1 = spA vm i
        j1 = spA vm j
        k1 = spA vm k
        l1 = spA vm l
        m1 = spA vm m
        n1 = spA vm n
        s e = exprSize e

spApp vm (Operator Tup15 :@ a :@ b :@ c :@ d :@ e :@ f :@ g :@ h :@ i :@ j :@ k :@ l :@ m :@ n :@ o)
  = Info (s a1, s b1, s c1, s d1, s e1, s f1, s g1, s h1, s i1, s j1, s k1, s l1, s m1, s n1, s o1)
    :& Operator Tup15 :@ a1 :@ b1 :@ c1 :@ d1 :@ e1 :@ f1 :@ g1 :@ h1 :@ i1 :@ j1 :@ k1 :@ l1 :@ m1 :@ n1 :@ o1
  where a1 = spA vm a
        b1 = spA vm b
        c1 = spA vm c
        d1 = spA vm d
        e1 = spA vm e
        f1 = spA vm f
        g1 = spA vm g
        h1 = spA vm h
        i1 = spA vm i
        j1 = spA vm j
        k1 = spA vm k
        l1 = spA vm l
        m1 = spA vm m
        n1 = spA vm n
        o1 = spA vm o
        s e = exprSize e

spApp vm (Operator            Sel1 :@ a)           = spApp1 vm            Sel1 sel1 a
spApp vm (Operator            Sel2 :@ a)           = spApp1 vm            Sel2 sel2 a
spApp vm (Operator            Sel3 :@ a)           = spApp1 vm            Sel3 sel3 a
spApp vm (Operator            Sel4 :@ a)           = spApp1 vm            Sel4 sel4 a
spApp vm (Operator            Sel5 :@ a)           = spApp1 vm            Sel5 sel5 a
spApp vm (Operator            Sel6 :@ a)           = spApp1 vm            Sel6 sel6 a
spApp vm (Operator            Sel7 :@ a)           = spApp1 vm            Sel7 sel7 a
spApp vm (Operator            Sel8 :@ a)           = spApp1 vm            Sel8 sel8 a
spApp vm (Operator            Sel9 :@ a)           = spApp1 vm            Sel9 sel9 a
spApp vm (Operator           Sel10 :@ a)           = spApp1 vm           Sel10 sel10 a
spApp vm (Operator           Sel11 :@ a)           = spApp1 vm           Sel11 sel11 a
spApp vm (Operator           Sel12 :@ a)           = spApp1 vm           Sel12 sel12 a
spApp vm (Operator           Sel13 :@ a)           = spApp1 vm           Sel13 sel13 a
spApp vm (Operator           Sel14 :@ a)           = spApp1 vm           Sel14 sel14 a
spApp vm (Operator           Sel15 :@ a)           = spApp1 vm           Sel15 sel15 a

-- | ConditionM
spApp vm (Operator      ConditionM :@ a :@ b :@ c) = spApp3 vm      ConditionM (const (\/)) a b c

-- | LoopM
spApp vm (Operator           While :@ a :@ b)      = spApp2 vm           While topF2 a b
spApp vm (Operator             For :@ a :@ b)      = spApp2 vm             For topF2 a b

-- | Mutable
spApp vm (Operator          Return :@ a)           = spApp1 vm          Return id a
spApp vm (Operator            Bind :@ a :@ f)      = i :& Operator Bind :@ a1 :@ f1
  where (i,a1,f1) = spBind vm a f
spApp vm (Operator            Then :@ a :@ b)      = spApp2 vm            Then (flip const) a b
spApp vm (Operator            When :@ a :@ b)      = spApp2 vm            When (\ _ _ -> top) a b

-- | Help functions

topF1 :: Lattice u => a -> u
topF1 _ = top

topF2 :: Lattice u => a -> b -> u
topF2 _ _ = top

topF3 :: Lattice u => a -> b -> c -> u
topF3 _ _ _ = top

finalInfo :: (Show (Size a), Lattice (Size a)) => AExpr a -> Size a -> Info a
finalInfo e s = Info $ exprSize e \/ s

-- | Indexed loops without state (Parallel, EparFor, ...)
spLoI :: (Show (Size u), Lattice (Size u))
      => BindEnv -> Op (Length -> (Index -> b) -> u)
      -> (Size Index -> Size b -> Size u)
      -> AExpr Length -> AExpr (Index -> b)
      -> AExpr u
spLoI vm op f a (_ :& Lambda v e) = Info (f i $ exprSize e1) :& Operator op :@ a1 :@ (funInfo a1 e1 :& Lambda v e1)
  where a1 = spA vm a
        i  = exprSize a1
        e1 = spA (extend vm v $ Info i) e

-- | Helper for binds
spBind :: (Typeable a, Show (Size a), Lattice (Size a),
           Show (Size b), Lattice (Size b),
           Show (Size c), Lattice (Size c), Size a ~ Size b)
        => BindEnv -> AExpr a -> AExpr (b -> c) -> (Info c, AExpr a, AExpr (b -> c))
spBind vm a f = (Info bs, a1, f1)
  where a1  = spA vm a
        (bs,f1) = spLambda vm (exprSize a1) f

-- | Helper for lambdas
spLambda :: BindEnv -> Size a -> AExpr (a -> b) -> (Size b, AExpr (a -> b))
spLambda vm s (_ :& Lambda v e) = (exprSize e1, Info (s, exprSize e1) :& Lambda v e1)
  where e1 = spA (extend vm v $ Info s) e
spLambda _  _ _ = error "SizeProp.spLambda: not a lambda abstraction."

-- | Helper for two levels of lambdas
spLambda2 :: BindEnv -> Size a -> Size b -> AExpr (a -> b -> c) -> (Size c, AExpr (a -> b -> c))
spLambda2 vm s t (_ :& Lambda v (_ :& Lambda w e)) = (exprSize e1, f1)
  where e1 = spA (extend (extend vm v $ Info s) w $ Info t) e
        u1 = (t, exprSize e1)
        f1 = Info (s, u1) :& Lambda v (Info u1 :& Lambda w e1)
spLambda2 _  _ _ _ = error "SizeProp.spLambda2: not a lambda abstraction."

-- | Nullary applications
spApp0 :: (Show (Size u), Lattice (Size u)) => BindEnv -> Op u -> Size u -> AExpr u
spApp0 vm op s = Info s :& Operator op

-- | Unary applications
spApp1 :: (TypeF a, Show (Size u), Lattice (Size u))
       => BindEnv -> Op (a -> u) -> (Size a -> Size u) -> AExpr a -> AExpr u
spApp1 vm op f a = Info (f ai) :& Operator op :@ a1
  where a1 = spA vm a
        ai = infoSize $ aeInfo a1

-- | Binary applications
spApp2 :: (TypeF a, TypeF b, Show (Size u), Lattice (Size u))
       => BindEnv -> Op (a -> b -> u) -> (Size a -> Size b -> Size u)
       -> AExpr a -> AExpr b -> AExpr u
spApp2 vm op f a b = Info (f ai bi) :& Operator op :@ a1 :@ b1
  where a1 = spA vm a
        ai = infoSize $ aeInfo a1
        b1 = spA vm b
        bi = infoSize $ aeInfo b1

-- | Ternary applications
spApp3 :: (TypeF a, TypeF b, TypeF c, Show (Size u), Lattice (Size u))
       => BindEnv -> Op (a -> b -> c -> u)
       -> (Size a -> Size b -> Size c -> Size u)
       -> AExpr a -> AExpr b -> AExpr c
       -> AExpr u
spApp3 vm op f a b c = Info (f ai bi ci) :& Operator op :@ a1 :@ b1 :@ c1
  where a1 = spA vm a
        ai = infoSize $ aeInfo a1
        b1 = spA vm b
        bi = infoSize $ aeInfo b1
        c1 = spA vm c
        ci = infoSize $ aeInfo c1

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

