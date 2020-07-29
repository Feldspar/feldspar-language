{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
-- This file takes 30+ seconds to compile with pattern match warnings
-- in GHC 8.4. https://gitlab.haskell.org/ghc/ghc/-/issues/14987
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

-- | Translate from the typed to an untyped functional representation
module Feldspar.Core.Middleend.FromTyped
  ( toU
  ) where

import qualified Feldspar.Core.UntypedRepresentation as U
import Feldspar.Core.Types (TypeRep(..), TypeF(..), (:>)(..))
import qualified Feldspar.Core.Types as T
import Feldspar.Core.UntypedRepresentation hiding (Type(..), ScalarType(..))
import Feldspar.Core.ValueInfo (ValueInfo(..))
import Feldspar.Range (Range(..))
import qualified Feldspar.Core.Representation as R
import Feldspar.Core.Representation (AExpr(..), Expr(..))
import Data.Complex (Complex(..))
import Feldspar.Lattice (universal)

toU :: AExpr a -> UntypedFeld ValueInfo
toU (((R.Info i) :: R.Info a) :& e)
  | Sym (R.Variable (R.Var n s)) <- e
  = i2 $ Variable $ Var n (untypeType tr i) s
  | Sym (R.Literal v) <- e
  = i2 $ Literal $ literal tr i v
  | Sym op <- e
  = i2 $ App (trOp op) (untypeType tr i) []
  | Sym (R.Lambda (R.Var n s)) :@ e' <- e
  , FunType b _ <- tr
  = i2 $ Lambda (Var n (untypeType b (fst i)) s) $ toU e'
  | Sym R.Cons :@ a1 :@ a2 <- e
  , e' <- toU a1
  , In _ (App Tup (U.TupType ts) es) <- toU a2
  = i2 $ App Tup (U.TupType $ typeof e' : ts) $ e' : es
  | Sym R.Car :@ a <- e
  , In _ (App (Drop n) (U.TupType (t:_)) es) <- addDrop $ toU a
  = i2 $ App (Sel n) t es
  | Sym R.Cdr :@ a <- e
  , In _ (App (Drop n) (U.TupType (_:ts)) es) <- addDrop $ toU a
  = i2 $ App (Drop $ n + 1) (U.TupType ts) es
  | Sym R.Tup :@ a <- e
  , In _ e' <- toU a
  = i2 e'
  | (op, es) <- go e []
  = i2 $ App op (untypeType tr i) es
  where tr = typeRepF :: TypeRep a
        i2 = In $ toValueInfo tr i
        go :: forall a' . Expr a' -> [UntypedFeld ValueInfo] -> (Op, [UntypedFeld ValueInfo])
        go (Sym op) es = (trOp op, es)
        go (f :@ e') es = go f $ toU e' : es
        addDrop e'@(In _ (App (Drop _) _ _)) = e'
        addDrop e' = In (error "FromTyped: temporary drop")
                         (App (Drop 0) (typeof e') [e'])

-- | Translate a Typed operator to the corresponding untyped one
trOp :: R.Op a -> Op
trOp R.GetLength       = GetLength
trOp R.Parallel        = Parallel
trOp R.Append          = Append
trOp R.GetIx           = GetIx
trOp R.SetLength       = SetLength
trOp R.Sequential      = Sequential
trOp R.SetIx           = SetIx
trOp R.Let             = Let
trOp R.Bit             = Bit
trOp R.Complement      = Complement
trOp R.ReverseBits     = ReverseBits
trOp R.BitScan         = BitScan
trOp R.BitCount        = BitCount
trOp R.BAnd            = BAnd
trOp R.BOr             = BOr
trOp R.BXor            = BXor
trOp R.SetBit          = SetBit
trOp R.ClearBit        = ClearBit
trOp R.ComplementBit   = ComplementBit
trOp R.TestBit         = TestBit
trOp R.ShiftLU         = ShiftLU
trOp R.ShiftRU         = ShiftRU
trOp R.ShiftL          = ShiftL
trOp R.ShiftR          = ShiftR
trOp R.RotateLU        = RotateLU
trOp R.RotateRU        = RotateRU
trOp R.RotateL         = RotateL
trOp R.RotateR         = RotateR
trOp R.RealPart        = RealPart
trOp R.ImagPart        = ImagPart
trOp R.Conjugate       = Conjugate
trOp R.Magnitude       = Magnitude
trOp R.Phase           = Phase
trOp R.Cis             = Cis
trOp R.MkComplex       = MkComplex
trOp R.MkPolar         = MkPolar
trOp R.Condition       = Condition
trOp R.ConditionM      = ConditionM
trOp R.F2I             = F2I
trOp R.I2N             = I2N
trOp R.B2I             = B2I
trOp R.Round           = Round
trOp R.Ceiling         = Ceiling
trOp R.Floor           = Floor
trOp R.ESkip           = ESkip
trOp R.EMaterialize    = EMaterialize
trOp R.EWrite          = EWrite
trOp R.EPar            = EPar
trOp R.EparFor         = EparFor
trOp R.Equal           = Equal
trOp R.NotEqual        = NotEqual
trOp R.Undefined       = Undefined
trOp (R.Assert s)      = Assert s
-- trOp R.ForeignImport   = ForeignImport
trOp R.Exp             = Exp
trOp R.Sqrt            = Sqrt
trOp R.Log             = Log
trOp R.Sin             = Sin
trOp R.Tan             = Tan
trOp R.Cos             = Cos
trOp R.Asin            = Asin
trOp R.Atan            = Atan
trOp R.Acos            = Acos
trOp R.Sinh            = Sinh
trOp R.Tanh            = Tanh
trOp R.Cosh            = Cosh
trOp R.Asinh           = Asinh
trOp R.Atanh           = Atanh
trOp R.Acosh           = Acosh
trOp R.Pow             = Pow
trOp R.LogBase         = LogBase
trOp R.Pi              = Pi
trOp R.DivFrac         = DivFrac
trOp R.MkFuture        = MkFuture
trOp R.Await           = Await
trOp R.Quot            = Quot
trOp R.Rem             = Rem
trOp R.Div             = Div
trOp R.Mod             = Mod
trOp R.IExp            = IExp
trOp R.Not             = Not
trOp R.And             = And
trOp R.Or              = Or
trOp R.ForLoop         = ForLoop
trOp R.WhileLoop       = WhileLoop
trOp R.While           = While
trOp R.For             = For
trOp R.Run             = Run
trOp R.Return          = Return
trOp R.Bind            = Bind
trOp R.Then            = Then
trOp R.When            = When
trOp R.NewArr_         = NewArr_
trOp R.ArrLength       = ArrLength
trOp R.NewArr          = NewArr
trOp R.GetArr          = GetArr
trOp R.SetArr          = SetArr
trOp R.RunMutableArray = RunMutableArray
trOp R.WithArray       = WithArray
trOp R.NewRef          = NewRef
trOp R.GetRef          = GetRef
trOp R.SetRef          = SetRef
trOp R.ModRef          = ModRef
trOp R.Nil             = Tup
trOp R.NoInline        = NoInline
trOp R.Abs             = Abs
trOp R.Sign            = Sign
trOp R.Add             = Add
trOp R.Sub             = Sub
trOp R.Mul             = Mul
trOp R.ParRun          = ParRun
trOp R.ParGet          = ParGet
trOp R.ParFork         = ParFork
trOp R.ParNew          = ParNew
trOp R.ParYield        = ParYield
trOp R.ParPut          = ParPut
trOp R.LTH             = LTH
trOp R.GTH             = GTH
trOp R.LTE             = LTE
trOp R.GTE             = GTE
trOp R.Min             = Min
trOp R.Max             = Max
trOp R.Atan2           = Atan2
trOp R.Save            = Save
trOp (R.PropSize _)    = PropSize
-- trOp R.SourceInfo      = SourceInfo
trOp R.Switch          = Switch
-- trOp R.Call            = Call
trOp op                = error $ "FromTyped.trOp: unknown op: " ++ show op

-- | Convert a BitWidth to an untyped Size
convSize :: T.BitWidth a -> U.Size
convSize T.N8      = U.S8
convSize T.N16     = U.S16
convSize T.N32     = U.S32
convSize T.N64     = U.S64

-- | Default size for numeric types
defaultNumSize :: TypeRep a -> T.Size a
defaultNumSize IntType{} = universal
defaultNumSize FloatType = universal
defaultNumSize DoubleType = universal
defaultNumSize t = error $ "defaultNumSize: missing numeric type: " ++ show t

untypeType :: TypeRep a -> T.Size a -> U.Type
untypeType UnitType _               = U.TupType []
untypeType BoolType _               = 1 U.:# U.BoolType
untypeType (IntType s n) _          = 1 U.:# U.IntType (convSign s) (convSize n)
untypeType FloatType _              = 1 U.:# U.FloatType
untypeType DoubleType _             = 1 U.:# U.DoubleType
untypeType (ComplexType t) _        = 1 U.:# U.ComplexType (untypeType t (defaultNumSize t))
untypeType (Tup2Type t) sz          = U.TupType $ untypeTup t sz
untypeType (Tup3Type t) sz          = U.TupType $ untypeTup t sz
untypeType (Tup4Type t) sz          = U.TupType $ untypeTup t sz
untypeType (Tup5Type t) sz          = U.TupType $ untypeTup t sz
untypeType (Tup6Type t) sz          = U.TupType $ untypeTup t sz
untypeType (Tup7Type t) sz          = U.TupType $ untypeTup t sz
untypeType (Tup8Type t) sz          = U.TupType $ untypeTup t sz
untypeType (Tup9Type t) sz          = U.TupType $ untypeTup t sz
untypeType (Tup10Type t) sz         = U.TupType $ untypeTup t sz
untypeType (Tup11Type t) sz         = U.TupType $ untypeTup t sz
untypeType (Tup12Type t) sz         = U.TupType $ untypeTup t sz
untypeType (Tup13Type t) sz         = U.TupType $ untypeTup t sz
untypeType (Tup14Type t) sz         = U.TupType $ untypeTup t sz
untypeType (Tup15Type t) sz         = U.TupType $ untypeTup t sz
untypeType (MutType a) sz              = U.MutType (untypeType a sz)
untypeType (RefType a) sz              = U.RefType (untypeType a sz)
untypeType (ArrayType a) (rs :> es)    = U.ArrayType rs (untypeType a es)
untypeType (MArrType a) (rs :> es)     = U.MArrType rs (untypeType a es)
untypeType (ParType a) sz              = U.ParType (untypeType a sz)
untypeType (ElementsType a) (_ :> es)  = U.ElementsType (untypeType a es)
untypeType c@ConsType{} sz             = U.TupType $ untypeTup c sz
untypeType NilType        _            = U.TupType []
untypeType (IVarType a) sz             = U.IVarType $ untypeType a sz
untypeType (FunType a b) (sa, sz)      = U.FunType (untypeType a sa) (untypeType b sz)
untypeType (FValType a) sz             = U.FValType (untypeType a sz)

untypeTup :: TypeRep (T.Tuple a) -> T.Size (T.Tuple a) -> [U.Type]
untypeTup (ConsType a b) (sa,sb) = untypeType a sa : untypeTup b sb
untypeTup NilType        _       = []

convSign :: T.Signedness a -> U.Signedness
convSign T.U       = Unsigned
convSign T.S       = Signed

literal :: TypeRep a -> T.Size a -> a -> Lit
literal UnitType        _ () = LTup []
literal BoolType        _  a = LBool a
literal (IntType s n)   _  a = LInt (convSign s) (convSize n) (toInteger a)
literal FloatType       _  a = LFloat a
literal DoubleType      _  a = LDouble a
literal (ArrayType t) (_ :> sz) a = LArray t' $ map (literal t sz) a
  where t' = untypeType t sz
literal (ComplexType t) _ (r :+ i) = LComplex re ie
  where re = literal t (defaultNumSize t) r
        ie = literal t (defaultNumSize t) i
literal _ _ _ = error "Missing pattern: FromTyped.hs: literal"

-- | Construct a ValueInfo from a TypeRep and a Size
toValueInfo :: TypeRep a -> T.Size a -> ValueInfo
toValueInfo UnitType          _             = VIProd []
-- FIXME: No range for boolean types yet.
toValueInfo BoolType          _             = VIBool    (Range 0 1)
toValueInfo (IntType T.U T.N8)      r       = VIWord8   r
toValueInfo (IntType T.S T.N8)      r       = VIInt8    r
toValueInfo (IntType T.U T.N16)     r       = VIWord16  r
toValueInfo (IntType T.S T.N16)     r       = VIInt16   r
toValueInfo (IntType T.U T.N32)     r       = VIWord32  r
toValueInfo (IntType T.S T.N32)     r       = VIInt32   r
toValueInfo (IntType T.U T.N64)     r       = VIWord64  r
toValueInfo (IntType T.S T.N64)     r       = VIInt64   r
-- FIXME: No range for FP types and ComplexType yet.
toValueInfo FloatType         _             = VIFloat
toValueInfo DoubleType        _             = VIDouble
toValueInfo (ComplexType _)   _             = VIProd []
toValueInfo (Tup2Type t) sz                 = toValueInfo t sz
toValueInfo (Tup3Type t) sz                 = toValueInfo t sz
toValueInfo (Tup4Type t) sz                 = toValueInfo t sz
toValueInfo (Tup5Type t) sz                 = toValueInfo t sz
toValueInfo (Tup6Type t) sz                 = toValueInfo t sz
toValueInfo (Tup7Type t) sz                 = toValueInfo t sz
toValueInfo (Tup8Type t) sz                 = toValueInfo t sz
toValueInfo (Tup9Type t) sz                 = toValueInfo t sz
toValueInfo (Tup10Type t) sz                = toValueInfo t sz
toValueInfo (Tup11Type t) sz                = toValueInfo t sz
toValueInfo (Tup12Type t) sz                = toValueInfo t sz
toValueInfo (Tup13Type t) sz                = toValueInfo t sz
toValueInfo (Tup14Type t) sz                = toValueInfo t sz
toValueInfo (Tup15Type t) sz                = toValueInfo t sz
toValueInfo (MutType a) sz                  = toValueInfo a sz
toValueInfo (RefType a) sz                  = toValueInfo a sz
toValueInfo (ArrayType a) (Range l r :> es)
  = VIProd [VIWord32 (Range l r), toValueInfo a es]
toValueInfo (MArrType a) (Range l r :> es)
  = VIProd [VIWord32 (Range l r), toValueInfo a es]
toValueInfo (ParType a) sz                  = toValueInfo a sz
toValueInfo (ElementsType a) (Range l r :> es)
  = VIProd [VIWord32 (Range l r), toValueInfo a es]
toValueInfo (ConsType a b) (sa,sb) = VIProd $ toValueInfo a sa : ss
  where VIProd ss = toValueInfo b sb
toValueInfo NilType _ = VIProd []
toValueInfo (IVarType a) sz                 = toValueInfo a sz
-- TODO: Maybe keep argument information for FunType.
toValueInfo (FunType a _) (sa, _)           = toValueInfo a sa
toValueInfo (FValType a) sz                 = toValueInfo a sz
