{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

module Feldspar.Core.Middleend.FromTyped
  ( untype
  , untypeDecor
  , untypeUnOpt
  , FrontendPass(..)
  , frontend
  )
  where

import qualified Data.ByteString.Char8 as B
import Feldspar.Core.Middleend.FromTypeUtil
import Feldspar.Core.Interpretation (FeldOpts)
import Feldspar.Core.Middleend.CreateTasks
import Feldspar.Core.Middleend.LetSinking
import Feldspar.Core.Middleend.OptimizeUntyped
import Feldspar.Core.Middleend.PushLets
import Feldspar.Core.Middleend.Expand
import Feldspar.Core.Middleend.UniqueVars
import Feldspar.Core.Middleend.PassManager
import qualified Feldspar.Core.UntypedRepresentation as U
import Feldspar.Core.Reify (ASTF, unASTF, render)
import Feldspar.Core.Types (TypeRep(..), typeRep, defaultSize, TypeF(..),
                            (:>)(..))
import qualified Feldspar.Core.Types as T
import Feldspar.Core.UntypedRepresentation hiding (Type(..), ScalarType(..))
import Feldspar.ValueInfo (ValueInfo(..))
import Feldspar.Range (Range(..))
import qualified Feldspar.Core.Representation as R
import Feldspar.Core.Representation (AExpr((:&)), Expr((:@)))
import Feldspar.Core.SizeProp
import Feldspar.Core.AdjustBindings
import Control.Monad.State (evalState)
import Data.Complex (Complex(..))
import Data.Typeable (Typeable)

-- | External module interface. Untype, optimize and unannotate.
untype :: FeldOpts -> ASTF a -> UntypedFeld
untype opts = cleanUp opts
            . untypeDecor opts

-- | External module interface. Untype and optimize.
untypeDecor :: FeldOpts -> ASTF a -> AUntypedFeld ValueInfo
untypeDecor opts = pushLets
                 . optimize
                 . sinkLets opts
                 . justUntype opts

-- | External module interface.
untypeUnOpt :: FeldOpts -> ASTF a -> UntypedFeld
untypeUnOpt opts = cleanUp opts
                 . justUntype opts

-- | Only do the conversion to AUntypedFeld ValueInfo
justUntype :: FeldOpts -> ASTF a -> AUntypedFeld ValueInfo
justUntype opts = renameExp . toU . sizeProp . adjustBindings . unASTF opts

-- | Prepare the code for fromCore
cleanUp :: FeldOpts -> AUntypedFeld ValueInfo -> UntypedFeld
cleanUp opts = createTasks opts . unAnnotate . uniqueVars

renameExp :: AUntypedFeld a -> AUntypedFeld a
renameExp e = evalState (rename e) 0

toU :: R.AExpr a -> AUntypedFeld ValueInfo
toU (((R.Info i) :: R.Info a) :& e)
  | (R.Variable (R.Var n s)) <- e
  = i2 $ Variable $ Var n (untypeType tr i) s
  | (R.Literal v) <- e
  = i2 $ Literal $ literal tr i v
  | (R.Operator op) <- e
  = i2 $ App (trOp op) (untypeType tr i) []
  | (R.Lambda (R.Var n s) e') <- e
  , FunType b _ <- tr
  = i2 $ Lambda (Var n (untypeType b (fst i)) s) $ toU e'
  | (R.Operator R.Cons :@ a1 :@ a2) <- e
  , e' <- toU a1
  , AIn _ (App Tup (U.TupType ts) es) <- toU a2
  = i2 $ App Tup (U.TupType $ typeof e' : ts) $ e' : es
  | (f :@ a) <- e
  = i2 $ case f of -- Avoid more pattern guards for GHC 8.4 performance reasons.
           R.Operator R.Car ->
            case toU a of
              AIn _ (App (Drop n) (U.TupType (t:_)) es) ->
                App (Sel $ n + 1) t es
           R.Operator R.Cdr ->
            case toU a of
              AIn _ (App (Drop n) (U.TupType (_:ts)) es) ->
                App (Drop $ n + 1) (U.TupType ts) es
           R.Operator R.Tup ->
             case toU a of
              AIn _ e' -> e'
           _ -> case go e [] of
                 (op, es) ->
                   App op (untypeType tr i) es
  where tr = typeRepF :: TypeRep a
        i2 = AIn $ toValueInfo tr i
        go :: forall a . R.Expr a -> [AUntypedFeld ValueInfo] -> (Op, [AUntypedFeld ValueInfo])
        go (R.Operator op) es = (trOp op, es)
        go (f :@ e) es = go f $ toU e : es

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
trOp (R.Sel n)         = Sel  (n  - 1)
trOp R.Sel1            = Sel  0
trOp R.Sel2            = Sel  1
trOp R.Sel3            = Sel  2
trOp R.Sel4            = Sel  3
trOp R.Sel5            = Sel  4
trOp R.Sel6            = Sel  5
trOp R.Sel7            = Sel  6
trOp R.Sel8            = Sel  7
trOp R.Sel9            = Sel  8
trOp R.Sel10           = Sel  9
trOp R.Sel11           = Sel 10
trOp R.Sel12           = Sel 11
trOp R.Sel13           = Sel 12
trOp R.Sel14           = Sel 13
trOp R.Sel15           = Sel 14
-- trOp R.Call            = Call
trOp R.Tup0            = Tup
trOp R.Tup2            = Tup
trOp R.Tup3            = Tup
trOp R.Tup4            = Tup
trOp R.Tup5            = Tup
trOp R.Tup6            = Tup
trOp R.Tup7            = Tup
trOp R.Tup8            = Tup
trOp R.Tup9            = Tup
trOp R.Tup10           = Tup
trOp R.Tup11           = Tup
trOp R.Tup12           = Tup
trOp R.Tup13           = Tup
trOp R.Tup14           = Tup
trOp R.Tup15           = Tup
trOp op                = error $ "FromTyped.trOp: unknown op: " ++ show op

untypeType :: TypeRep a -> T.Size a -> U.Type
untypeType UnitType _               = U.TupType []
untypeType BoolType _               = 1 U.:# U.BoolType
untypeType (IntType s n) _          = 1 U.:# U.IntType (convSign s) (convSize n)
untypeType FloatType _              = 1 U.:# U.FloatType
untypeType DoubleType _             = 1 U.:# U.DoubleType
untypeType (ComplexType t) _        = 1 U.:# U.ComplexType (untypeType t (defaultSize t))
untypeType (Tup2Type a b) (sa,sb)
  = U.TupType [untypeType a sa, untypeType b sb]
untypeType (Tup3Type a b c) (sa,sb,sc)
  = U.TupType [untypeType a sa, untypeType b sb, untypeType c sc]
untypeType (Tup4Type a b c d) (sa,sb,sc,sd)
  = U.TupType [ untypeType a sa, untypeType b sb, untypeType c sc
              , untypeType d sd
              ]
untypeType (Tup5Type a b c d e) (sa,sb,sc,sd,se)
  = U.TupType [ untypeType a sa, untypeType b sb, untypeType c sc
              , untypeType d sd, untypeType e se
              ]
untypeType (Tup6Type a b c d e f) (sa,sb,sc,sd,se,sf)
  = U.TupType [ untypeType a sa, untypeType b sb, untypeType c sc
              , untypeType d sd, untypeType e se, untypeType f sf
              ]
untypeType (Tup7Type a b c d e f g) (sa,sb,sc,sd,se,sf,sg)
  = U.TupType [ untypeType a sa, untypeType b sb, untypeType c sc
              , untypeType d sd, untypeType e se, untypeType f sf
              , untypeType g sg
              ]
untypeType (Tup8Type a b c d e f g h) (sa,sb,sc,sd,se,sf,sg,sh)
  = U.TupType [ untypeType a sa, untypeType b sb, untypeType c sc
              , untypeType d sd, untypeType e se, untypeType f sf
              , untypeType g sg, untypeType h sh
              ]
untypeType (Tup9Type a b c d e f g h i) (sa,sb,sc,sd,se,sf,sg,sh,si)
  = U.TupType [ untypeType a sa, untypeType b sb, untypeType c sc
              , untypeType d sd, untypeType e se, untypeType f sf
              , untypeType g sg, untypeType h sh, untypeType i si
              ]
untypeType (Tup10Type a b c d e f g h i j) (sa,sb,sc,sd,se,sf,sg,sh,si,sj)
  = U.TupType [ untypeType a sa, untypeType b sb, untypeType c sc
              , untypeType d sd, untypeType e se, untypeType f sf
              , untypeType g sg, untypeType h sh, untypeType i si
              , untypeType j sj
              ]
untypeType (Tup11Type a b c d e f g h i j k) (sa,sb,sc,sd,se,sf,sg,sh,si,sj,sk)
  = U.TupType [ untypeType a sa, untypeType b sb, untypeType c sc
              , untypeType d sd, untypeType e se, untypeType f sf
              , untypeType g sg, untypeType h sh, untypeType i si
              , untypeType j sj, untypeType k sk
              ]
untypeType (Tup12Type a b c d e f g h i j k l) (sa,sb,sc,sd,se,sf,sg,sh,si,sj,sk,sl)
  = U.TupType [ untypeType a sa, untypeType b sb, untypeType c sc
              , untypeType d sd, untypeType e se, untypeType f sf
              , untypeType g sg, untypeType h sh, untypeType i si
              , untypeType j sj, untypeType k sk, untypeType l sl
              ]
untypeType (Tup13Type a b c d e f g h i j k l m) (sa,sb,sc,sd,se,sf,sg,sh,si,sj,sk,sl,sm)
  = U.TupType [ untypeType a sa, untypeType b sb, untypeType c sc
              , untypeType d sd, untypeType e se, untypeType f sf
              , untypeType g sg, untypeType h sh, untypeType i si
              , untypeType j sj, untypeType k sk, untypeType l sl
              , untypeType m sm
              ]
untypeType (Tup14Type a b c d e f g h i j k l m n) (sa,sb,sc,sd,se,sf,sg,sh,si,sj,sk,sl,sm,sn)
  = U.TupType [ untypeType a sa, untypeType b sb, untypeType c sc
              , untypeType d sd, untypeType e se, untypeType f sf
              , untypeType g sg, untypeType h sh, untypeType i si
              , untypeType j sj, untypeType k sk, untypeType l sl
              , untypeType m sm, untypeType n sn
              ]
untypeType (Tup15Type a b c d e f g h i j k l m n o) (sa,sb,sc,sd,se,sf,sg,sh,si,sj,sk,sl,sm,sn,so)
  = U.TupType [ untypeType a sa, untypeType b sb, untypeType c sc
              , untypeType d sd, untypeType e se, untypeType f sf
              , untypeType g sg, untypeType h sh, untypeType i si
              , untypeType j sj, untypeType k sk, untypeType l sl
              , untypeType m sm, untypeType n sn, untypeType o so
              ]
untypeType (MutType a) sz              = U.MutType (untypeType a sz)
untypeType (RefType a) sz              = U.RefType (untypeType a sz)
untypeType (ArrayType a) (rs :> es)    = U.ArrayType rs (untypeType a es)
untypeType (MArrType a) (rs :> es)     = U.MArrType rs (untypeType a es)
untypeType (ParType a) sz              = U.ParType (untypeType a sz)
untypeType (ElementsType a) (rs :> es) = U.ElementsType (untypeType a es)
untypeType (ConsType a b) (sa,sb)      = U.TupType $ untypeTup (ConsType a b) (sa,sb)
untypeType NilType        _            = U.TupType []
untypeType (TupleType t) sz            = untypeType t sz
untypeType (IVarType a) sz             = U.IVarType $ untypeType a sz
untypeType (FunType a b) (sa, sz)      = U.FunType (untypeType a sa) (untypeType b sz)
untypeType (FValType a) sz             = U.FValType (untypeType a sz)
untypeType _ _                       = error "untypeType: missing "

untypeTup :: TypeRep (T.RTuple a) -> T.Size (T.RTuple a) -> [U.Type]
untypeTup (ConsType a b) (sa,sb) = untypeType a sa : untypeTup b sb
untypeTup NilType        _       = []

convSign :: T.Signedness a -> U.Signedness
convSign T.U       = Unsigned
convSign T.S       = Signed

literal :: TypeRep a -> T.Size a -> a -> Lit
literal t@UnitType        sz a = literalConst t sz a
literal t@BoolType        sz a = literalConst t sz a
literal t@IntType{}       sz a = literalConst t sz a
literal t@FloatType       sz a = literalConst t sz a
literal t@DoubleType      sz a = literalConst t sz a
literal t@ComplexType{}   sz a = literalConst t sz a
literal t@ArrayType{}     sz a = literalConst t sz a
literal (Tup2Type ta tb) (sa,sb) (a,b)
    = LTup [literal ta sa a, literal tb sb b]
literal (Tup3Type ta tb tc) (sa,sb,sc) (a,b,c)
    = LTup [literal ta sa a, literal tb sb b, literal tc sc c]
literal (Tup4Type ta tb tc td) (sa,sb,sc,sd) (a,b,c,d)
    = LTup [ literal ta sa a, literal tb sb b, literal tc sc c
           , literal td sd d
           ]
literal (Tup5Type ta tb tc td te) (sa,sb,sc,sd,se) (a,b,c,d,e)
    = LTup [ literal ta sa a, literal tb sb b, literal tc sc c
           , literal td sd d, literal te se e
           ]
literal (Tup6Type ta tb tc td te tf) (sa,sb,sc,sd,se,sf) (a,b,c,d,e,f)
    = LTup [ literal ta sa a, literal tb sb b, literal tc sc c
           , literal td sd d, literal te se e, literal tf sf f
           ]
literal (Tup7Type ta tb tc td te tf tg) (sa,sb,sc,sd,se,sf,sg) (a,b,c,d,e,f,g)
    = LTup [ literal ta sa a, literal tb sb b, literal tc sc c
           , literal td sd d, literal te se e, literal tf sf f
           , literal tg sg g
           ]
literal _ _ _ = error "Missing pattern: FromTyped.hs: literal"

literalConst :: TypeRep a -> T.Size a -> a -> Lit
literalConst UnitType        _  ()     = LTup []
literalConst BoolType        _  a      = LBool a
literalConst (IntType s n)   _  a      = LInt (convSign s) (convSize n) (toInteger a)
literalConst FloatType       _  a      = LFloat a
literalConst DoubleType      _  a      = LDouble a
literalConst (ArrayType t) (_ :> sz) a = LArray t' $ map (literalConst t sz) a
  where t' = untypeType t sz
literalConst (ComplexType t) _  (r:+i) = LComplex re ie
  where re = literalConst t (defaultSize t) r
        ie = literalConst t (defaultSize t) i

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
toValueInfo (IntType T.U T.NNative) r       = VIWordN   r
toValueInfo (IntType T.S T.NNative) r       = VIIntN    r
-- FIXME: No range for FP types and ComplexType yet.
toValueInfo FloatType         _             = VIFloat
toValueInfo DoubleType        _             = VIDouble
toValueInfo (ComplexType _)   _             = VIProd []
toValueInfo (Tup2Type a b) (sa,sb)
  = VIProd [toValueInfo a sa, toValueInfo b sb]
toValueInfo (Tup3Type a b c) (sa,sb,sc)
  = VIProd [toValueInfo a sa, toValueInfo b sb, toValueInfo c sc]
toValueInfo (Tup4Type a b c d) (sa,sb,sc,sd)
  = VIProd [ toValueInfo a sa, toValueInfo b sb, toValueInfo c sc
           , toValueInfo d sd
           ]
toValueInfo (Tup5Type a b c d e) (sa,sb,sc,sd,se)
  = VIProd [ toValueInfo a sa, toValueInfo b sb, toValueInfo c sc
           , toValueInfo d sd, toValueInfo e se
           ]
toValueInfo (Tup6Type a b c d e f) (sa,sb,sc,sd,se,sf)
  = VIProd [ toValueInfo a sa, toValueInfo b sb, toValueInfo c sc
           , toValueInfo d sd, toValueInfo e se, toValueInfo f sf
           ]
toValueInfo (Tup7Type a b c d e f g) (sa,sb,sc,sd,se,sf,sg)
  = VIProd [ toValueInfo a sa, toValueInfo b sb, toValueInfo c sc
           , toValueInfo d sd, toValueInfo e se, toValueInfo f sf
           , toValueInfo g sg
           ]
toValueInfo (Tup8Type a b c d e f g h) (sa,sb,sc,sd,se,sf,sg,sh)
  = VIProd [ toValueInfo a sa, toValueInfo b sb, toValueInfo c sc
           , toValueInfo d sd, toValueInfo e se, toValueInfo f sf
           , toValueInfo g sg, toValueInfo h sh
           ]
toValueInfo (Tup9Type a b c d e f g h i) (sa,sb,sc,sd,se,sf,sg,sh,si)
  = VIProd [ toValueInfo a sa, toValueInfo b sb, toValueInfo c sc
           , toValueInfo d sd, toValueInfo e se, toValueInfo f sf
           , toValueInfo g sg, toValueInfo h sh, toValueInfo i si
            ]
toValueInfo (Tup10Type a b c d e f g h i j) (sa,sb,sc,sd,se,sf,sg,sh,si,sj)
  = VIProd [ toValueInfo a sa, toValueInfo b sb, toValueInfo c sc
           , toValueInfo d sd, toValueInfo e se, toValueInfo f sf
           , toValueInfo g sg, toValueInfo h sh, toValueInfo i si
           , toValueInfo j sj
           ]
toValueInfo (Tup11Type a b c d e f g h i j k) (sa,sb,sc,sd,se,sf,sg,sh,si,sj,sk)
  = VIProd [ toValueInfo a sa, toValueInfo b sb, toValueInfo c sc
           , toValueInfo d sd, toValueInfo e se, toValueInfo f sf
           , toValueInfo g sg, toValueInfo h sh, toValueInfo i si
           , toValueInfo j sj, toValueInfo k sk
           ]
toValueInfo (Tup12Type a b c d e f g h i j k l) (sa,sb,sc,sd,se,sf,sg,sh,si,sj,sk,sl)
  = VIProd [ toValueInfo a sa, toValueInfo b sb, toValueInfo c sc
           , toValueInfo d sd, toValueInfo e se, toValueInfo f sf
           , toValueInfo g sg, toValueInfo h sh, toValueInfo i si
           , toValueInfo j sj, toValueInfo k sk, toValueInfo l sl
           ]
toValueInfo (Tup13Type a b c d e f g h i j k l m) (sa,sb,sc,sd,se,sf,sg,sh,si,sj,sk,sl,sm)
  = VIProd [ toValueInfo a sa, toValueInfo b sb, toValueInfo c sc
           , toValueInfo d sd, toValueInfo e se, toValueInfo f sf
           , toValueInfo g sg, toValueInfo h sh, toValueInfo i si
           , toValueInfo j sj, toValueInfo k sk, toValueInfo l sl
           , toValueInfo m sm
           ]
toValueInfo (Tup14Type a b c d e f g h i j k l m n) (sa,sb,sc,sd,se,sf,sg,sh,si,sj,sk,sl,sm,sn)
  = VIProd [ toValueInfo a sa, toValueInfo b sb, toValueInfo c sc
           , toValueInfo d sd, toValueInfo e se, toValueInfo f sf
           , toValueInfo g sg, toValueInfo h sh, toValueInfo i si
           , toValueInfo j sj, toValueInfo k sk, toValueInfo l sl
           , toValueInfo m sm, toValueInfo n sn
           ]
toValueInfo (Tup15Type a b c d e f g h i j k l m n o) (sa,sb,sc,sd,se,sf,sg,sh,si,sj,sk,sl,sm,sn,so)
  = VIProd [ toValueInfo a sa, toValueInfo b sb, toValueInfo c sc
           , toValueInfo d sd, toValueInfo e se, toValueInfo f sf
           , toValueInfo g sg, toValueInfo h sh, toValueInfo i si
           , toValueInfo j sj, toValueInfo k sk, toValueInfo l sl
           , toValueInfo m sm, toValueInfo n sn, toValueInfo o so
           ]
toValueInfo (MutType a) sz                  = toValueInfo a sz
toValueInfo (RefType a) sz                  = toValueInfo a sz
toValueInfo (ArrayType a) (Range (T.WordN l) (T.WordN r) :> es)
  = VIProd [VIWord32 (Range l r), toValueInfo a es]
toValueInfo (MArrType a) (Range (T.WordN l) (T.WordN r) :> es)
  = VIProd [VIWord32 (Range l r), toValueInfo a es]
toValueInfo (ParType a) sz                  = toValueInfo a sz
toValueInfo (ElementsType a) (Range (T.WordN l) (T.WordN r) :> es)
  = VIProd [VIWord32 (Range l r), toValueInfo a es]
toValueInfo (ConsType a b) (sa,sb) = VIProd $ toValueInfo a sa : ss
  where VIProd ss = toValueInfo b sb
toValueInfo NilType _ = VIProd []
toValueInfo (TupleType t) sz = toValueInfo t sz
toValueInfo (IVarType a) sz                 = toValueInfo a sz
-- TODO: Maybe keep argument information for FunType.
toValueInfo (FunType a b) (sa, _)           = toValueInfo a sa
toValueInfo (FValType a) sz                 = toValueInfo a sz
toValueInfo typ _
  = error $ "toValueInfo: missing case for " ++ show typ

-- The front-end driver.

-- | Enumeration of front end passes
data FrontendPass
     = FPUnASTF
     | FPAdjustBind
     | FPSizeProp
     | FPUntype
     | FPRename
     | FPSinkLets
     | FPOptimize
     | FPPushLets
     | FPExpand
     | FPUnique
     | FPUnAnnotate
     | FPCreateTasks
     deriving (Eq, Show, Enum, Bounded, Read)

-- | Overloaded pretty printing of annotations (for instance range information)

class PrettyInfo a where
  prettyInfo :: U.Type -> a -> String

instance PrettyInfo ValueInfo where
  prettyInfo = prettyVI

instance PrettyInfo () where
  prettyInfo _ _ = ""

instance Pretty UntypedFeld where
  pretty = pretty . annotate (const ())

instance PrettyInfo a => Pretty (AUntypedFeld a) where
  pretty = prettyExp f
     where f t x = " | " ++ prettyInfo t x

instance Pretty (AExpr a) where
  pretty = show

instance Pretty (ASTF a) where
  pretty = render

-- | Front-end driver
frontend :: PassCtrl FrontendPass -> FeldOpts -> ASTF a -> ([String], Maybe UntypedFeld)
frontend ctrl opts = evalPasses 0
                   $ pc FPCreateTasks      (createTasks opts)
                   . pt FPUnAnnotate       unAnnotate
                   . pc FPUnique           uniqueVars
                   . pc FPExpand           expand
                   . pc FPPushLets         pushLets
                   . pc FPOptimize         optimize
                   . pc FPSinkLets         (sinkLets opts)
                   . pc FPRename           renameExp
                   . pt FPUntype           toU
                   . pc FPSizeProp         sizeProp
                   . pc FPAdjustBind       adjustBindings
                   . pt FPUnASTF           (unASTF opts)
  where pc :: Pretty a => FrontendPass -> (a -> a) -> Prog a Int -> Prog a Int
        pc = passC ctrl
        pt :: (Pretty a, Pretty b) => FrontendPass -> (a -> b) -> Prog a Int -> Prog b Int
        pt = passT ctrl
