{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
  , untypeType
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
import Feldspar.Core.Reify (ASTF(..), unASTF)
import Feldspar.Core.Types (TypeRep(..), typeRep, defaultSize, TypeF(..))
import qualified Feldspar.Core.Types as T
import Feldspar.Core.UntypedRepresentation as U
import Feldspar.ValueInfo
import qualified Feldspar.Core.Representation as R
import Feldspar.Core.Representation (AExpr((:&)), Expr((:@)))
import Control.Monad.State
import Data.Typeable (Typeable)
import Feldspar.Core.SizeProp
import Feldspar.Core.AdjustBindings

-- | External module interface. Untype, optimize and unannotate.
untype :: TypeF a => FeldOpts -> ASTF a -> UntypedFeld
untype opts = cleanUp opts
            . pushLets
            . optimize
            . sinkLets opts
            . justUntype opts

-- | External module interface.
untypeDecor :: TypeF a => FeldOpts -> ASTF a -> AUntypedFeld ValueInfo
untypeDecor opts = pushLets
                 . optimize
                 . sinkLets opts
                 . justUntype opts

-- | External module interface.
untypeUnOpt :: TypeF a => FeldOpts -> ASTF a -> UntypedFeld
untypeUnOpt opts = cleanUp opts
                 . justUntype opts

-- | Only do the conversion to AUntypedFeld ValueInfo
justUntype :: TypeF a => FeldOpts -> ASTF a -> AUntypedFeld ValueInfo
justUntype opts = renameExp . toU . sizeProp . adjustBindings . unASTF opts

-- | Prepare the code for fromCore
cleanUp :: FeldOpts -> AUntypedFeld ValueInfo -> UntypedFeld
cleanUp opts = createTasks opts . unAnnotate . uniqueVars

renameExp :: AUntypedFeld a -> AUntypedFeld a
renameExp e = evalState (rename e) 0

toAnno :: TypeF a => R.Info a -> ValueInfo
toAnno info = toValueInfo (asInfo info) (R.infoSize info)

asInfo :: TypeF a => R.Info a -> TypeRep a
asInfo _ = typeRepF

asVar :: TypeF a => R.Var a -> TypeRep a
asVar _ = typeRepF

asExpr :: TypeF a => R.Expr a -> TypeRep a
asExpr _ = typeRepF

asOpT :: TypeF a => R.Op a -> TypeRep a
asOpT _ = typeRepF

toType :: TypeRep a -> Type
toType tr = untypeType tr (defaultSize tr)

toU :: R.AExpr a -> AUntypedFeld ValueInfo
toU (i :& e) = AIn (toAnno i) (toUr e)

toUr :: TypeF a => R.Expr a -> UntypedFeldF (AUntypedFeld ValueInfo)
toUr (R.Variable v) = Variable $ trV v
toUr (R.Literal v) = Literal $ literal tr (defaultSize tr) v
  where tr = typeRep
toUr e@(R.Operator op) = App (trOp op) (toType $ asExpr e) []
toUr (f :@ a) = toApp f [toU a]
toUr (R.Lambda v e) = Lambda (trV v) (toU e)

trV :: TypeF a => R.Var a -> Var
trV v =  Var {varNum = R.varNum v, varType = toType $ asVar v, varName = R.varName v}

toApp :: TypeF a => R.Expr a -> [AUntypedFeld ValueInfo] -> UntypedFeldF (AUntypedFeld ValueInfo)
toApp (R.Operator R.Cons) [e, AIn _ (App Tup (TupType ts) es)]
      = App Tup (TupType $ typeof e : ts) $ e : es
toApp (R.Operator R.Cdr) [AIn _ (App (DropN n) (TupType (_:ts)) es)]
      = App (DropN $ n+1) (TupType ts) es
toApp (R.Operator R.Car) [AIn _ (App (DropN n) (TupType (t:_)) es)] = App (SelN $ n+1) t es
toApp (R.Operator R.Tup) [AIn _ e] = e
toApp (R.Operator R.UnTup) [e] = App (DropN 0) (typeof e) [e]
toApp (R.Operator op) es = App (trOp op) (unwind es $ toType $ asOpT op) es
toApp (f :@ e) es = toApp f $ toU e : es

unwind :: [AUntypedFeld a] -> Type -> Type
unwind (_:es) (U.FunType _ t) = unwind es t
unwind []     t               = t
unwind es     t               = error $ "FromTyped.unwind: fun tye mismatch between "
                                         ++ show t ++ " and " ++ show es

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
trOp R.Sel1            = Sel1
trOp R.Sel2            = Sel2
trOp R.Sel3            = Sel3
trOp R.Sel4            = Sel4
trOp R.Sel5            = Sel5
trOp R.Sel6            = Sel6
trOp R.Sel7            = Sel7
trOp R.Sel8            = Sel8
trOp R.Sel9            = Sel9
trOp R.Sel10           = Sel10
trOp R.Sel11           = Sel11
trOp R.Sel12           = Sel12
trOp R.Sel13           = Sel13
trOp R.Sel14           = Sel14
trOp R.Sel15           = Sel15
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

instance TypeF a => Pretty (AExpr a) where
  pretty = show

instance TypeF a => Pretty (ASTF a) where
  pretty = pretty . unASTF ()

-- | Untype version to use with the new CSE
untypeProgOpt :: FeldOpts -> AExpr a -> AUntypedFeld ValueInfo
untypeProgOpt opts = toU

-- | Front-end driver
frontend :: TypeF a => PassCtrl FrontendPass -> FeldOpts -> ASTF a -> ([String], Maybe UntypedFeld)
frontend ctrl opts = evalPasses 0
                   $ pc FPCreateTasks      (createTasks opts)
                   . pt FPUnAnnotate       unAnnotate
                   . pc FPUnique           uniqueVars
                   . pc FPExpand           expand
                   . pc FPPushLets         pushLets
                   . pc FPOptimize         optimize
                   . pc FPSinkLets         (sinkLets opts)
                   . pc FPRename           renameExp
                   . pt FPUntype           (untypeProgOpt opts)
                   . pc FPSizeProp         sizeProp
                   . pc FPAdjustBind       adjustBindings
                   . pt FPUnASTF           (unASTF opts)
  where pc :: Pretty a => FrontendPass -> (a -> a) -> Prog a Int -> Prog a Int
        pc = passC ctrl
        pt :: (Pretty a, Pretty b) => FrontendPass -> (a -> b) -> Prog a Int -> Prog b Int
        pt = passT ctrl
