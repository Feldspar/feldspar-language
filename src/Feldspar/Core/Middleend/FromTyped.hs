{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Feldspar.Core.Middleend.FromTyped (
  untypeProg
  )
  where

import Data.Complex
import Data.Typeable (Typeable)

import Language.Syntactic
import Language.Syntactic.Constructs.Binding hiding (Variable, Let)
import qualified Language.Syntactic.Constructs.Binding as SynBind
import Language.Syntactic.Constructs.Binding.HigherOrder hiding (Let)

import Feldspar.Range (upperBound)

import Feldspar.Core.Types
import Feldspar.Core.Interpretation hiding (literal)
import Feldspar.Core.Constructs (FeldDom(..))
import Feldspar.Core.Constructs.Array
import Feldspar.Core.Constructs.Bits
import Feldspar.Core.Constructs.Literal
import Feldspar.Core.Constructs.Complex
import Feldspar.Core.Constructs.Condition
import Feldspar.Core.Constructs.ConditionM
import Feldspar.Core.Constructs.Conversion
import Feldspar.Core.Constructs.Elements
import Feldspar.Core.Constructs.Error
import Feldspar.Core.Constructs.Eq
import Feldspar.Core.Constructs.Floating
import Feldspar.Core.Constructs.FFI
import Feldspar.Core.Constructs.Fractional
import Feldspar.Core.Constructs.Future
import Feldspar.Core.Constructs.Integral
import Feldspar.Core.Constructs.Loop
import Feldspar.Core.Constructs.Logic
import Feldspar.Core.Constructs.Par
import Feldspar.Core.Constructs.Mutable
import Feldspar.Core.Constructs.MutableArray
import Feldspar.Core.Constructs.MutableReference
import Feldspar.Core.Constructs.MutableToPure
import Feldspar.Core.Constructs.NoInline
import Feldspar.Core.Constructs.Num
import Feldspar.Core.Constructs.Ord
import Feldspar.Core.Constructs.RealFloat
import Feldspar.Core.Constructs.Save
import Feldspar.Core.Constructs.SizeProp
import Feldspar.Core.Constructs.SourceInfo
import Feldspar.Core.Constructs.Switch
import Feldspar.Core.Constructs.Trace
import Feldspar.Core.Constructs.Tuple
import qualified Feldspar.Core.Constructs.Binding as Core
import Feldspar.Core.UntypedRepresentation hiding ( Lambda, UntypedFeldF(..)
                                                  , Size, Type(..), Signedness
                                                  , PrimOp1(..), PrimOp2(..)
                                                  )
import qualified Feldspar.Core.UntypedRepresentation as Ut

-- Translate from the typed AST to a nearly identical format that is not
-- typed in the host language.

-- | A minimal complete instance has to define either 'untypeProgSym' or
-- 'untypeExprSym'.
class Untype sub dom
  where
    untypeProgSym
        :: sub a
        -> Info (DenResult a)
        -> Args (AST (Decor Info dom)) a
        -> UntypedFeld
    untypeProgSym = untypeProgFresh

instance (Untype sub1 dom, Untype sub2 dom) =>
    Untype (sub1 :+: sub2) dom
  where
    untypeProgSym (InjL a) = untypeProgSym a
    untypeProgSym (InjR a) = untypeProgSym a

instance Untype FeldDom FeldDom
  where
    untypeProgSym (C' a) = untypeProgSym a

instance Untype Empty dom
  where
    untypeProgSym _ = error "Can't untype Empty"

untypeProgDecor :: Untype dom dom
    => Decor Info dom a
    -> Args (AST (Decor Info dom)) a
    -> UntypedFeld
untypeProgDecor (Decor info a) args = untypeProgSym a info args

untypeProg :: Untype dom dom =>
    ASTF (Decor Info dom) a -> UntypedFeld
untypeProg = simpleMatch untypeProgDecor

-- | Implementation of 'untypeProgSym' that generates code into a fresh
-- variable.
untypeProgFresh :: Untype sub dom
    => sub a
    -> Info (DenResult a)
    -> Args (AST (Decor Info dom)) a
    -> UntypedFeld
untypeProgFresh = untypeProgSym

-- Array
instance ( Untype dom dom
         , Project (CLambda Type) dom
         , Project (Literal  :|| Type) dom
         , Project (Core.Variable :|| Type) dom
         , Project Core.Let dom
         , Project (Array :|| Type) dom
         , Project (Tuple :|| Type) dom
         , ConstrainedBy dom Typeable
         , AlphaEq dom dom (Decor Info dom) [(VarId, VarId)]
         )
      => Untype (Array :|| Type) dom
  where
    untypeProgSym (C' Parallel) info (len :* ixf :* Nil)
        = In (Ut.Parallel (untypeProg len) (untypeProg ixf))

    untypeProgSym (C' Sequential) info (len :* st :* ixf :* Nil)
        = In (Ut.Sequential (untypeProg len) (untypeProg st) (untypeProg ixf))

    untypeProgSym (C' Append) info (a :* b :* Nil)
        = In (Ut.Append (untypeProg a) (untypeProg b))

    untypeProgSym (C' SetIx) info (arr :* i :* a :* Nil)
        = In (Ut.SetIx (untypeProg arr) (untypeProg i) (untypeProg a))

    untypeProgSym (C' GetIx) info (arr :* i :* Nil)
        = In (Ut.GetIx (untypeProg arr) (untypeProg i))

    untypeProgSym (C' SetLength) info (len :* arr :* Nil)
        = In (Ut.SetLength (untypeProg len) (untypeProg arr))

    untypeProgSym (C' GetLength) info (a :* Nil)
        = In (Ut.GetLength (untypeProg a))

-- Binding
instance Untype (Core.Variable :|| Type) dom
  where
    untypeProgSym (C' (Core.Variable v)) info Nil
        = In (Ut.Variable (Ut.Var (varInteger v) t'))
           where t' = untypeType (infoType info) (infoSize info)

instance (Untype dom dom,Project (CLambda Type) dom) => Untype (CLambda Type) dom
  where
    untypeProgSym (SubConstr2 (Lambda v)) info (body :* Nil)
     = In (Ut.Lambda (Ut.Var (varInteger v) t') (untypeProg body))
        where t' = untypeType (argType $ infoType info) (fst $ infoSize info)

instance ( Untype dom dom
         , Project (CLambda Type) dom
         ) => Untype Core.Let dom
  where
    untypeProgSym Core.Let info (a :* b :* Nil)
        = In (Ut.Let (untypeProg a) (untypeProg b))

-- Condition
instance Untype dom dom => Untype (Condition :|| Type) dom
  where
    untypeProgSym (C' Condition) info (cond :* tHEN :* eLSE :* Nil)
      = In (Ut.Condition (untypeProg cond) (untypeProg tHEN) (untypeProg eLSE))

instance Untype dom dom => Untype (ConditionM m) dom
  where
    untypeProgSym ConditionM info (cond :* tHEN :* eLSE :* Nil)
      = In (Ut.ConditionM (untypeProg cond) (untypeProg tHEN) (untypeProg eLSE))

-- Error
instance (Untype dom dom) => Untype (Error :|| Type) dom
  where
    untypeProgSym (C' Undefined)    info Nil = In (Ut.Undefined)
    untypeProgSym (C' (Assert msg)) info (cond :* a :* Nil)
     = In (Ut.Assert (untypeProg cond) (untypeProg a))

instance ( Untype dom dom
         , Render dom
         , Project (CLambda Type) dom
         , Project (Core.Variable :|| Type) dom
         , Project (Literal :|| Type) dom
         , Project Core.Let dom
         , Project (ElementsFeat :|| Type) dom
         )
      => Untype (ElementsFeat :|| Type) dom
  where
    untypeProgSym (C' EMaterialize) info (len :* arr :* Nil)
      = In (Ut.EMaterialize (untypeProg len) (untypeProg arr))

    untypeProgSym (C' EWrite) info (ix :* e :* Nil)
      = In (Ut.EWrite (untypeProg ix) (untypeProg e))

    untypeProgSym (C' EPar) info (p1 :* p2 :* Nil)
      = In (Ut.EPar (untypeProg p1) (untypeProg p2))

    untypeProgSym (C' EparFor) info (len :* b :* Nil)
      = In (Ut.EparFor (untypeProg len) (untypeProg b))

    untypeProgSym (C' ESkip) info Nil = In (Ut.ESkip)

-- FFI
instance (Untype dom dom) => Untype (FFI :|| Type) dom
  where -- No use for second argument at this stage.
    untypeProgSym (C' (ForeignImport name _)) info args
      = In (Ut.ForeignImport name (listArgs untypeProg args))

-- Future
instance Untype dom dom => Untype (FUTURE :|| Type) dom
  where
    untypeProgSym (C' MkFuture) info (p :* Nil)
      = In (Ut.MkFuture (untypeProg p))

    untypeProgSym (C' Await) info (a :* Nil)
      = In (Ut.Await (untypeProg a))

-- Literal
instance Untype (Literal :|| Type) dom
  where
    untypeProgSym t@(C' (Literal a)) info Nil
      = In (Ut.Literal (literal (infoType info) (infoSize info) a))

literal :: TypeRep a -> Size a -> a -> Lit
literal t@UnitType        sz a = literalConst t sz a
literal t@BoolType        sz a = literalConst t sz a
literal t@IntType{}       sz a = literalConst t sz a
literal t@FloatType       sz a = literalConst t sz a
literal t@DoubleType      sz a = literalConst t sz a
literal t@ComplexType{}   sz a = literalConst t sz a
literal t@ArrayType{}     sz a = literalConst t sz a
literal (Tup2Type ta tb) (sa,sb) (a,b)
    = LTup2 (literal ta sa a) (literal tb sb b)

literal (Tup3Type ta tb tc) (sa,sb,sc) (a,b,c)
    = LTup3 (literal ta sa a) (literal tb sb b) (literal tc sc c)

literal (Tup4Type ta tb tc td) (sa,sb,sc,sd) (a,b,c,d)
    = LTup4 (literal ta sa a) (literal tb sb b) (literal tc sc c)
            (literal td sd d)

literal (Tup5Type ta tb tc td te) (sa,sb,sc,sd,se) (a,b,c,d,e)
    = LTup5 (literal ta sa a) (literal tb sb b) (literal tc sc c)
            (literal td sd d) (literal te se e)

literal (Tup6Type ta tb tc td te tf) (sa,sb,sc,sd,se,sf) (a,b,c,d,e,f)
    = LTup6 (literal ta sa a) (literal tb sb b) (literal tc sc c)
            (literal td sd d) (literal te se e) (literal tf sf f)

literal (Tup7Type ta tb tc td te tf tg) (sa,sb,sc,sd,se,sf,sg) (a,b,c,d,e,f,g)
    = LTup7 (literal ta sa a) (literal tb sb b) (literal tc sc c)
            (literal td sd d) (literal te se e) (literal tf sf f)
            (literal tg sg g)
literal t s a = error "Missing pattern: FromTyped.hs: literal"

literalConst :: TypeRep a -> Size a -> a -> Lit
literalConst UnitType        _  ()     = LUnit
literalConst BoolType        _  a      = LBool a
literalConst (IntType s n)   sz a      = LInt (convSign s) (convSize n) (toInteger a)
literalConst FloatType       _  a      = LFloat a
literalConst DoubleType      _  a      = LDouble a
literalConst (ArrayType t)   _  a      = LArray t' $ map (literalConst t (defaultSize t)) a
  where t' = untypeType t (defaultSize t)
literalConst (ComplexType t) _  (r:+i) = LComplex re ie
  where re = literalConst t (defaultSize t) r
        ie = literalConst t (defaultSize t) i


-- Loop
instance ( Untype dom dom
         , Project (CLambda Type) dom
         , Project (Literal  :|| Type) dom
         , Project (Core.Variable :|| Type) dom
         , Project Core.Let dom
         , ConstrainedBy dom Typeable
         )
      => Untype (Loop :|| Type) dom
  where
    untypeProgSym (C' ForLoop) info (len :* init :* b :* Nil)
         = In (Ut.ForLoop (untypeProg len) (untypeProg init) (untypeProg b))

    untypeProgSym (C' WhileLoop) info (init :* a :* b :* Nil)
        = In (Ut.WhileLoop (untypeProg init) (untypeProg a) (untypeProg b))

instance ( Untype dom dom
         , Project (CLambda Type) dom
         , Project (Literal  :|| Type) dom
         , Project (Core.Variable :|| Type) dom
         )
      => Untype (LoopM Mut) dom
  where
    untypeProgSym For info (len :* a :* Nil)
        = In (Ut.For (untypeProg len) (untypeProg a))

    untypeProgSym While info (cond :* step :* Nil)
        = In (Ut.While (untypeProg cond) (untypeProg step))

-- MutableToPure
instance ( Untype dom dom
         , Project (CLambda Type) dom
         , Project (Core.Variable :|| Type) dom
         , Project (MONAD Mut) dom
         , Project MutableArray dom
         )
      => Untype MutableToPure dom
  where
    untypeProgSym WithArray info (marr :* b :* Nil)
        = In (Ut.WithArray (untypeProg marr) (untypeProg b))

    untypeProgSym RunMutableArray info (marr :* Nil)
        = In (Ut.RunMutableArray (untypeProg marr))


instance ( Untype dom dom
         , Project (CLambda Type) dom
         , Project ParFeature dom
         )
      => Untype (MONAD Par) dom
  where
    untypeProgSym Bind info (ma :* mb :* Nil)
        = In (Ut.Bind (untypeProg ma) (untypeProg mb))

    untypeProgSym Then info (ma :* mb :* Nil)
        = In (Ut.Then (untypeProg ma) (untypeProg mb))

    untypeProgSym Return info (a :* Nil)
        = In (Ut.Return (untypeProg a))

    untypeProgSym When info (c :* action :* Nil)
        = In (Ut.When (untypeProg c) (untypeProg action))

instance ( Untype dom dom
         , Project (CLambda Type) dom
         )
      => Untype (MONAD Mut) dom
  where
    untypeProgSym Bind info (ma :* mb :* Nil)
        = In (Ut.Bind (untypeProg ma) (untypeProg mb))

    untypeProgSym Then info (ma :* mb :* Nil)
        = In (Ut.Then (untypeProg ma) (untypeProg mb))

    untypeProgSym Return info (a :* Nil)
        = In (Ut.Return (untypeProg a))

    untypeProgSym When info (c :* action :* Nil)
        = In (Ut.When (untypeProg c) (untypeProg action))

instance ( Untype dom dom
         , Project (CLambda Type) dom
         ) => Untype MutableArray dom
  where
    untypeProgSym NewArr_ info (len :* Nil) = In (Ut.NewArr_ (untypeProg len))

    untypeProgSym NewArr info (len :* a :* Nil)
      = In (Ut.NewArr (untypeProg len) (untypeProg a))

    untypeProgSym GetArr info (arr :* i :* Nil)
      = In (Ut.GetArr (untypeProg arr) (untypeProg i))

    untypeProgSym SetArr info (arr :* i :* a :* Nil)
      = In (Ut.SetArr (untypeProg arr) (untypeProg i) (untypeProg a))

    untypeProgSym ArrLength info (arr :* Nil)
      = In (Ut.ArrLength (untypeProg arr))

-- MutableRef
instance (Untype dom dom, Project (CLambda Type) dom) => Untype Mutable dom
  where
    untypeProgSym Run info (ma :* Nil) = In (Ut.Run (untypeProg ma))

instance ( Untype dom dom
         , Project (CLambda Type) dom
         ) => Untype MutableReference dom
  where
    untypeProgSym NewRef info (a :* Nil) = In (Ut.NewRef (untypeProg a))
    untypeProgSym GetRef info (r :* Nil) = In (Ut.GetRef (untypeProg r))
    untypeProgSym SetRef info (r :* a :* Nil)
       = In (Ut.SetRef (untypeProg r) (untypeProg a))
    untypeProgSym ModRef info (r :* a :* Nil)
       = In (Ut.ModRef (untypeProg r) (untypeProg a))

-- Noinline
instance Untype dom dom => Untype (NoInline :|| Type) dom
  where
    untypeProgSym (C' NoInline) info (p :* Nil)
      = In (Ut.NoInline (untypeProg p))

-- Par
instance ( Untype dom dom
         , Project (Core.Variable :|| Type) dom
         )
      => Untype ParFeature dom
  where
    untypeProgSym ParRun info (p :* Nil) = In (Ut.ParRun (untypeProg p))

    untypeProgSym ParNew info Nil = In Ut.ParNew

    untypeProgSym ParGet info (r :* Nil) = In (Ut.ParGet (untypeProg r))

    untypeProgSym ParPut info (r :* a :* Nil)
      = In (Ut.ParPut (untypeProg r) (untypeProg a))

    untypeProgSym ParFork info (p :* Nil) = In (Ut.ParFork (untypeProg p))

    untypeProgSym ParYield info Nil = In Ut.ParYield

-- Primitive

-- | Converts symbols to primitive function calls
instance Untype dom dom => Untype Semantics dom
  where
    untypeProgSym (Sem name _) args = error "untypesemantics"

-- | Convenient implementation of 'untypeExprSym' for primitive functions
untypePrim :: (Semantic expr, Untype dom dom)
    => (expr :|| Type) a
    -> Info (DenResult a)
    -> Args (AST (Decor Info dom)) a
    -> UntypedFeld
untypePrim (C' s) info = untypeProgSym (semantics s) info

instance Untype dom dom => Untype (BITS       :|| Type) dom
   where
     untypeProgSym (C' BAnd) info (a :* b :* Nil)
        = In (Ut.PrimApp2 Ut.BAnd t' (untypeProg a) (untypeProg b))
          where t' = untypeType (infoType info) (infoSize info)
     untypeProgSym (C' BOr) info (a :* b :* Nil)
        = In (Ut.PrimApp2 Ut.BOr t' (untypeProg a) (untypeProg b))
          where t' = untypeType (infoType info) (infoSize info)
     untypeProgSym (C' BXor) info (a :* b :* Nil)
        = In (Ut.PrimApp2 Ut.BXor t' (untypeProg a) (untypeProg b))
          where t' = untypeType (infoType info) (infoSize info)
     untypeProgSym (C' Complement) info (a :* Nil)
        = In (Ut.PrimApp1 Ut.Complement t' (untypeProg a))
          where t' = untypeType (infoType info) (infoSize info)
     untypeProgSym (C' Bit) info (a :* Nil)
        = In (Ut.PrimApp1 Ut.Bit t' (untypeProg a))
          where t' = untypeType (infoType info) (infoSize info)
     untypeProgSym (C' SetBit) info (a :* b :* Nil)
        = In (Ut.PrimApp2 Ut.SetBit t' (untypeProg a) (untypeProg b))
          where t' = untypeType (infoType info) (infoSize info)
     untypeProgSym (C' ClearBit) info (a :* b :* Nil)
        = In (Ut.PrimApp2 Ut.ClearBit t' (untypeProg a) (untypeProg b))
          where t' = untypeType (infoType info) (infoSize info)
     untypeProgSym (C' ComplementBit) info (a :* b :* Nil)
        = In (Ut.PrimApp2 Ut.ComplementBit t' (untypeProg a) (untypeProg b))
          where t' = untypeType (infoType info) (infoSize info)
     untypeProgSym (C' TestBit) info (a :* b :* Nil)
        = In (Ut.PrimApp2 Ut.TestBit t' (untypeProg a) (untypeProg b))
          where t' = untypeType (infoType info) (infoSize info)
     untypeProgSym (C' ShiftLU) info (a :* b :* Nil)
        = In (Ut.PrimApp2 Ut.ShiftLU t' (untypeProg a) (untypeProg b))
          where t' = untypeType (infoType info) (infoSize info)
     untypeProgSym (C' ShiftRU) info (a :* b :* Nil)
        = In (Ut.PrimApp2 Ut.ShiftRU t' (untypeProg a) (untypeProg b))
          where t' = untypeType (infoType info) (infoSize info)
     untypeProgSym (C' ShiftL) info (a :* b :* Nil)
        = In (Ut.PrimApp2 Ut.ShiftL t' (untypeProg a) (untypeProg b))
          where t' = untypeType (infoType info) (infoSize info)
     untypeProgSym (C' ShiftR) info (a :* b :* Nil)
        = In (Ut.PrimApp2 Ut.ShiftR t' (untypeProg a) (untypeProg b))
          where t' = untypeType (infoType info) (infoSize info)
     untypeProgSym (C' RotateLU) info (a :* b :* Nil)
        = In (Ut.PrimApp2 Ut.RotateLU t' (untypeProg a) (untypeProg b))
          where t' = untypeType (infoType info) (infoSize info)
     untypeProgSym (C' RotateRU) info (a :* b :* Nil)
        = In (Ut.PrimApp2 Ut.RotateRU t' (untypeProg a) (untypeProg b))
          where t' = untypeType (infoType info) (infoSize info)
     untypeProgSym (C' RotateL) info (a :* b :* Nil)
        = In (Ut.PrimApp2 Ut.RotateL t' (untypeProg a) (untypeProg b))
          where t' = untypeType (infoType info) (infoSize info)
     untypeProgSym (C' RotateR) info (a :* b :* Nil)
        = In (Ut.PrimApp2 Ut.RotateR t' (untypeProg a) (untypeProg b))
          where t' = untypeType (infoType info) (infoSize info)
     untypeProgSym (C' ReverseBits) info (a :* Nil)
        = In (Ut.PrimApp1 Ut.ReverseBits t' (untypeProg a))
          where t' = untypeType (infoType info) (infoSize info)
     untypeProgSym (C' BitScan) info (a :* Nil)
        = In (Ut.PrimApp1 Ut.BitScan t' (untypeProg a))
          where t' = untypeType (infoType info) (infoSize info)
     untypeProgSym (C' BitCount) info (a :* Nil)
        = In (Ut.PrimApp1 Ut.BitCount t' (untypeProg a))
          where t' = untypeType (infoType info) (infoSize info)

instance Untype dom dom => Untype (COMPLEX    :|| Type) dom
   where
      untypeProgSym (C' MkComplex) info (a :* b :* Nil)
        = In (Ut.MkComplex (untypeProg a) (untypeProg b))
      untypeProgSym (C' RealPart) info (a :* Nil)
        = In (Ut.RealPart (untypeProg a))
      untypeProgSym (C' ImagPart) info (a :* Nil)
        = In (Ut.ImagPart (untypeProg a))
      untypeProgSym (C' MkPolar) info (a :* b :* Nil)
        = In (Ut.MkPolar (untypeProg a) (untypeProg b))
      untypeProgSym (C' Conjugate) info (a :* Nil)
        = In (Ut.Conjugate (untypeProg a))
      untypeProgSym (C' Magnitude) info (a :* Nil)
        = In (Ut.Magnitude (untypeProg a))
      untypeProgSym (C' Phase) info (a :* Nil)
        = In (Ut.Phase (untypeProg a))
      untypeProgSym (C' Cis) info (a :* Nil)
        = In (Ut.Cis (untypeProg a))

instance Untype dom dom => Untype (Conversion :|| Type) dom
  where
      untypeProgSym (C' F2I) info (a :* Nil)
        = In (Ut.F2I (untypeType (infoType info) (infoSize info)) (untypeProg a))
      untypeProgSym (C' I2N) info (a :* Nil)
        = In (Ut.I2N (untypeType (infoType info) (infoSize info)) (untypeProg a))
      untypeProgSym (C' B2I) info (a :* Nil)
        = In (Ut.B2I (untypeType (infoType info) (infoSize info)) (untypeProg a))
      untypeProgSym (C' Round) info (a :* Nil)
        = In (Ut.Round (untypeType (infoType info) (infoSize info)) (untypeProg a))
      untypeProgSym (C' Ceiling) info (a :* Nil)
        = In (Ut.Ceiling (untypeType (infoType info) (infoSize info)) (untypeProg a))
      untypeProgSym (C' Floor) info (a :* Nil)
        = In (Ut.Floor (untypeType (infoType info) (infoSize info)) (untypeProg a))

instance Untype dom dom => Untype (EQ         :|| Type) dom
  where
      untypeProgSym (C' Equal) info (a :* b :* Nil)
        = In (Ut.Equal (untypeProg a) (untypeProg b))
      untypeProgSym (C' NotEqual) info (a :* b :* Nil)
        = In (Ut.NotEqual (untypeProg a) (untypeProg b))

instance Untype dom dom => Untype (FLOATING   :|| Type) dom
  where
      untypeProgSym (C' Pi) info Nil
        = In (Ut.Pi)
      untypeProgSym (C' Feldspar.Core.Constructs.Floating.Exp) info (a :* Nil)
        = In (Ut.Exp (untypeProg a))
      untypeProgSym (C' Sqrt) info (a :* Nil)
        = In (Ut.Sqrt (untypeProg a))
      untypeProgSym (C' Log) info (a :* Nil)
        = In (Ut.Log (untypeProg a))
      untypeProgSym (C' Pow) info (a :* b :* Nil)
        = In (Ut.Pow (untypeProg a) (untypeProg b))
      untypeProgSym (C' LogBase) info (a :* b :* Nil)
        = In (Ut.LogBase (untypeProg a) (untypeProg b))
      untypeProgSym (C' Sin) info (a :* Nil)
        = In (Ut.Sin (untypeProg a))
      untypeProgSym (C' Tan) info (a :* Nil)
        = In (Ut.Tan (untypeProg a))
      untypeProgSym (C' Cos) info (a :* Nil)
        = In (Ut.Cos (untypeProg a))
      untypeProgSym (C' Asin) info (a :* Nil)
        = In (Ut.Asin (untypeProg a))
      untypeProgSym (C' Atan) info (a :* Nil)
        = In (Ut.Atan (untypeProg a))
      untypeProgSym (C' Acos) info (a :* Nil)
        = In (Ut.Acos (untypeProg a))
      untypeProgSym (C' Sinh) info (a :* Nil)
        = In (Ut.Sinh (untypeProg a))
      untypeProgSym (C' Tanh) info (a :* Nil)
        = In (Ut.Tanh (untypeProg a))
      untypeProgSym (C' Cosh) info (a :* Nil)
        = In (Ut.Cosh (untypeProg a))
      untypeProgSym (C' Asinh) info (a :* Nil)
        = In (Ut.Asinh (untypeProg a))
      untypeProgSym (C' Atanh) info (a :* Nil)
        = In (Ut.Atanh (untypeProg a))
      untypeProgSym (C' Acosh) info (a :* Nil)
        = In (Ut.Acosh (untypeProg a))

instance Untype dom dom => Untype (FRACTIONAL :|| Type) dom
  where
      untypeProgSym (C' DivFrac) info (a :* b :* Nil)
        = In (Ut.DivFrac (untypeProg a) (untypeProg b))

instance Untype dom dom => Untype (INTEGRAL   :|| Type) dom
  where
      untypeProgSym (C' Quot) info (a :* b :* Nil)
        = In (Ut.Quot (untypeProg a) (untypeProg b))
      untypeProgSym (C' Rem) info (a :* b :* Nil)
        = In (Ut.Rem (untypeProg a) (untypeProg b))
      untypeProgSym (C' Div) info (a :* b :* Nil)
        = In (Ut.Div (untypeProg a) (untypeProg b))
      untypeProgSym (C' Mod) info (a :* b :* Nil)
        = In (Ut.Mod (untypeProg a) (untypeProg b))
      untypeProgSym (C' Feldspar.Core.Constructs.Integral.Exp) info (a :* b :* Nil)
        = In (Ut.IExp (untypeProg a) (untypeProg b))

instance Untype dom dom => Untype (Logic      :|| Type) dom
  where
      untypeProgSym (C' And) info (a :* b :* Nil)
        = In (Ut.And (untypeProg a) (untypeProg b))
      untypeProgSym (C' Or) info (a :* b :* Nil)
        = In (Ut.Or (untypeProg a) (untypeProg b))
      untypeProgSym (C' Not) info (a :* Nil)
        = In (Ut.Not (untypeProg a))

instance Untype dom dom => Untype (NUM        :|| Type) dom
  where
      untypeProgSym (C' Abs) info (a :* Nil)
        = In (Ut.Abs (untypeProg a))
      untypeProgSym (C' Sign) info (a :* Nil)
        = In (Ut.Sign (untypeProg a))
      untypeProgSym (C' Add) info (a :* b :* Nil)
        = In (Ut.Add (untypeProg a) (untypeProg b))
      untypeProgSym (C' Sub) info (a :* b :* Nil)
        = In (Ut.Sub (untypeProg a) (untypeProg b))
      untypeProgSym (C' Mul) info (a :* b :* Nil)
        = In (Ut.Mul (untypeProg a) (untypeProg b))

instance Untype dom dom => Untype (ORD        :|| Type) dom
  where
      untypeProgSym (C' LTH) info (a :* b :* Nil)
        = In (Ut.LTH (untypeProg a) (untypeProg b))
      untypeProgSym (C' GTH) info (a :* b :* Nil)
        = In (Ut.GTH (untypeProg a) (untypeProg b))
      untypeProgSym (C' LTE) info (a :* b :* Nil)
        = In (Ut.LTE (untypeProg a) (untypeProg b))
      untypeProgSym (C' GTE) info (a :* b :* Nil)
        = In (Ut.GTE (untypeProg a) (untypeProg b))
      untypeProgSym (C' Min) info (a :* b :* Nil)
        = In (Ut.Min (untypeProg a) (untypeProg b))
      untypeProgSym (C' Max) info (a :* b :* Nil)
        = In (Ut.Max (untypeProg a) (untypeProg b))

instance Untype dom dom => Untype (REALFLOAT  :|| Type) dom
  where
      untypeProgSym (C' Atan2) info (a :* b :* Nil)
        = In (Ut.Atan2 (untypeProg a) (untypeProg b))

instance Untype dom dom => Untype (Trace      :|| Type) dom
  where
      untypeProgSym (C' Trace) info (a :* b :* Nil)
        = In (Ut.Trace (untypeProg a) (untypeProg b))

-- Save
instance Untype dom dom => Untype (Save :|| Type) dom
  where
    untypeProgSym (C' Save) info (a :* Nil) = In (Ut.Save (untypeProg a))

-- Sizeprop
instance Untype dom dom => Untype (PropSize :|| Type) dom
  where
    untypeProgSym (C' (PropSize _)) info (_ :* b :* Nil)
      = In (Ut.PropSize (untypeProg b))

-- SourceInfo
instance Untype dom dom => Untype (Decor SourceInfo1 Identity :|| Type) dom
  where
    untypeProgSym (C' (Decor info Id)) _ (a :* Nil)
      = In (Ut.SourceInfo (untypeProg a))

-- Switch
instance ( Untype dom dom
         , Project (EQ :|| Type) dom
         , Project (Condition :|| Type) dom
         )
      => Untype (Switch :|| Type) dom
  where
    untypeProgSym (C' Switch) info (tree :* Nil)
        = In (Ut.Switch (untypeProg tree))

-- Tuple
instance Untype dom dom => Untype (Tuple :|| Type) dom
  where
    untypeProgSym (C' Tup2) info (m1 :* m2 :* Nil)
      = In (Ut.Tup2 (untypeProg m1) (untypeProg m2))
    untypeProgSym (C' Tup3) info (m1 :* m2 :* m3 :* Nil)
      = In (Ut.Tup3 (untypeProg m1) (untypeProg m2) (untypeProg m3))
    untypeProgSym (C' Tup4) info (m1 :* m2 :* m3 :* m4 :* Nil)
      = In (Ut.Tup4 (untypeProg m1) (untypeProg m2) (untypeProg m3) (untypeProg m4))
    untypeProgSym (C' Tup5) info (m1 :* m2 :* m3 :* m4 :* m5 :* Nil)
      = In (Ut.Tup5 (untypeProg m1) (untypeProg m2) (untypeProg m3) (untypeProg m4) (untypeProg m5))
    untypeProgSym (C' Tup6) info (m1 :* m2 :* m3 :* m4 :* m5 :* m6 :* Nil)
      = In (Ut.Tup6 (untypeProg m1) (untypeProg m2) (untypeProg m3) (untypeProg m4) (untypeProg m5) (untypeProg m6))
    untypeProgSym (C' Tup7) info (m1 :* m2 :* m3 :* m4 :* m5 :* m6 :* m7 :* Nil)
      = In (Ut.Tup7 (untypeProg m1) (untypeProg m2) (untypeProg m3) (untypeProg m4) (untypeProg m5) (untypeProg m6)  (untypeProg m7))

instance Untype dom dom => Untype (Select :|| Type) dom
  where
    untypeProgSym (C' Sel1) info (tup :* Nil) = In (Ut.Sel1 (untypeProg tup))
    untypeProgSym (C' Sel2) info (tup :* Nil) = In (Ut.Sel2 (untypeProg tup))
    untypeProgSym (C' Sel3) info (tup :* Nil) = In (Ut.Sel3 (untypeProg tup))
    untypeProgSym (C' Sel4) info (tup :* Nil) = In (Ut.Sel4 (untypeProg tup))
    untypeProgSym (C' Sel5) info (tup :* Nil) = In (Ut.Sel5 (untypeProg tup))
    untypeProgSym (C' Sel6) info (tup :* Nil) = In (Ut.Sel6 (untypeProg tup))
    untypeProgSym (C' Sel7) info (tup :* Nil) = In (Ut.Sel7 (untypeProg tup))

untypeType :: TypeRep a -> Size a -> Ut.Type
untypeType UnitType _               = Ut.UnitType
untypeType BoolType _               = Ut.BoolType
untypeType (IntType s n) _          = Ut.IntType (convSign s) (convSize n)
untypeType FloatType _              = Ut.FloatType
untypeType DoubleType _             = Ut.DoubleType
untypeType (ComplexType t) _        = Ut.ComplexType (untypeType t (defaultSize t))
untypeType (Tup2Type a b) (sa,sb)
  = Ut.Tup2Type (untypeType a sa) (untypeType b sb)
untypeType (Tup3Type a b c) (sa,sb,sc)
  = Ut.Tup3Type (untypeType a sa) (untypeType b sb) (untypeType c sc)
untypeType (Tup4Type a b c d) (sa,sb,sc,sd)
  = Ut.Tup4Type (untypeType a sa) (untypeType b sb) (untypeType c sc)
                (untypeType d sd)
untypeType (Tup5Type a b c d e) (sa,sb,sc,sd,se)
  = Ut.Tup5Type (untypeType a sa) (untypeType b sb) (untypeType c sc)
                (untypeType d sd) (untypeType e se)
untypeType (Tup6Type a b c d e f) (sa,sb,sc,sd,se,sf)
  = Ut.Tup6Type (untypeType a sa) (untypeType b sb) (untypeType c sc)
                (untypeType d sd) (untypeType e se) (untypeType f sf)
untypeType (Tup7Type a b c d e f g) (sa,sb,sc,sd,se,sf,sg)
  = Ut.Tup7Type (untypeType a sa) (untypeType b sb) (untypeType c sc)
                (untypeType d sd) (untypeType e se) (untypeType f sf)
                (untypeType g sg)
untypeType (MutType a) _            = Ut.MutType (untypeType a (defaultSize a))
untypeType (RefType a) _            = Ut.RefType (untypeType a (defaultSize a))
untypeType (ArrayType a) (rs :> es) = Ut.ArrayType rs (untypeType a es)
untypeType (MArrType a) (rs :> es)  = Ut.MArrType rs (untypeType a es)
untypeType (ParType a) _            = Ut.ParType (untypeType a (defaultSize a))
untypeType (ElementsType a) (rs :> es) = Ut.ElementsType (untypeType a es)
untypeType (IVarType a) _           = Ut.IVarType $ untypeType a $ defaultSize a
untypeType (FunType a b) (sa, sz)   = Ut.FunType (untypeType a sa) (untypeType b sz)
untypeType (FValType a) sz          = Ut.FValType (untypeType a sz)
untypeType typ _                    = error $ "untypeType: missing "

convSign :: Signedness a -> Ut.Signedness
convSign U       = Unsigned
convSign S       = Signed

convSize :: BitWidth a -> Ut.Size
convSize N8      = S8
convSize N16     = S16
convSize N32     = S32
convSize N64     = S64
convSize NNative = S32
