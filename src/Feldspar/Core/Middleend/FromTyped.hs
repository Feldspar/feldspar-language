{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Feldspar.Core.Middleend.FromTyped
  ( untype
  , untypeType
  )
  where

import Data.Complex
import Data.Typeable (Typeable)

import Language.Syntactic
import Language.Syntactic.Constructs.Binding hiding (Variable, Let)
import qualified Language.Syntactic.Constructs.Binding as SynBind
import Language.Syntactic.Constructs.Binding.HigherOrder hiding (Let)

import Feldspar.Range (Range(..), upperBound)

import Feldspar.Core.Types
import Feldspar.Core.Interpretation hiding (literal, optimize)
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
import Feldspar.Core.Constructs.Tuple
import qualified Feldspar.Core.Constructs.Binding as Core
import Feldspar.Core.UntypedRepresentation hiding ( Lambda, UntypedFeldF(..)
                                                  , Size, Type(..), Signedness
                                                  , Op(..)
                                                  )
import qualified Feldspar.Core.UntypedRepresentation as Ut
import Feldspar.Core.Middleend.CreateTasks
import Feldspar.Core.Middleend.LetSinking
import Feldspar.Core.Middleend.OptimizeUntyped
import Feldspar.ValueInfo hiding (toValueInfo)

-- A self contained translation from the Syntactic format into UntypedFeld.
--
-- The file begins with the necessary Untype-functions and
-- Untype-instances and tucks in some local helper functions at the
-- end. "untype" is the only exported function.

-- | A minimal complete instance has to define either 'untypeProgSym' or
-- 'untypeExprSym'.
class Untype sub dom
  where
    untypeProgSym
        :: sub a
        -> Info (DenResult a)
        -> Args (AST (Decor Info dom)) a
        -> AUntypedFeld ValueInfo
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
    -> AUntypedFeld ValueInfo
untypeProgDecor (Decor info a) = untypeProgSym a info

-- | External module interface.
untype :: Untype dom dom => FeldOpts -> ASTF (Decor Info dom) a ->  UntypedFeld
untype opts = createTasks opts . optimize . sinkLets . unAnnotate . untypeProg

untypeProg :: Untype dom dom =>
    ASTF (Decor Info dom) a -> AUntypedFeld ValueInfo
untypeProg = simpleMatch untypeProgDecor

-- | Implementation of 'untypeProgSym' that generates code into a fresh
-- variable.
untypeProgFresh :: Untype sub dom
    => sub a
    -> Info (DenResult a)
    -> Args (AST (Decor Info dom)) a
    -> AUntypedFeld ValueInfo
untypeProgFresh = untypeProgSym

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
        = AIn r' (Ut.App Ut.Parallel t' [untypeProg len, untypeProg ixf])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym (C' Sequential) info (len :* st :* ixf :* Nil)
        = AIn r' (Ut.App Ut.Sequential t' [untypeProg len, untypeProg st, untypeProg ixf])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym (C' Append) info (a :* b :* Nil)
        = AIn r' (Ut.App Ut.Append t' [untypeProg a, untypeProg b])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym (C' SetIx) info (arr :* i :* a :* Nil)
        = AIn r' (Ut.App Ut.SetIx t' [untypeProg arr, untypeProg i, untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym (C' GetIx) info (arr :* i :* Nil)
        = AIn r' (Ut.App Ut.GetIx t' [untypeProg arr, untypeProg i])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym (C' SetLength) info (len :* arr :* Nil)
        = AIn r' (Ut.App Ut.SetLength t' [untypeProg len, untypeProg arr])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym (C' GetLength) info (a :* Nil)
        = AIn r' (Ut.App Ut.GetLength t' [untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)

instance Untype (Core.Variable :|| Type) dom
  where
    untypeProgSym (C' (Core.Variable v)) info Nil
        = AIn r' (Ut.Variable (Ut.Var v t'))
           where t' = untypeType (infoType info) (infoSize info)
                 r' = toValueInfo (infoType info) (infoSize info)

instance (Untype dom dom,Project (CLambda Type) dom) => Untype (CLambda Type) dom
  where
    untypeProgSym (SubConstr2 (Lambda v)) info (body :* Nil)
     = AIn r' (Ut.Lambda (Ut.Var v t') body')
        where t' = untypeType (argType $ infoType info) (fst $ infoSize info)
              -- The value info of a function is that of its return value.
              body'@(AIn r' _) = untypeProg body

instance ( Untype dom dom
         , Project (CLambda Type) dom
         ) => Untype Core.Let dom
  where
    untypeProgSym Core.Let info (a :* b :* Nil)
        = AIn r' (Ut.App Ut.Let t' [untypeProg a, untypeProg b])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)

instance Untype dom dom => Untype (Condition :|| Type) dom
  where
    untypeProgSym (C' Condition) info (cond :* tHEN :* eLSE :* Nil)
      = AIn r' (Ut.App Ut.Condition t' [untypeProg cond, untypeProg tHEN, untypeProg eLSE])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)

instance Untype dom dom => Untype (ConditionM m) dom
  where
    untypeProgSym ConditionM info (cond :* tHEN :* eLSE :* Nil)
      = AIn r' (Ut.App Ut.ConditionM t' [untypeProg cond, untypeProg tHEN, untypeProg eLSE])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)

instance (Untype dom dom) => Untype (Error :|| Type) dom
  where
    untypeProgSym (C' Undefined)    info Nil
        = AIn r' (Ut.App Ut.Undefined t' [])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym (C' (Assert msg)) info (cond :* a :* Nil)
        = AIn r' (Ut.App (Ut.Assert msg) t' [untypeProg cond, untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)

instance ( Untype dom dom
         , Render dom
         , Project (CLambda Type) dom
         , Project (Core.Variable :|| Type) dom
         , Project (Literal :|| Type) dom
         , Project Core.Let dom
         , Project ElementsFeat dom
         )
      => Untype ElementsFeat dom
  where
    untypeProgSym EMaterialize info (len :* arr :* Nil)
      = AIn r' (Ut.App Ut.EMaterialize t' [untypeProg len, untypeProg arr])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym EWrite info (ix :* e :* Nil)
      = AIn r' (Ut.App Ut.EWrite t' [untypeProg ix, untypeProg e])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym EPar info (p1 :* p2 :* Nil)
      = AIn r' (Ut.App Ut.EPar t' [untypeProg p1, untypeProg p2])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym EparFor info (len :* b :* Nil)
      = AIn r' (Ut.App Ut.EparFor t' [untypeProg len, untypeProg b])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym ESkip info Nil
      = AIn r' (Ut.App Ut.ESkip t' [])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)

instance (Untype dom dom) => Untype (FFI :|| Type) dom
  where -- No use for second argument at this stage.
    untypeProgSym (C' (ForeignImport name _)) info args
      = AIn r' (Ut.App (Ut.ForeignImport name) t' (listArgs untypeProg args))
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)

instance Untype dom dom => Untype (FUTURE :|| Type) dom
  where
    untypeProgSym (C' MkFuture) info (p :* Nil)
      = AIn r' (Ut.App Ut.MkFuture t' [untypeProg p])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym (C' Await) info (a :* Nil)
      = AIn r' (Ut.App Ut.Await t' [untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)

instance Untype (Literal :|| Type) dom
  where
    untypeProgSym t@(C' (Literal a)) info Nil
      = AIn r' (Ut.Literal (literal (infoType info) (infoSize info) a))
          where r' = toValueInfo (infoType info) (infoSize info)

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
         = AIn r' (Ut.App Ut.ForLoop t' [untypeProg len, untypeProg init, untypeProg b])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym (C' WhileLoop) info (init :* a :* b :* Nil)
        = AIn r' (Ut.App Ut.WhileLoop t' [untypeProg init, untypeProg a, untypeProg b])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)

instance ( Untype dom dom
         , Project (CLambda Type) dom
         , Project (Literal  :|| Type) dom
         , Project (Core.Variable :|| Type) dom
         )
      => Untype (LoopM Mut) dom
  where
    untypeProgSym For info (len :* a :* Nil)
        = AIn r' (Ut.App Ut.For t' [untypeProg len, untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym While info (cond :* step :* Nil)
        = AIn r' (Ut.App Ut.While t' [untypeProg cond, untypeProg step])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)

instance ( Untype dom dom
         , Project (CLambda Type) dom
         , Project (Core.Variable :|| Type) dom
         , Project (MONAD Mut) dom
         , Project MutableArray dom
         )
      => Untype MutableToPure dom
  where
    untypeProgSym WithArray info (marr :* b :* Nil)
        = AIn r' (Ut.App Ut.WithArray t' [untypeProg marr, untypeProg b])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym RunMutableArray info (marr :* Nil)
        = AIn r' (Ut.App Ut.RunMutableArray t' [untypeProg marr])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)

instance ( Untype dom dom
         , Project (CLambda Type) dom
         , Project ParFeature dom
         )
      => Untype (MONAD Par) dom
  where
    untypeProgSym Bind info (ma :* mb :* Nil)
        = AIn r' (Ut.App Ut.Bind t' [untypeProg ma, untypeProg mb])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym Then info (ma :* mb :* Nil)
        = AIn r' (Ut.App Ut.Then t' [untypeProg ma, untypeProg mb])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym Return info (a :* Nil)
        = AIn r' (Ut.App Ut.Return t' [untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym When info (c :* action :* Nil)
        = AIn r' (Ut.App Ut.When t' [untypeProg c, untypeProg action])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)

instance ( Untype dom dom
         , Project (CLambda Type) dom
         )
      => Untype (MONAD Mut) dom
  where
    untypeProgSym Bind info (ma :* mb :* Nil)
        = AIn r' (Ut.App Ut.Bind t' [untypeProg ma, untypeProg mb])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym Then info (ma :* mb :* Nil)
        = AIn r' (Ut.App Ut.Then t' [untypeProg ma, untypeProg mb])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym Return info (a :* Nil)
        = AIn r' (Ut.App Ut.Return t' [untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym When info (c :* action :* Nil)
        = AIn r' (Ut.App Ut.When t' [untypeProg c, untypeProg action])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)

instance ( Untype dom dom
         , Project (CLambda Type) dom
         ) => Untype MutableArray dom
  where
    untypeProgSym NewArr_ info (len :* Nil)
        = AIn r' (Ut.App Ut.NewArr_ t' [untypeProg len])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym NewArr info (len :* a :* Nil)
        = AIn r' (Ut.App Ut.NewArr t' [untypeProg len, untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym GetArr info (arr :* i :* Nil)
        = AIn r' (Ut.App Ut.GetArr t' [untypeProg arr, untypeProg i])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym SetArr info (arr :* i :* a :* Nil)
        = AIn r' (Ut.App Ut.SetArr t' [untypeProg arr, untypeProg i, untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym ArrLength info (arr :* Nil)
        = AIn r' (Ut.App Ut.ArrLength t' [untypeProg arr])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)

instance (Untype dom dom, Project (CLambda Type) dom) => Untype Mutable dom
  where
    untypeProgSym Run info (ma :* Nil)
      = AIn r' (Ut.App Ut.Run t' [untypeProg ma])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)

instance ( Untype dom dom
         , Project (CLambda Type) dom
         ) => Untype MutableReference dom
  where
    untypeProgSym NewRef info (a :* Nil)
      = AIn r' (Ut.App Ut.NewRef t' [untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym GetRef info (r :* Nil)
      = AIn r' (Ut.App Ut.GetRef t' [untypeProg r])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym SetRef info (r :* a :* Nil)
      = AIn r' (Ut.App Ut.SetRef t' [untypeProg r, untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym ModRef info (r :* a :* Nil)
      = AIn r' (Ut.App Ut.ModRef t' [untypeProg r, untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)

instance Untype dom dom => Untype (NoInline :|| Type) dom
  where
    untypeProgSym (C' NoInline) info (p :* Nil)
      = AIn r' (Ut.App Ut.NoInline t' [untypeProg p])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)

instance ( Untype dom dom
         , Project (Core.Variable :|| Type) dom
         )
      => Untype ParFeature dom
  where
    untypeProgSym ParRun info (p :* Nil)
      = AIn r' (Ut.App Ut.ParRun t' [untypeProg p])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym ParNew info Nil
        = AIn r' (Ut.App Ut.ParNew t' [])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym ParGet info (r :* Nil)
      = AIn r' (Ut.App Ut.ParGet t' [untypeProg r])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym ParPut info (r :* a :* Nil)
      = AIn r' (Ut.App Ut.ParPut t' [untypeProg r, untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym ParFork info (p :* Nil)
      = AIn r' (Ut.App Ut.ParFork t' [untypeProg p])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym ParYield info Nil
        = AIn r' (Ut.App Ut.ParYield t' [])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)

-- | Converts symbols to primitive function calls
instance Untype dom dom => Untype Semantics dom
  where
    untypeProgSym (Sem name _) args = error "untypesemantics"

-- | Convenient implementation of 'untypeExprSym' for primitive functions
untypePrim :: (Semantic expr, Untype dom dom)
    => (expr :|| Type) a
    -> Info (DenResult a)
    -> Args (AST (Decor Info dom)) a
    -> AUntypedFeld ValueInfo
untypePrim (C' s) = untypeProgSym (semantics s)

instance Untype dom dom => Untype (BITS       :|| Type) dom
   where
     untypeProgSym (C' BAnd) info (a :* b :* Nil)
        = AIn r' (Ut.App Ut.BAnd t' [untypeProg a, untypeProg b])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
     untypeProgSym (C' BOr) info (a :* b :* Nil)
        = AIn r' (Ut.App Ut.BOr t' [untypeProg a, untypeProg b])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
     untypeProgSym (C' BXor) info (a :* b :* Nil)
        = AIn r' (Ut.App Ut.BXor t' [untypeProg a, untypeProg b])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
     untypeProgSym (C' Complement) info (a :* Nil)
        = AIn r' (Ut.App Ut.Complement t' [untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
     untypeProgSym (C' Bit) info (a :* Nil)
        = AIn r' (Ut.App Ut.Bit t' [untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
     untypeProgSym (C' SetBit) info (a :* b :* Nil)
        = AIn r' (Ut.App Ut.SetBit t' [untypeProg a, untypeProg b])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
     untypeProgSym (C' ClearBit) info (a :* b :* Nil)
        = AIn r' (Ut.App Ut.ClearBit t' [untypeProg a, untypeProg b])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
     untypeProgSym (C' ComplementBit) info (a :* b :* Nil)
        = AIn r' (Ut.App Ut.ComplementBit t' [untypeProg a, untypeProg b])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
     untypeProgSym (C' TestBit) info (a :* b :* Nil)
        = AIn r' (Ut.App Ut.TestBit t' [untypeProg a, untypeProg b])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
     untypeProgSym (C' ShiftLU) info (a :* b :* Nil)
        = AIn r' (Ut.App Ut.ShiftLU t' [untypeProg a, untypeProg b])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
     untypeProgSym (C' ShiftRU) info (a :* b :* Nil)
        = AIn r' (Ut.App Ut.ShiftRU t' [untypeProg a, untypeProg b])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
     untypeProgSym (C' ShiftL) info (a :* b :* Nil)
        = AIn r' (Ut.App Ut.ShiftL t' [untypeProg a, untypeProg b])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
     untypeProgSym (C' ShiftR) info (a :* b :* Nil)
        = AIn r' (Ut.App Ut.ShiftR t' [untypeProg a, untypeProg b])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
     untypeProgSym (C' RotateLU) info (a :* b :* Nil)
        = AIn r' (Ut.App Ut.RotateLU t' [untypeProg a, untypeProg b])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
     untypeProgSym (C' RotateRU) info (a :* b :* Nil)
        = AIn r' (Ut.App Ut.RotateRU t' [untypeProg a, untypeProg b])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
     untypeProgSym (C' RotateL) info (a :* b :* Nil)
        = AIn r' (Ut.App Ut.RotateL t' [untypeProg a, untypeProg b])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
     untypeProgSym (C' RotateR) info (a :* b :* Nil)
        = AIn r' (Ut.App Ut.RotateR t' [untypeProg a, untypeProg b])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
     untypeProgSym (C' ReverseBits) info (a :* Nil)
        = AIn r' (Ut.App Ut.ReverseBits t' [untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
     untypeProgSym (C' BitScan) info (a :* Nil)
        = AIn r' (Ut.App Ut.BitScan t' [untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
     untypeProgSym (C' BitCount) info (a :* Nil)
        = AIn r' (Ut.App Ut.BitCount t' [untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)

instance Untype dom dom => Untype (COMPLEX    :|| Type) dom
   where
      untypeProgSym (C' MkComplex) info (a :* b :* Nil)
        = AIn r' (Ut.App Ut.MkComplex t' [untypeProg a, untypeProg b])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
      untypeProgSym (C' RealPart) info (a :* Nil)
        = AIn r' (Ut.App Ut.RealPart t' [untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
      untypeProgSym (C' ImagPart) info (a :* Nil)
        = AIn r' (Ut.App Ut.ImagPart t' [untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
      untypeProgSym (C' MkPolar) info (a :* b :* Nil)
        = AIn r' (Ut.App Ut.MkPolar t' [untypeProg a, untypeProg b])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
      untypeProgSym (C' Conjugate) info (a :* Nil)
        = AIn r' (Ut.App Ut.Conjugate t' [untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
      untypeProgSym (C' Magnitude) info (a :* Nil)
        = AIn r' (Ut.App Ut.Magnitude t' [untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
      untypeProgSym (C' Phase) info (a :* Nil)
        = AIn r' (Ut.App Ut.Phase t' [untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
      untypeProgSym (C' Cis) info (a :* Nil)
        = AIn r' (Ut.App Ut.Cis t' [untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)

instance Untype dom dom => Untype (Conversion :|| Type) dom
  where
      untypeProgSym (C' F2I) info (a :* Nil)
        = AIn r' (Ut.App Ut.F2I t' [untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
      untypeProgSym (C' I2N) info (a :* Nil)
        = AIn r' (Ut.App Ut.I2N t' [untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
      untypeProgSym (C' B2I) info (a :* Nil)
        = AIn r' (Ut.App Ut.B2I t' [untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
      untypeProgSym (C' Round) info (a :* Nil)
        = AIn r' (Ut.App Ut.Round t' [untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
      untypeProgSym (C' Ceiling) info (a :* Nil)
        = AIn r' (Ut.App Ut.Ceiling t' [untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
      untypeProgSym (C' Floor) info (a :* Nil)
        = AIn r' (Ut.App Ut.Floor t' [untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)

instance Untype dom dom => Untype (EQ         :|| Type) dom
  where
      untypeProgSym (C' Equal) info (a :* b :* Nil)
        = AIn r' (Ut.App Ut.Equal t' [untypeProg a, untypeProg b])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
      untypeProgSym (C' NotEqual) info (a :* b :* Nil)
        = AIn r' (Ut.App Ut.NotEqual t' [untypeProg a, untypeProg b])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)

instance Untype dom dom => Untype (FLOATING   :|| Type) dom
  where
      untypeProgSym (C' Pi) info Nil
        = AIn r' (Ut.App Ut.Pi t' [])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
      untypeProgSym (C' Feldspar.Core.Constructs.Floating.Exp) info (a :* Nil)
        = AIn r' (Ut.App Ut.Exp t' [untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
      untypeProgSym (C' Sqrt) info (a :* Nil)
        = AIn r' (Ut.App Ut.Sqrt t' [untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
      untypeProgSym (C' Log) info (a :* Nil)
        = AIn r' (Ut.App Ut.Log t' [untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
      untypeProgSym (C' Pow) info (a :* b :* Nil)
        = AIn r' (Ut.App Ut.Pow t' [untypeProg a, untypeProg b])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
      untypeProgSym (C' LogBase) info (a :* b :* Nil)
        = AIn r' (Ut.App Ut.LogBase t' [untypeProg a, untypeProg b])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
      untypeProgSym (C' Sin) info (a :* Nil)
        = AIn r' (Ut.App Ut.Sin t' [untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
      untypeProgSym (C' Tan) info (a :* Nil)
        = AIn r' (Ut.App Ut.Tan t' [untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
      untypeProgSym (C' Cos) info (a :* Nil)
        = AIn r' (Ut.App Ut.Cos t' [untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
      untypeProgSym (C' Asin) info (a :* Nil)
        = AIn r' (Ut.App Ut.Asin t' [untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
      untypeProgSym (C' Atan) info (a :* Nil)
        = AIn r' (Ut.App Ut.Atan t' [untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
      untypeProgSym (C' Acos) info (a :* Nil)
        = AIn r' (Ut.App Ut.Acos t' [untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
      untypeProgSym (C' Sinh) info (a :* Nil)
        = AIn r' (Ut.App Ut.Sinh t' [untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
      untypeProgSym (C' Tanh) info (a :* Nil)
        = AIn r' (Ut.App Ut.Tanh t' [untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
      untypeProgSym (C' Cosh) info (a :* Nil)
        = AIn r' (Ut.App Ut.Cosh t' [untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
      untypeProgSym (C' Asinh) info (a :* Nil)
        = AIn r' (Ut.App Ut.Asinh t' [untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
      untypeProgSym (C' Atanh) info (a :* Nil)
        = AIn r' (Ut.App Ut.Atanh t' [untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
      untypeProgSym (C' Acosh) info (a :* Nil)
        = AIn r' (Ut.App Ut.Acosh t' [untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)

instance Untype dom dom => Untype (FRACTIONAL :|| Type) dom
  where
      untypeProgSym (C' DivFrac) info (a :* b :* Nil)
        = AIn r' (Ut.App Ut.DivFrac t' [untypeProg a, untypeProg b])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)

instance Untype dom dom => Untype (INTEGRAL   :|| Type) dom
  where
      untypeProgSym (C' Quot) info (a :* b :* Nil)
        = AIn r' (Ut.App Ut.Quot t' [untypeProg a, untypeProg b])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
      untypeProgSym (C' Rem) info (a :* b :* Nil)
        = AIn r' (Ut.App Ut.Rem t' [untypeProg a, untypeProg b])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
      untypeProgSym (C' Div) info (a :* b :* Nil)
        = AIn r' (Ut.App Ut.Div t' [untypeProg a, untypeProg b])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
      untypeProgSym (C' Mod) info (a :* b :* Nil)
        = AIn r' (Ut.App Ut.Mod t' [untypeProg a, untypeProg b])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
      untypeProgSym (C' Feldspar.Core.Constructs.Integral.Exp) info (a :* b :* Nil)
        = AIn r' (Ut.App Ut.IExp t' [untypeProg a, untypeProg b])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)

instance Untype dom dom => Untype (Logic      :|| Type) dom
  where
      untypeProgSym (C' And) info (a :* b :* Nil)
        = AIn r' (Ut.App Ut.And t' [untypeProg a, untypeProg b])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
      untypeProgSym (C' Or) info (a :* b :* Nil)
        = AIn r' (Ut.App Ut.Or t' [untypeProg a, untypeProg b])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
      untypeProgSym (C' Not) info (a :* Nil)
        = AIn r' (Ut.App Ut.Not t' [untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)

instance Untype dom dom => Untype (NUM        :|| Type) dom
  where
      untypeProgSym (C' Abs) info (a :* Nil)
        = AIn r' (Ut.App Ut.Abs t' [untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
      untypeProgSym (C' Sign) info (a :* Nil)
        = AIn r' (Ut.App Ut.Sign t' [untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
      untypeProgSym (C' Add) info (a :* b :* Nil)
        = AIn r' (Ut.App Ut.Add t' [untypeProg a, untypeProg b])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
      untypeProgSym (C' Sub) info (a :* b :* Nil)
        = AIn r' (Ut.App Ut.Sub t' [untypeProg a, untypeProg b])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
      untypeProgSym (C' Mul) info (a :* b :* Nil)
        = AIn r' (Ut.App Ut.Mul t' [untypeProg a, untypeProg b])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)

instance Untype dom dom => Untype (ORD        :|| Type) dom
  where
      untypeProgSym (C' LTH) info (a :* b :* Nil)
        = AIn r' (Ut.App Ut.LTH t' [untypeProg a, untypeProg b])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
      untypeProgSym (C' GTH) info (a :* b :* Nil)
        = AIn r' (Ut.App Ut.GTH t' [untypeProg a, untypeProg b])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
      untypeProgSym (C' LTE) info (a :* b :* Nil)
        = AIn r' (Ut.App Ut.LTE t' [untypeProg a, untypeProg b])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
      untypeProgSym (C' GTE) info (a :* b :* Nil)
        = AIn r' (Ut.App Ut.GTE t' [untypeProg a, untypeProg b])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
      untypeProgSym (C' Min) info (a :* b :* Nil)
        = AIn r' (Ut.App Ut.Min t' [untypeProg a, untypeProg b])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
      untypeProgSym (C' Max) info (a :* b :* Nil)
        = AIn r' (Ut.App Ut.Max t' [untypeProg a, untypeProg b])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)

instance Untype dom dom => Untype (REALFLOAT  :|| Type) dom
  where
   untypeProgSym (C' Atan2) info (a :* b :* Nil)
      = AIn r' (Ut.App Ut.Atan2 t' [untypeProg a, untypeProg b])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)

instance Untype dom dom => Untype (Save :|| Type) dom
  where
    untypeProgSym (C' Save) info (a :* Nil) = untypeProg a

instance Untype dom dom => Untype (PropSize :|| Type) dom
  where
    untypeProgSym (C' (PropSize _)) info (_ :* b :* Nil)
        = AIn r' (Ut.App Ut.PropSize t' [untypeProg b])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)

instance Untype dom dom => Untype (Decor SourceInfo1 Identity :|| Type) dom
  where
    untypeProgSym (C' (Decor (SourceInfo1 comment) Id)) info (a :* Nil)
        = AIn r' (Ut.App (Ut.SourceInfo comment) t' [untypeProg a])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)

instance ( Untype dom dom
         , Project (EQ :|| Type) dom
         , Project (Condition :|| Type) dom
         )
      => Untype (Switch :|| Type) dom
  where
    untypeProgSym (C' Switch) info (tree :* Nil)
        = AIn r' (Ut.App Ut.Switch t' [untypeProg tree])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)

instance Untype dom dom => Untype (Tuple :|| Type) dom
  where
    untypeProgSym _ info args = AIn r' $ Ut.App Ut.Tup t' $ listArgs untypeProg args
      where t' = untypeType (infoType info) (infoSize info)
            r' = toValueInfo (infoType info) (infoSize info)

instance Untype dom dom => Untype (Select :|| Type) dom
  where
    untypeProgSym (C' Sel1) info (tup :* Nil)
        = AIn r' (Ut.App Ut.Sel1 t' [untypeProg tup])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym (C' Sel2) info (tup :* Nil)
        = AIn r' (Ut.App Ut.Sel2 t' [untypeProg tup])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym (C' Sel3) info (tup :* Nil)
        = AIn r' (Ut.App Ut.Sel3 t' [untypeProg tup])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym (C' Sel4) info (tup :* Nil)
        = AIn r' (Ut.App Ut.Sel4 t' [untypeProg tup])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym (C' Sel5) info (tup :* Nil)
        = AIn r' (Ut.App Ut.Sel5 t' [untypeProg tup])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym (C' Sel6) info (tup :* Nil)
        = AIn r' (Ut.App Ut.Sel6 t' [untypeProg tup])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym (C' Sel7) info (tup :* Nil)
        = AIn r' (Ut.App Ut.Sel7 t' [untypeProg tup])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym (C' Sel8) info (tup :* Nil)
        = AIn r' (Ut.App Ut.Sel8 t' [untypeProg tup])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym (C' Sel9) info (tup :* Nil)
        = AIn r' (Ut.App Ut.Sel9 t' [untypeProg tup])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym (C' Sel10) info (tup :* Nil)
        = AIn r' (Ut.App Ut.Sel10 t' [untypeProg tup])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym (C' Sel11) info (tup :* Nil)
        = AIn r' (Ut.App Ut.Sel11 t' [untypeProg tup])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym (C' Sel12) info (tup :* Nil)
        = AIn r' (Ut.App Ut.Sel12 t' [untypeProg tup])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym (C' Sel13) info (tup :* Nil)
        = AIn r' (Ut.App Ut.Sel13 t' [untypeProg tup])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym (C' Sel14) info (tup :* Nil)
        = AIn r' (Ut.App Ut.Sel14 t' [untypeProg tup])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)
    untypeProgSym (C' Sel15) info (tup :* Nil)
        = AIn r' (Ut.App Ut.Sel15 t' [untypeProg tup])
          where t' = untypeType (infoType info) (infoSize info)
                r' = toValueInfo (infoType info) (infoSize info)

-- | Construct a ValueInfo from a TypeRep and a Size
toValueInfo :: TypeRep a -> Size a -> ValueInfo
toValueInfo UnitType          _             = VIProd []
-- FIXME: No range for boolean types yet.
toValueInfo BoolType          _             = VIBool    (Range 0 1)
toValueInfo (IntType U N8)    r             = VIWord8   r
toValueInfo (IntType S N8)    r             = VIInt8    r
toValueInfo (IntType U N16)   r             = VIWord16  r
toValueInfo (IntType S N16)   r             = VIInt16   r
toValueInfo (IntType U N32)   r             = VIWord32  r
toValueInfo (IntType S N32)   r             = VIInt32   r
toValueInfo (IntType U N64)   r             = VIWord64  r
toValueInfo (IntType S N64)   r             = VIInt64   r
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
toValueInfo (ArrayType a) ((Range (WordN l) (WordN r)) :> es)
  = VIProd [VIWord32 (Range l r), toValueInfo a es]
toValueInfo (MArrType a) ((Range (WordN l) (WordN r)) :> es)
  = VIProd [VIWord32 (Range l r), toValueInfo a es]
toValueInfo (ParType a) sz                  = toValueInfo a sz
toValueInfo (ElementsType a) ((Range (WordN l) (WordN r)) :> es)
  = VIProd [VIWord32 (Range l r), toValueInfo a es]
toValueInfo (IVarType a) sz                 = toValueInfo a sz
-- TODO: Maybe keep argument information for FunType.
toValueInfo (FunType a b) (sa, _)           = toValueInfo a sa
toValueInfo (FValType a) sz                 = toValueInfo a sz
toValueInfo typ _
  = error $ "toValueInfo: missing case for " ++ show typ

untypeType :: TypeRep a -> Size a -> Ut.Type
untypeType UnitType _               = Ut.TupType []
untypeType BoolType _               = Ut.BoolType
untypeType (IntType s n) _          = Ut.IntType (convSign s) (convSize n)
untypeType FloatType _              = Ut.FloatType
untypeType DoubleType _             = Ut.DoubleType
untypeType (ComplexType t) _        = Ut.ComplexType (untypeType t (defaultSize t))
untypeType (Tup2Type a b) (sa,sb)
  = Ut.TupType [untypeType a sa, untypeType b sb]
untypeType (Tup3Type a b c) (sa,sb,sc)
  = Ut.TupType [untypeType a sa, untypeType b sb, untypeType c sc]
untypeType (Tup4Type a b c d) (sa,sb,sc,sd)
  = Ut.TupType [ untypeType a sa, untypeType b sb, untypeType c sc
               , untypeType d sd
               ]
untypeType (Tup5Type a b c d e) (sa,sb,sc,sd,se)
  = Ut.TupType [ untypeType a sa, untypeType b sb, untypeType c sc
               , untypeType d sd, untypeType e se
               ]
untypeType (Tup6Type a b c d e f) (sa,sb,sc,sd,se,sf)
  = Ut.TupType [ untypeType a sa, untypeType b sb, untypeType c sc
               , untypeType d sd, untypeType e se, untypeType f sf
               ]
untypeType (Tup7Type a b c d e f g) (sa,sb,sc,sd,se,sf,sg)
  = Ut.TupType [ untypeType a sa, untypeType b sb, untypeType c sc
               , untypeType d sd, untypeType e se, untypeType f sf
               , untypeType g sg
               ]
untypeType (Tup8Type a b c d e f g h) (sa,sb,sc,sd,se,sf,sg,sh)
  = Ut.TupType [ untypeType a sa, untypeType b sb, untypeType c sc
               , untypeType d sd, untypeType e se, untypeType f sf
               , untypeType g sg, untypeType h sh
               ]
untypeType (Tup9Type a b c d e f g h i) (sa,sb,sc,sd,se,sf,sg,sh,si)
  = Ut.TupType [ untypeType a sa, untypeType b sb, untypeType c sc
               , untypeType d sd, untypeType e se, untypeType f sf
               , untypeType g sg, untypeType h sh, untypeType i si
               ]
untypeType (Tup10Type a b c d e f g h i j) (sa,sb,sc,sd,se,sf,sg,sh,si,sj)
  = Ut.TupType [ untypeType a sa, untypeType b sb, untypeType c sc
               , untypeType d sd, untypeType e se, untypeType f sf
               , untypeType g sg, untypeType h sh, untypeType i si
               , untypeType j sj
               ]
untypeType (Tup11Type a b c d e f g h i j k) (sa,sb,sc,sd,se,sf,sg,sh,si,sj,sk)
  = Ut.TupType [ untypeType a sa, untypeType b sb, untypeType c sc
               , untypeType d sd, untypeType e se, untypeType f sf
               , untypeType g sg, untypeType h sh, untypeType i si
               , untypeType j sj, untypeType k sk
               ]
untypeType (Tup12Type a b c d e f g h i j k l) (sa,sb,sc,sd,se,sf,sg,sh,si,sj,sk,sl)
  = Ut.TupType [ untypeType a sa, untypeType b sb, untypeType c sc
               , untypeType d sd, untypeType e se, untypeType f sf
               , untypeType g sg, untypeType h sh, untypeType i si
               , untypeType j sj, untypeType k sk, untypeType l sl
               ]
untypeType (Tup13Type a b c d e f g h i j k l m) (sa,sb,sc,sd,se,sf,sg,sh,si,sj,sk,sl,sm)
  = Ut.TupType [ untypeType a sa, untypeType b sb, untypeType c sc
               , untypeType d sd, untypeType e se, untypeType f sf
               , untypeType g sg, untypeType h sh, untypeType i si
               , untypeType j sj, untypeType k sk, untypeType l sl
               , untypeType m sm
               ]
untypeType (Tup14Type a b c d e f g h i j k l m n) (sa,sb,sc,sd,se,sf,sg,sh,si,sj,sk,sl,sm,sn)
  = Ut.TupType [ untypeType a sa, untypeType b sb, untypeType c sc
               , untypeType d sd, untypeType e se, untypeType f sf
               , untypeType g sg, untypeType h sh, untypeType i si
               , untypeType j sj, untypeType k sk, untypeType l sl
               , untypeType m sm, untypeType n sn
               ]
untypeType (Tup15Type a b c d e f g h i j k l m n o) (sa,sb,sc,sd,se,sf,sg,sh,si,sj,sk,sl,sm,sn,so)
  = Ut.TupType [ untypeType a sa, untypeType b sb, untypeType c sc
               , untypeType d sd, untypeType e se, untypeType f sf
               , untypeType g sg, untypeType h sh, untypeType i si
               , untypeType j sj, untypeType k sk, untypeType l sl
               , untypeType m sm, untypeType n sn, untypeType o so
               ]
untypeType (MutType a) sz           = Ut.MutType (untypeType a sz)
untypeType (RefType a) sz           = Ut.RefType (untypeType a sz)
untypeType (ArrayType a) (rs :> es) = Ut.ArrayType rs (untypeType a es)
untypeType (MArrType a) (rs :> es)  = Ut.MArrType rs (untypeType a es)
untypeType (ParType a) sz           = Ut.ParType (untypeType a sz)
untypeType (ElementsType a) (rs :> es) = Ut.ElementsType (untypeType a es)
untypeType (IVarType a) sz          = Ut.IVarType $ untypeType a sz
untypeType (FunType a b) (sa, sz)   = Ut.FunType (untypeType a sa) (untypeType b sz)
untypeType (FValType a) sz          = Ut.FValType (untypeType a sz)
untypeType typ _                    = error "untypeType: missing "


-- Helper functions.

literal :: TypeRep a -> Size a -> a -> Lit
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
literal t s a = error "Missing pattern: FromTyped.hs: literal"

literalConst :: TypeRep a -> Size a -> a -> Lit
literalConst UnitType        _  ()     = LTup []
literalConst BoolType        _  a      = LBool a
literalConst (IntType s n)   sz a      = LInt (convSign s) (convSize n) (toInteger a)
literalConst FloatType       _  a      = LFloat a
literalConst DoubleType      _  a      = LDouble a
literalConst (ArrayType t)   _  a      = LArray t' $ map (literalConst t (defaultSize t)) a
  where t' = untypeType t (defaultSize t)
literalConst (ComplexType t) _  (r:+i) = LComplex re ie
  where re = literalConst t (defaultSize t) r
        ie = literalConst t (defaultSize t) i

convSign :: Signedness a -> Ut.Signedness
convSign U       = Unsigned
convSign S       = Signed

convSize :: BitWidth a -> Ut.Size
convSize N8      = S8
convSize N16     = S16
convSize N32     = S32
convSize N64     = S64
convSize NNative = S32
