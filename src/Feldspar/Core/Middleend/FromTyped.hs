{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Feldspar.Core.Middleend.FromTyped
  ( untype
  , untypeType
  , untypeDecor
  , untypeUnOpt
  )
  where

import qualified Data.ByteString.Char8 as B
import Feldspar.Core.Middleend.FromTypeUtil
import Feldspar.Core.Interpretation (FeldOpts, Info(..))
import Feldspar.Core.Middleend.CreateTasks
import Feldspar.Core.Middleend.LetSinking
import Feldspar.Core.Middleend.OptimizeUntyped

#ifndef INCREMENTAL_CSE

import Data.Complex
import Data.Typeable (Typeable)

import Language.Syntactic
import Language.Syntactic.Constructs.Binding hiding (Variable, Let)
import qualified Language.Syntactic.Constructs.Binding as SynBind
import Language.Syntactic.Constructs.Binding.HigherOrder hiding (Let)

import Feldspar.Range (Range(..), upperBound)

import Feldspar.Core.Types
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

-- | External module interface. Optimized code ready for fromCore.
untype :: Untype dom dom => FeldOpts -> ASTF (Decor Info dom) a ->  UntypedFeld
untype opts = createTasks opts . unAnnotate . optimize . sinkLets opts . untypeProg

-- | External module interface. Unoptimized code with value info.
untypeDecor :: Untype dom dom => FeldOpts -> ASTF (Decor Info dom) a -> AUntypedFeld ValueInfo
untypeDecor opts = sinkLets opts . untypeProg

-- | External module interface. Unoptimized code ready for fromCore.
untypeUnOpt :: Untype dom dom => FeldOpts -> ASTF (Decor Info dom) a ->  UntypedFeld
untypeUnOpt opts = createTasks opts . unAnnotate . sinkLets opts . untypeProg

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
        = AIn r' (Ut.Variable (Ut.Var v t' B.empty))
           where t' = untypeType (infoType info) (infoSize info)
                 r' = toValueInfo (infoType info) (infoSize info)

instance (Untype dom dom,Project (CLambda Type) dom) => Untype (CLambda Type) dom
  where
    untypeProgSym (SubConstr2 (Lambda v)) info (body :* Nil)
     = AIn r' (Ut.Lambda (Ut.Var v t' B.empty) body')
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

#else

import Feldspar.Core.Reify (ASTF(..), unASTF)
import Feldspar.Core.Types (TypeRep(..), typeRep, defaultSize, TypeF(..))
import qualified Feldspar.Core.Types as T
import Feldspar.Core.UntypedRepresentation as U
import Feldspar.ValueInfo
import Feldspar.Core.Middleend.PushLets
import Feldspar.Core.Middleend.UniqueVars
import qualified Feldspar.Core.Representation as R
import Feldspar.Core.Representation (AExpr((:&)), Expr((:@)))
import Control.Monad.State
import Data.Typeable (Typeable)

-- | External module interface. Untype, optimize and unannotate.
untype :: TypeF a => FeldOpts -> ASTF dom a -> UntypedFeld
untype opts = cleanUp opts
            . pushLets
            . optimize
            . sinkLets opts
            . justUntype opts

-- | External module interface.
untypeDecor :: TypeF a => FeldOpts -> ASTF dom a -> AUntypedFeld ValueInfo
untypeDecor opts = pushLets
                 . optimize
                 . sinkLets opts
                 . justUntype opts

-- | External module interface.
untypeUnOpt :: TypeF a => FeldOpts -> ASTF dom a -> UntypedFeld
untypeUnOpt opts = cleanUp opts
                 . justUntype opts

-- | Only do the conversion to AUntypedFeld ValueInfo
justUntype :: TypeF a => FeldOpts -> ASTF dom a -> AUntypedFeld ValueInfo
justUntype opts = renameExp . toU . unASTF opts

-- | Prepare the code for fromCore
cleanUp :: FeldOpts -> AUntypedFeld ValueInfo -> UntypedFeld
cleanUp opts = createTasks opts . unAnnotate . uniqueVars

renameExp :: AUntypedFeld a -> AUntypedFeld a
renameExp e = evalState (rename e) 0

toAnno :: TypeF a => R.Info a -> ValueInfo
toAnno = topInfo . toType . asInfo

asInfo :: TypeF a => R.Info a -> TypeRep a
asInfo _ = typeRepF

asVar :: TypeF a => R.Var a -> TypeRep a
asVar _ = typeRepF

asExpr :: TypeF a => R.Expr a -> TypeRep a
asExpr _ = typeRepF

asValue :: TypeF a => a -> TypeRep a
asValue _ = typeRepF

asOpT :: TypeF a => R.Op a -> TypeRep a
asOpT _ = typeRepF

toType :: TypeRep a -> Type
toType tr = untypeType tr (defaultSize tr)

toU :: TypeF a => R.AExpr a -> AUntypedFeld ValueInfo
toU (i :& e) = AIn (toAnno i) (toUr e)

toUr :: TypeF a => R.Expr a -> UntypedFeldF (AUntypedFeld ValueInfo)
toUr (R.Variable v) = Variable $ trV v
toUr (R.Literal v) = Literal $ literal tr (defaultSize tr) v
  where tr = asValue v
toUr e@(R.Operator op) = App (trOp op) (toType $ asExpr e) []
toUr (f :@ a) = toApp f [toU a]
toUr (R.Lambda v e) = Lambda (trV v) (toU e)

trV :: TypeF a => R.Var a -> Var
trV v =  Var {varNum = R.varNum v, varType = toType $ asVar v, varName = R.varName v}

toApp :: TypeF a => R.Expr a -> [AUntypedFeld ValueInfo] -> UntypedFeldF (AUntypedFeld ValueInfo)
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

#endif
