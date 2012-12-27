{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

--
-- Copyright (c) 2009-2011, ERICSSON AB
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

module Feldspar.Core.Constructs.Par where

import Language.Syntactic
import Language.Syntactic.Constructs.Monad
import Language.Syntactic.Constructs.Binding.HigherOrder

import qualified Control.Monad.Par as CMP
import Control.Monad.Par.Scheds.TraceInternal (yield)

import Feldspar.Lattice
import Feldspar.Core.Types
import Feldspar.Core.Interpretation
import Feldspar.Core.Constructs.Binding

import Data.Map (notMember)
import Data.Typeable (gcast)

data ParFeature a
  where
    ParRun    :: Type a => ParFeature (Par a :-> Full a)
    ParNew    :: Type a => ParFeature (Full (Par (IV a)))
    ParGet    :: Type a => ParFeature (IV a :-> Full (Par a))
    ParPut    :: Type a => ParFeature (IV a :-> a :-> Full (Par ()))
    ParFork   ::           ParFeature (Par () :-> Full (Par ()))
    ParYield  ::           ParFeature (Full (Par ()))

instance Semantic ParFeature
  where
    semantics ParRun    = Sem "runPar" CMP.runPar
    semantics ParNew    = Sem "new" CMP.new
    semantics ParGet    = Sem "get" CMP.get
    semantics ParPut    = Sem "put" CMP.put_
    semantics ParFork   = Sem "fork" CMP.fork
    semantics ParYield  = Sem "yield" yield

instance Equality ParFeature where equal = equalDefault; exprHash = exprHashDefault
instance Render   ParFeature where renderArgs = renderArgsDefault
instance ToTree   ParFeature
instance Eval     ParFeature where evaluate = evaluateDefault
instance EvalBind ParFeature where evalBindSym = evalBindSymDefault
instance Sharable ParFeature

instance AlphaEq dom dom dom env => AlphaEq ParFeature ParFeature dom env
  where
    alphaEqSym = alphaEqSymDefault

instance Sharable (MONAD Par)

instance SizeProp ParFeature
  where
    sizeProp ParRun   (WrapFull a :* Nil) = infoSize a
    sizeProp ParNew   _                   = universal
    sizeProp ParGet   _                   = universal
    sizeProp ParPut   _                   = universal
    sizeProp ParFork  _                   = universal
    sizeProp ParYield _                   = universal

instance ( MONAD Par :<: dom
         , ParFeature :<: dom
         , Optimize dom dom
         )
      => Optimize ParFeature dom
  where
    constructFeatUnOpt ParRun args   = constructFeatUnOptDefault ParRun args
    constructFeatUnOpt ParNew args   = constructFeatUnOptDefaultTyp (ParType $ IVarType typeRep) ParNew args
    constructFeatUnOpt ParGet args   = constructFeatUnOptDefaultTyp (ParType typeRep) ParGet args
    constructFeatUnOpt ParPut args   = constructFeatUnOptDefaultTyp (ParType typeRep) ParPut args
    constructFeatUnOpt ParFork args  = constructFeatUnOptDefaultTyp (ParType typeRep) ParFork args
    constructFeatUnOpt ParYield args = constructFeatUnOptDefaultTyp (ParType typeRep) ParYield args

monadProxy :: P Par
monadProxy = P

instance SizeProp (MONAD Par)
  where
    sizeProp Return (WrapFull a :* Nil)      = infoSize a
    sizeProp Bind   (_ :* WrapFull f :* Nil) = snd $ infoSize f
    sizeProp Then   (_ :* WrapFull b :* Nil) = infoSize b
    sizeProp When   _                        = AnySize

instance ( MONAD Par :<: dom
         , (Variable :|| Type) :<: dom
         , CLambda Type :<: dom
         , OptimizeSuper dom
         )
      => Optimize (MONAD Par) dom
  where
    optimizeFeat bnd@Bind (ma :* f :* Nil) = do
        ma' <- optimizeM ma
        case getInfo ma' of
          Info (ParType ty) sz vs src -> do
            f' <- optimizeFunction optimizeM (Info ty sz vs src) f
            case getInfo f' of
              Info{} -> constructFeat bnd (ma' :* f' :* Nil)

    optimizeFeat a args = optimizeFeatDefault a args

    constructFeatOpt Bind (ma :* (lam :$ (Sym (Decor _ ret) :$ var)) :* Nil)
      | Just (SubConstr2 (Lambda v1)) <- prjLambda lam
      , Just Return                   <- prjMonad monadProxy ret
      , Just (C' (Variable v2))       <- prjF var
      , v1 == v2
      , Just ma' <- gcast ma
      = return ma'

    constructFeatOpt Bind (ma :* (lam :$ body) :* Nil)
        | Just (SubConstr2 (Lambda v)) <- prjLambda lam
        , v `notMember` vars
        = constructFeat Then (ma :* body :* Nil)
      where
        vars = infoVars $ getInfo body

      -- return x >> mb ==> mb
    constructFeatOpt Then ((Sym (Decor _ ret) :$ _) :* mb :* Nil)
        | Just Return <- prjMonad monadProxy ret
        = return mb

      -- ma >> return () ==> ma
    constructFeatOpt Then (ma :* (Sym (Decor info ret) :$ u) :* Nil)
        | Just Return <- prjMonad monadProxy ret
        , Just TypeEq <- typeEq (infoType $ getInfo ma) (ParType UnitType)
        , Just TypeEq <- typeEq (infoType info)         (ParType UnitType)
        , Just ()     <- viewLiteral u
        = return ma

    constructFeatOpt a args = constructFeatUnOpt a args

    constructFeatUnOpt Return args@(a :* Nil)
        | Info {infoType = t} <- getInfo a
        = constructFeatUnOptDefaultTyp (ParType t) Return args
{-
    constructFeatUnOpt Bind args@(_ :* f :* Nil)
        | Info {infoType = FunType _ t} <- getInfo f
        = constructFeatUnOptDefaultTyp t Bind args
-}
      -- TODO The match on `FunType` is total with the current definition of
      --      `TypeRep`, but there's no guarantee this will remain true in the
      --      future. One way around that would be to match `f` against
      --      `Lambda`, but that is also a partial match (at least possibly, in
      --      the future). Another option would be to add a context parameter to
      --      `MONAD` to be able to add the constraint `Type a`.

    constructFeatUnOpt Then args@(_ :* mb :* Nil)
        | Info {infoType = t} <- getInfo mb
        = constructFeatUnOptDefaultTyp t Then args

    constructFeatUnOpt When args =
        constructFeatUnOptDefaultTyp voidTypeRep When args

