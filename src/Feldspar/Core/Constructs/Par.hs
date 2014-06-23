{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
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

semanticInstances ''ParFeature

instance EvalBind ParFeature where evalBindSym = evalBindSymDefault

instance AlphaEq dom dom dom env => AlphaEq ParFeature ParFeature dom env
  where
    alphaEqSym = alphaEqSymDefault

instance Sharable ParFeature

instance Sharable (MONAD Par)

instance Creases ParFeature

instance Creases (MONAD Par)

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
    constructFeatUnOpt opts ParRun args   = constructFeatUnOptDefault opts ParRun args
    constructFeatUnOpt opts ParNew args   = constructFeatUnOptDefaultTyp opts (ParType $ IVarType typeRep) ParNew args
    constructFeatUnOpt opts ParGet args   = constructFeatUnOptDefaultTyp opts (ParType typeRep) ParGet args
    constructFeatUnOpt opts ParPut args   = constructFeatUnOptDefaultTyp opts (ParType typeRep) ParPut args
    constructFeatUnOpt opts ParFork args  = constructFeatUnOptDefaultTyp opts (ParType typeRep) ParFork args
    constructFeatUnOpt opts ParYield args = constructFeatUnOptDefaultTyp opts (ParType typeRep) ParYield args

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
         , Let :<: dom
         , OptimizeSuper dom
         )
      => Optimize (MONAD Par) dom
  where
    optimizeFeat opts bnd@Bind (ma :* f :* Nil) = do
        ma' <- optimizeM opts ma
        case getInfo ma' of
          Info (ParType ty) sz vs src -> do
            f' <- optimizeFunction opts (optimizeM opts) (Info ty sz vs src) f
            case getInfo f' of
              Info{} -> constructFeat opts bnd (ma' :* f' :* Nil)

    optimizeFeat opts a args = optimizeFeatDefault opts a args

    constructFeatOpt _ Bind (ma :* (lam :$ (ret :$ var)) :* Nil)
      | Just (SubConstr2 (Lambda v1)) <- prjLambda lam
      , Just Return                   <- prjMonad monadProxy ret
      , Just (C' (Variable v2))       <- prjF var
      , v1 == v2
      , Just ma' <- gcast ma
      = return ma'

    constructFeatOpt opts Bind (ma :* (lam :$ body) :* Nil)
        | Just (SubConstr2 (Lambda v)) <- prjLambda lam
        , v `notMember` vars
        = constructFeat opts Then (ma :* body :* Nil)
      where
        vars = infoVars $ getInfo body

      -- return x >> mb ==> mb
    constructFeatOpt _ Then ((ret :$ _) :* mb :* Nil)
        | Just Return <- prjMonad monadProxy ret
        = return mb

      -- ma >> return () ==> ma
    constructFeatOpt _ Then (ma :* (ret :$ u) :* Nil)
        | Just Return <- prjMonad monadProxy ret
        , Just TypeEq <- typeEq (infoType $ getInfo ma)  (ParType UnitType)
        , Just TypeEq <- typeEq (infoType $ getInfo ret) (ParType UnitType)
        , Just ()     <- viewLiteral u
        = return ma

    constructFeatOpt opts a args = constructFeatUnOpt opts a args

    constructFeatUnOpt opts Return args@(a :* Nil)
        | Info {infoType = t} <- getInfo a
        = constructFeatUnOptDefaultTyp opts (ParType t) Return args

    constructFeatUnOpt opts Bind args@(_ :* (lam :$ body) :* Nil)
        | Just (SubConstr2 (Lambda _))  <- prjLambda lam
        , Info {infoType = t} <- getInfo body
        = constructFeatUnOptDefaultTyp opts t Bind args

    constructFeatUnOpt opts Then args@(_ :* mb :* Nil)
        | Info {infoType = t} <- getInfo mb
        = constructFeatUnOptDefaultTyp opts t Then args

    constructFeatUnOpt opts When args =
        constructFeatUnOptDefaultTyp opts voidTypeRep When args

