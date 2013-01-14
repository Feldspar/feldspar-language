{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
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

module Feldspar.Core.Constructs.Mutable
    ( module Feldspar.Core.Constructs.Mutable
    , module Language.Syntactic.Constructs.Monad
    )
where

import Data.Map
import Data.Typeable
import System.IO.Unsafe

import Language.Syntactic
import Language.Syntactic.Constructs.Binding
import Language.Syntactic.Constructs.Binding.HigherOrder
import Language.Syntactic.Constructs.Monad

import Feldspar.Core.Types
import Feldspar.Core.Interpretation
import Feldspar.Core.Constructs.Binding

data Mutable a
  where
    Run :: Type a => Mutable (Mut a :-> Full a)

instance Semantic Mutable
  where
    semantics Run = Sem "runMutable" unsafePerformIO

instance Equality Mutable where equal = equalDefault; exprHash = exprHashDefault
instance Render   Mutable where renderArgs = renderArgsDefault
instance ToTree   Mutable
instance Eval     Mutable where evaluate = evaluateDefault
instance EvalBind Mutable where evalBindSym = evalBindSymDefault
instance Sharable Mutable

instance Typed Mutable
  where
    typeDictSym Run = Just Dict

instance AlphaEq dom dom dom env => AlphaEq Mutable Mutable dom env
  where
    alphaEqSym = alphaEqSymDefault

instance Sharable (MONAD Mut)

instance SizeProp (MONAD Mut)
  where
    sizeProp Return (WrapFull a :* Nil)      = infoSize a
    sizeProp Bind   (_ :* WrapFull f :* Nil) = snd $ infoSize f
    sizeProp Then   (_ :* WrapFull b :* Nil) = infoSize b
    sizeProp When   _                        = AnySize

instance SizeProp Mutable
  where
    sizeProp Run (WrapFull a :* Nil) = infoSize a

monadProxy :: P Mut
monadProxy = P

instance ( MONAD Mut :<: dom
         , (Variable :|| Type) :<: dom
         , CLambda Type :<: dom
         , OptimizeSuper dom)
      => Optimize (MONAD Mut) dom
  where
    optimizeFeat bnd@Bind (ma :* f :* Nil) = do
        ma' <- optimizeM ma
        case getInfo ma' of
          Info (MutType ty) sz vs src -> do
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

       -- (bind e1 (\x -> e2) >> e3 ==> bind e1 (\x -> e2 >> e3)
    constructFeatOpt Then ((Sym (Decor _ bnd) :$ x :$ (lam :$ bd)) :* y :* Nil)
        | Just Bind <- prjMonad monadProxy bnd
        , Just lam'@(SubConstr2 (Lambda v1)) <- prjLambda lam
        = do
             bb <- constructFeat Then (bd :* y :* Nil)
             bd' <- constructFeat (reuseCLambda lam') (bb :* Nil)
             constructFeatUnOpt Bind (x :* bd' :* Nil)

      -- (bind (bind e1 (\x -> e2)) (\y -> e3) => bind e1 (\x -> bind e2 (\y-> e3))
    constructFeatOpt Bind ((Sym (Decor _ bnd) :$ x :$ (lam :$ bd)) :* y :* Nil)
        | Just Bind <- prjMonad monadProxy bnd
        , Just lam'@(SubConstr2 (Lambda v1)) <- prjLambda lam
        = do
             bb <- constructFeat Bind (bd :* y :* Nil)
             bd' <- constructFeat (reuseCLambda lam') (bb :* Nil)
             constructFeatUnOpt Bind (x :* bd' :* Nil)

      -- return x >> mb ==> mb
    constructFeatOpt Then ((Sym (Decor _ ret) :$ _) :* mb :* Nil)
        | Just Return <- prjMonad monadProxy ret
        = return mb

      -- ma >> return () ==> ma
    constructFeatOpt Then (ma :* (Sym (Decor info ret) :$ u) :* Nil)
        | Just Return <- prjMonad monadProxy ret
        , Just TypeEq <- typeEq (infoType $ getInfo ma) (MutType UnitType)
        , Just TypeEq <- typeEq (infoType info)         (MutType UnitType)
        , Just ()     <- viewLiteral u
        = return ma

    constructFeatOpt a args = constructFeatUnOpt a args

    constructFeatUnOpt Return args@(a :* Nil)
        | Info {infoType = t} <- getInfo a
        = constructFeatUnOptDefaultTyp (MutType t) Return args

    constructFeatUnOpt Bind args@(_ :* f@(lam :$ body) :* Nil)
        | Just (SubConstr2 (Lambda _))  <- prjLambda lam
        , Info {infoType = t} <- getInfo body
        = constructFeatUnOptDefaultTyp t Bind args

    constructFeatUnOpt Then args@(_ :* mb :* Nil)
        | Info {infoType = t} <- getInfo mb
        = constructFeatUnOptDefaultTyp t Then args

    constructFeatUnOpt When args =
        constructFeatUnOptDefaultTyp voidTypeRep When args

instance (Mutable :<: dom, MONAD Mut :<: dom, OptimizeSuper dom) => Optimize Mutable dom
  where
    constructFeatUnOpt Run ((ret :$ a) :* Nil)
        | Just Return <- prjMonad monadProxy ret = return a
    constructFeatUnOpt Run args = constructFeatUnOptDefault Run args

