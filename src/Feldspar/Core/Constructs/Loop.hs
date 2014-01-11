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

module Feldspar.Core.Constructs.Loop
where

import Data.Map (notMember)

import Control.Monad (forM_, when)

import Language.Syntactic
import Language.Syntactic.Constructs.Binding hiding (betaReduce)
import Language.Syntactic.Constructs.Binding.HigherOrder (CLambda)

import Feldspar.Range
import Feldspar.Core.Types
import Feldspar.Core.Interpretation
import Feldspar.Core.Constructs.Binding
import Feldspar.Core.Constructs.Literal
import Feldspar.Core.Constructs.Mutable
import Feldspar.Core.Constructs.MutableReference


data LoopM m a
  where
    While :: (Size (m ()) ~ AnySize) => LoopM m (m Bool :-> m a :-> Full (m ()))
    For   :: (Size (m ()) ~ AnySize) => LoopM m (Length :-> (Index -> m a) :-> Full (m ()))

data Loop a
  where
    ForLoop   :: Type a => Loop (Length :-> a :-> (Index -> a -> a) :-> Full a)
    WhileLoop :: Type a => Loop (a :-> (a -> Bool) :-> (a -> a) :-> Full a)

instance Monad m => Semantic (LoopM m)
  where
    semantics While = Sem "while" while
      where
        while cond body = do
                            c <- cond
                            when c (body >> while cond body)
    semantics For = Sem "for" for
      where
        for 0 _    = return ()
        for l body = forM_ [0..l-1] body

instance Semantic Loop
  where
    semantics ForLoop = Sem "forLoop" forLoop
      where
        forLoop 0 initial _    = initial
        forLoop l initial body = foldl (flip body) initial [0..l-1]
    semantics WhileLoop = Sem "whileLoop" whileLoop
      where
        whileLoop initial cond body = go initial
          where
            go st | cond st   = go $ body st
                  | otherwise = st

instance Monad m => Equality   (LoopM m) where equal = equalDefault; exprHash = exprHashDefault
instance Monad m => Render     (LoopM m) where renderSym  = renderSymDefault
                                               renderArgs = renderArgsDefault
instance Monad m => StringTree (LoopM m)
instance Monad m => Eval       (LoopM m) where evaluate = evaluateDefault
instance Monad m => EvalBind   (LoopM m) where evalBindSym = evalBindSymDefault
instance            Sharable   (LoopM m)

semanticInstances ''Loop

instance EvalBind Loop where evalBindSym = evalBindSymDefault

instance (AlphaEq dom dom dom env, Monad m) =>
    AlphaEq (LoopM m) (LoopM m) dom env
  where
    alphaEqSym = alphaEqSymDefault

instance AlphaEq dom dom dom env => AlphaEq Loop Loop dom env
  where
    alphaEqSym = alphaEqSymDefault

instance Sharable Loop

instance SizeProp (LoopM m)
  where
    sizeProp While _ = AnySize
    sizeProp For   _ = AnySize

instance SizeProp (Loop :|| Type)
  where
    sizeProp (C' ForLoop)   (_ :* _ :* WrapFull step :* Nil) = snd $ snd $ infoSize step
    sizeProp (C' WhileLoop) (_ :* _ :* WrapFull step :* Nil) = snd $ infoSize step


instance ( LoopM Mut :<: dom
         , (Variable :|| Type) :<: dom
         , CLambda Type :<: dom
         , MONAD Mut :<: dom
         , MutableReference :<: dom
         , Let :<: dom
         , Optimize (CLambda Type) dom
         , Optimize (MONAD Mut) dom
         , Optimize dom dom
         )
      => Optimize (LoopM Mut) dom
  where
    optimizeFeat opts for@For (len :* step :* Nil) = do
        len' <- optimizeM opts len
        let szI     = infoSize (getInfo len')
            ixRange = rangeByRange 0 (rangeSubSat szI 1)
        step' <- optimizeFunction opts (optimizeM opts) (mkInfo ixRange) step
        case getInfo step' of
          Info{} -> constructFeat opts for (len' :* step' :* Nil)

    optimizeFeat opts a args = optimizeFeatDefault opts a args

    constructFeatOpt opts For (len :* (lam1 :$ (bnd :$ getRefV2@(grf :$ ref) :$ bd@(lam3 :$ body))) :* Nil)
      | Just (SubConstr2 (Lambda v1)) <- prjLambda lam1
      , Just lam3'@(SubConstr2 (Lambda v3)) <- prjLambda lam3
      , Just Bind <- prjMonad monadProxy bnd
      , Just GetRef <- prj grf
      , Just (C' (Variable v2)) <- prjF ref
      , v1 /= v2
      , v2 `notMember` fvars
      = do
          loop      <- constructFeat opts For (len :* (lam1 :$ body) :* Nil)
          hoistedV3 <- constructFeat opts (reuseCLambda lam3') (loop :* Nil)
          -- Do not optimize right now; the v3 binding gets lost.
          constructFeatUnOpt opts Bind (getRefV2 :* hoistedV3 :* Nil)
     where
      fvars = infoVars $ getInfo bd

    constructFeatOpt opts feat args = constructFeatUnOpt opts feat args

    constructFeatUnOpt opts While args = constructFeatUnOptDefaultTyp opts voidTypeRep While args
    constructFeatUnOpt opts For   args = constructFeatUnOptDefaultTyp opts voidTypeRep For   args

instance ( (Literal  :|| Type) :<: dom
         , (Loop     :|| Type) :<: dom
         , (Variable :|| Type) :<: dom
         , CLambda Type :<: dom
         , Let :<: dom
         , OptimizeSuper dom
         )
      => Optimize (Loop :|| Type) dom
  where
    optimizeFeat opts sym@(C' ForLoop) (len :* initial :* step :* Nil) = do
        len'  <- optimizeM opts len
        init' <- optimizeM opts initial
        let szI     = infoSize (getInfo len')
            ixRange = Range 0 (upperBound szI-1)
        step' <- optimizeFunction opts
            (optimizeFunction opts (optimizeM opts) (mkInfoTy typeRep))
            -- (optimizeFunctionFix optimizeM (getInfo init'))
            -- TODO The above optimization is unsound, as shown by the following
            --      program:
            --
            --        drawAST $ fold max (value minBound) -:: tVec1 tWordN >-> id
            (mkInfo ixRange)
            step
        constructFeat opts sym (len' :* init' :* step' :* Nil)

    optimizeFeat opts sym@(C' WhileLoop) (initial :* cond :* body :* Nil) = do
        init' <- optimizeM opts initial
        body' <- optimizeFunction opts (optimizeM opts) (mkInfoTy typeRep) body
        -- body' <- optimizeFunctionFix optimizeM info body
        -- TODO See comment above

        let info  = getInfo init'
        let info' = info { infoSize = snd $ infoSize (getInfo body') }
        cond' <- optimizeFunction opts (optimizeM opts) info' cond
        constructFeat opts sym (init' :* cond' :* body' :* Nil)

{-
    constructFeatOpt (C' ForLoop) (len :* initial :* step :* Nil)
        | Just 0 <- viewLiteral len = return initial
        | Just 1 <- viewLiteral len = do
          let init' = stripDecor initial
              step' = stripDecor step
          optimizeM $ betaReduce init' $ betaReduce (appSym (c' (Literal 0))) step'
        -- TODO add an optional unroll limit?

      -- ForLoop len init (const id) ==> init
    constructFeatOpt (C' ForLoop) (_ :* initial :* step :* Nil)
        | alphaEq step' (fun `asTypeOf` step') = optimizeM $ stripDecor initial
      where
        step' = stripDecor step
        fun = appSym (Lambda 0) $ appSym (Lambda 1) $ appSym (Variable 1)
-}

      -- TODO ForLoop len init (flip (const f)) ==> step (len - 1) init
      -- This optimization requires that the len > 0

    constructFeatOpt opts feat args = constructFeatUnOpt opts feat args

    constructFeatUnOpt opts x@(C' _) = constructFeatUnOptDefault opts x

