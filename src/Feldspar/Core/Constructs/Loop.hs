{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

{-# LANGUAGE UndecidableInstances #-}

module Feldspar.Core.Constructs.Loop
where

import Data.Typeable

import Control.Monad (forM_, when)

import Language.Syntactic
import Language.Syntactic.Constructs.Binding hiding (betaReduce)
import Language.Syntactic.Constructs.Binding.HigherOrder (ArgConstr(..))

import Feldspar.Range
import Feldspar.Core.Types
import Feldspar.Core.Interpretation
import Feldspar.Core.Constructs.Binding
import Feldspar.Core.Constructs.Literal

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

instance Monad m => Equality (LoopM m) where equal = equalDefault; exprHash = exprHashDefault
instance Monad m => Render   (LoopM m) where renderArgs = renderArgsDefault
instance Monad m => ToTree   (LoopM m)
instance Monad m => Eval     (LoopM m) where evaluate = evaluateDefault
instance Monad m => EvalBind (LoopM m) where evalBindSym = evalBindSymDefault
instance Sharable (LoopM m)
  -- Will not be shared anyway, because 'maybeWitnessSat' returns 'Nothing'

instance Equality Loop where equal = equalDefault; exprHash = exprHashDefault
instance Render   Loop where renderArgs = renderArgsDefault
instance ToTree   Loop
instance Eval     Loop where evaluate = evaluateDefault
instance EvalBind Loop where evalBindSym = evalBindSymDefault
instance Sharable Loop

instance (AlphaEq dom dom dom env, Monad m) =>
    AlphaEq (LoopM m) (LoopM m) dom env
  where
    alphaEqSym = alphaEqSymDefault

instance AlphaEq dom dom dom env => AlphaEq Loop Loop dom env
  where
    alphaEqSym = alphaEqSymDefault

instance SizeProp (LoopM m)
  where
    sizeProp While _ = AnySize
    sizeProp For   _ = AnySize

instance SizeProp (Loop :|| Type)
  where
    sizeProp (C' ForLoop)   (_ :* _ :* WrapFull step :* Nil) = infoSize step
    sizeProp (C' WhileLoop) (_ :* _ :* WrapFull step :* Nil) = infoSize step


instance ( MonadType m
         , LoopM m :<: dom
         , ArgConstr Lambda Type :<: dom
         , Optimize dom dom
         )
      => Optimize (LoopM m) dom
  where
    optimizeFeat for@For (len :* step :* Nil) = do
        len' <- optimizeM len
        let szI     = infoSize (getInfo len')
            ixRange = rangeByRange 0 (szI-1)
        step' <- optimizeFunction optimizeM (mkInfo ixRange) step
        case getInfo step' of
          Info{} -> constructFeat for (len' :* step' :* Nil)

    optimizeFeat a args = optimizeFeatDefault a args

    constructFeatUnOpt While args = constructFeatUnOptDefaultTyp voidTypeRep While args
    constructFeatUnOpt For   args = constructFeatUnOptDefaultTyp voidTypeRep For   args

instance ( (Literal  :|| Type) :<: dom
         , (Loop     :|| Type) :<: dom
         , (Variable :|| Type) :<: dom
         , ArgConstr Lambda Type :<: dom
         , OptimizeSuper dom
         )
      => Optimize (Loop :|| Type) dom
  where
    optimizeFeat sym@(C' ForLoop) (len :* initial :* step :* Nil) = do
        len'  <- optimizeM len
        init' <- optimizeM initial
        let szI     = infoSize (getInfo len')
            ixRange = Range 0 (upperBound szI-1)
        step' <- optimizeFunction
            (optimizeFunction optimizeM (mkInfoTy typeRep))
            -- (optimizeFunctionFix optimizeM (getInfo init'))
            -- TODO The above optimization is unsound, as shown by the following
            --      program:
            --
            --        drawAST $ fold max (value minBound) -:: tVec1 tWordN >-> id
            (mkInfo ixRange)
            step
        constructFeat sym (len' :* init' :* step' :* Nil)

    optimizeFeat sym@(C' WhileLoop) (initial :* cond :* body :* Nil) = do
        init' <- optimizeM initial
        body' <- optimizeFunction optimizeM (mkInfoTy typeRep) body
        -- body' <- optimizeFunctionFix optimizeM info body
        -- TODO See comment above

        let info  = getInfo init'
        let info' = info { infoSize = infoSize (getInfo body') }
        cond' <- optimizeFunction optimizeM info' cond
        constructFeat sym (init' :* cond' :* body' :* Nil)

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

    constructFeatOpt feat args = constructFeatUnOpt feat args

    constructFeatUnOpt x@(C' _) = constructFeatUnOptDefault x

