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

module Feldspar.Core.Constructs.Array
where

import Data.List
import Data.Map (notMember)

import Language.Syntactic
import Language.Syntactic.Constructs.Binding.HigherOrder (CLambda)

import Feldspar.Range
import Feldspar.Lattice
import Feldspar.Core.Types
import Feldspar.Core.Interpretation
import Feldspar.Core.Constructs.Binding
import Feldspar.Core.Constructs.Complex
import Feldspar.Core.Constructs.Integral
import Feldspar.Core.Constructs.Num
import Feldspar.Core.Constructs.Ord

data Array a
  where
    Parallel   :: Type a => Array (Length :-> (Index -> a) :-> Full [a])
    Sequential :: (Type a, Type st) =>
                  Array (Length :-> st :-> (Index -> st -> (a,st)) :-> Full [a])
    Append     :: Type a => Array ([a] :-> [a] :-> Full [a])
    GetIx      :: Type a => Array ([a] :-> Index :-> Full a)
    SetIx      :: Type a => Array ([a] :-> Index :-> a :-> Full [a])
    GetLength  :: Type a => Array ([a] :-> Full Length)
    SetLength  :: Type a => Array (Length :-> [a] :-> Full [a])

instance Semantic Array
  where
    semantics Append    = Sem "(++)"      (++)
    semantics GetIx     = Sem "(!)"       genericIndex
    semantics GetLength = Sem "getLength" genericLength
    semantics SetLength = Sem "setLength"
        (\n as -> genericTake n (as ++ repeat err))
      where
        err = error "reading uninitialized array element"

    semantics Parallel = Sem "parallel"
        (\len ixf -> genericTake len $ map ixf [0..])

    semantics Sequential = Sem "sequential"
        (\len i step -> genericTake len $
                        snd $ mapAccumL (\a ix -> swap (step ix a)) i [0..])
      where swap (a,b) = (b,a)

    semantics SetIx = Sem "setIx" evalSetIx
      where
        evalSetIx as i v
            | i < len   = genericTake i as ++ [v] ++ genericDrop (i+1) as
            | otherwise = error $ unwords
                [ "setIx: assigning index"
                , show i
                , "past the end of an array of length"
                , show len
                ]
          where
            len = genericLength as

semanticInstances ''Array

instance EvalBind Array where evalBindSym = evalBindSymDefault

instance AlphaEq dom dom dom env => AlphaEq Array Array dom env
  where
    alphaEqSym = alphaEqSymDefault

instance Sharable Array

instance Monotonic Array

instance SizeProp (Array :|| Type)
  where
    sizeProp (C' Parallel) (WrapFull len :* WrapFull ixf :* Nil) =
        infoSize len :> snd (infoSize ixf)
    sizeProp (C' Sequential) (WrapFull len :* _ :* WrapFull step :* Nil) =
        infoSize len :> fst (snd $ snd $ infoSize step)
    sizeProp (C' Append) (WrapFull arra :* WrapFull arrb :* Nil) =
        (alen + blen) :> (aelem \/ belem)
      where
        alen :> aelem = infoSize arra
        blen :> belem = infoSize arrb
    sizeProp (C' GetIx) (WrapFull arr :* _ :* Nil) = el
      where
        _ :> el = infoSize arr
    sizeProp (C' SetIx) (WrapFull arr :* _ :* WrapFull e :* Nil) =
        len :> (el \/ infoSize e)
      where
        len :> el = infoSize arr
    sizeProp (C' GetLength) (WrapFull arr :* Nil) = len
      where
        len :> _ = infoSize arr
    sizeProp (C' SetLength) (WrapFull len :* WrapFull arr :* Nil) =
        infoSize len :> el
      where
        _ :> el = infoSize arr

instance
    ( (Array    :|| Type) :<: dom
    , (NUM      :|| Type) :<: dom
    , Let                 :<: dom
    , (ORD      :|| Type) :<: dom
    , (INTEGRAL :|| Type) :<: dom
    , (COMPLEX  :|| Type) :<: dom
    , (Variable :|| Type) :<: dom
    , CLambda Type :<: dom
    , OptimizeSuper dom
    ) =>
      Optimize (Array :|| Type) dom
  where
    optimizeFeat opts sym@(C' Parallel) (len :* ixf :* Nil) = do
        len' <- optimizeM opts len
        let szI     = infoSize (getInfo len')
            ixRange = rangeByRange 0 (rangeSubSat szI 1)
        ixf' <- optimizeFunction opts (optimizeM opts) (mkInfo ixRange) ixf
        constructFeat opts sym (len' :* ixf' :* Nil)

    optimizeFeat opts sym@(C' Sequential) (len :* inital :* step :* Nil) = do
        len'  <- optimizeM opts len
        init' <- optimizeM opts inital
        let szI     = infoSize (getInfo len')
            ixRange = rangeByRange 0 (rangeSubSat szI 1)
        step' <- optimizeFunction opts
            (optimizeM opts)  -- TODO (optimizeFunctionFix optimizeM (mkInfo universal))
            (mkInfo ixRange)
            step
        constructFeat opts sym (len' :* init' :* step' :* Nil)
      -- TODO Should use fixed-point iteration, but `optimizeFunctionFix` only
      --      works for functions of type `a -> a`.

    optimizeFeat opts a args = optimizeFeatDefault opts a args

    -- parallel l (\x -> let y = e in e') => let y = e in parallel l (\x -> e')
    --
    -- Test case: nothing simple. Look at the index calculation in the
    -- outer second loop in turboDecode.
    constructFeatOpt opts sym@(C' Parallel) (len :* (lam1 :$ (lt :$ e1 :$ (lam2 :$ bd))) :* Nil)
        | Just       (SubConstr2 (Lambda v1)) <- prjLambda lam1
        , Just lam2'@(SubConstr2 (Lambda _ )) <- prjLambda lam2
        , Just Let <- prj lt
        , v1 `notMember` infoVars (getInfo e1)
        , SICS `inTarget` opts
        = do
             sym' <- constructFeat opts sym (len :* (lam1 :$ bd) :* Nil)
             sym'' <- constructFeat opts (reuseCLambda lam2') (sym' :* Nil)
             constructFeat opts Let (e1 :* sym'' :* Nil)

    constructFeatOpt _ (C' Parallel) (len :* _ :* Nil)
        | Just 0 <- viewLiteral len
        = return $ literalDecor []
      -- TODO Optimize when length is one. This requires a way to create an
      --      uninitialized array of length one, and setting the first element.
      --      Use `betaReduce` to apply `ixf` to the literal 0.

    constructFeatOpt opts (C' Parallel) (len :* (lam :$ (gix :$ arr2 :$ ix)) :* Nil)
        | Just (SubConstr2 (Lambda v1))   <- prjLambda lam
        , Just (C' GetIx)         <- prjF gix
        , Just (C' (Variable v2)) <- prjF ix
        , v1 == v2
        , v1 `notMember` infoVars (getInfo arr2)
        = constructFeat opts (c' SetLength) (len :* arr2 :* Nil)

    constructFeatOpt _ (C' Sequential) (len :* _ :* _ :* Nil)
        | Just 0 <- viewLiteral len
        = return $ literalDecor []
      -- TODO Optimize when length is one. This requires a way to create an
      --      uninitialized array of length one, and setting the first element.
      --      Use `betaReduce` to apply the step function.

    constructFeatOpt _ (C' Append) (a :* b :* Nil)
        | Just [] <- viewLiteral a = return b
        | Just [] <- viewLiteral b = return a

    constructFeatOpt opts (C' GetIx) ((op :$ _ :$ ixf) :* ix :* Nil)
        | Just (C' Parallel) <- prjF op
        = optimizeM opts $ betaReduce (stripDecor ix) (stripDecor ixf)
          -- TODO should not need to drop the decorations

    constructFeatOpt opts s@(C' GetIx) ((op :$ _ :$ arr) :* ix :* Nil)
        | Just (C' SetLength) <- prjF op
        = constructFeat opts s (arr :* ix :* Nil)

    constructFeatOpt _ (C' GetLength) (arr :* Nil)
        | Just as <- viewLiteral arr = return $ literalDecor $ genericLength as

    constructFeatOpt opts s@(C' GetLength) ((op :$ a :$ _ :$ _) :* Nil)
        | Just (C' Sequential) <- prjF op = return a
        | Just (C' SetIx)      <- prjF op = constructFeat opts s (a :* Nil)

    constructFeatOpt opts sym@(C' GetLength) ((op :$ a :$ b) :* Nil)
        | Just (C' Append) <- prjF op = do
            aLen <- constructFeat opts sym (a :* Nil)
            bLen <- constructFeat opts sym (b :* Nil)
            constructFeatOpt opts (c' Add) (aLen :* bLen :* Nil)
        | Just (C' Parallel)  <- prjF op = return a
        | Just (C' SetLength) <- prjF op = return a

    -- TODO remove this optimization when the singletonRange -> literal
    -- optimization in Feldspar.Core.Interpretation has been implemented (issue #27)
    constructFeatOpt _ (C' GetLength) (arr :* Nil)
        | len :> _ <- infoSize $ getInfo arr
        , isSingleton len
        = return $ literalDecor $ lowerBound len

    constructFeatOpt _ (C' SetLength) (len :* _ :* Nil)
        | Just 0 <- viewLiteral len = return $ literalDecor []

    constructFeatOpt _ (C' SetLength) ((getLength :$ arr') :* arr :* Nil)
        | Just (C' GetLength) <- prjF getLength
        , alphaEq arr arr'
        = return arr

    constructFeatOpt opts (C' SetLength) (len1 :* (par :$ len2 :$ ixf) :* Nil)
        | Just p@(C' Parallel) <- prjF par
        , alphaEq len1 len2
        = constructFeat opts p (len2 :* ixf :* Nil)

    constructFeatOpt opts (C' SetLength) (len1 :* (sq :$ len2 :$ ini :$ step) :* Nil)
        | Just s@(C' Sequential) <- prjF sq
        , alphaEq len1 len2
        = constructFeat opts s (len2 :* ini :* step :* Nil)

    constructFeatOpt _ (C' SetLength) (len :* arr :* Nil)
        | rlen      <- infoSize $ getInfo len
        , rarr :> _ <- infoSize $ getInfo arr
        , isSingleton rlen
        , isSingleton rarr
        , rlen == rarr
        = return arr

    constructFeatOpt opts a args = constructFeatUnOpt opts a args

    constructFeatUnOpt opts x@(C' _) = constructFeatUnOptDefault opts x

