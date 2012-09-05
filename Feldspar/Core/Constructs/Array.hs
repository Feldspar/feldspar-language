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

module Feldspar.Core.Constructs.Array
where

import Control.Monad
import Data.List
import Data.Map (notMember)

import Language.Syntactic
import Language.Syntactic.Interpretation.Semantics
import Language.Syntactic.Constructs.Binding
import Language.Syntactic.Constructs.Literal

import Feldspar.Range
import Feldspar.Lattice
import Feldspar.Core.Types
import Feldspar.Core.Interpretation
import Feldspar.Core.Constructs.Binding
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

instance WitnessCons Array
  where
    witnessCons Parallel   = ConsWit
    witnessCons Sequential = ConsWit
    witnessCons Append     = ConsWit
    witnessCons GetIx      = ConsWit
    witnessCons SetIx      = ConsWit
    witnessCons GetLength  = ConsWit
    witnessCons SetLength  = ConsWit

instance WitnessSat Array
  where
    type SatContext Array = TypeCtx
    witnessSat Parallel   = SatWit
    witnessSat Sequential = SatWit
    witnessSat Append     = SatWit
    witnessSat GetIx      = SatWit
    witnessSat SetIx      = SatWit
    witnessSat GetLength  = SatWit
    witnessSat SetLength  = SatWit

instance MaybeWitnessSat TypeCtx Array
  where
    maybeWitnessSat = maybeWitnessSatDefault

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

instance ExprEq   Array where exprEq = exprEqSem; exprHash = exprHashSem
instance Render   Array where renderPart = renderPartSem
instance ToTree   Array
instance Eval     Array where evaluate = evaluateSem
instance EvalBind Array where evalBindSym = evalBindSymDefault

instance AlphaEq dom dom dom env => AlphaEq Array Array dom env
  where
    alphaEqSym = alphaEqSymDefault

instance Sharable Array
  where
    sharable GetIx = False
    sharable _     = True

instance SizeProp Array
  where
    sizeProp Parallel (WrapFull len :* WrapFull ixf :* Nil) =
        infoSize len :> infoSize ixf
    sizeProp Sequential (WrapFull len :* init :* WrapFull step :* Nil) =
        infoSize len :> fst (infoSize step)
    sizeProp Append (WrapFull arra :* WrapFull arrb :* Nil) =
        (alen + blen) :> (aelem \/ belem)
      where
        alen :> aelem = infoSize arra
        blen :> belem = infoSize arrb
    sizeProp GetIx (WrapFull arr :* ix :* Nil) = elem
      where
        _ :> elem = infoSize arr
    sizeProp SetIx (WrapFull arr :* ix :* WrapFull e :* Nil) =
        len :> (elem \/ infoSize e)
      where
        len :> elem = infoSize arr
    sizeProp GetLength (WrapFull arr :* Nil) = len
      where
        len :> _ = infoSize arr
    sizeProp SetLength (WrapFull len :* WrapFull arr :* Nil) =
        infoSize len :> elem
      where
        _ :> elem = infoSize arr



instance
    ( Array :<: dom
    , NUM :<: dom
    , ORD :<: dom
    , OptimizeSuper dom
    ) =>
      Optimize Array dom
  where
    optimizeFeat Parallel (len :* ixf :* Nil) = do
        len' <- optimizeM len
        let szI     = infoSize (getInfo len')
            ixRange = rangeByRange 0 (szI-1)
        ixf' <- optimizeFunction optimizeM (mkInfo ixRange) ixf
        constructFeat Parallel (len' :* ixf' :* Nil)

    optimizeFeat Sequential (len :* init :* step :* Nil) = do
        len'  <- optimizeM len
        init' <- optimizeM init
        let szI     = infoSize (getInfo len')
            ixRange = rangeByRange 0 (szI-1)
        step' <- optimizeFunction
            optimizeM  -- TODO (optimizeFunctionFix optimizeM (mkInfo universal))
            (mkInfo ixRange)
            step
        constructFeat Sequential (len' :* init' :* step' :* Nil)
      -- TODO Should use fixed-point iteration, but `optimizeFunctionFix` only
      --      works for functions of type `a -> a`.

    optimizeFeat a args = optimizeFeatDefault a args

    constructFeatOpt Parallel (len :* ixf :* Nil)
        | Just 0 <- viewLiteral len
        = return $ literalDecor []
      -- TODO Optimize when length is one. This requires a way to create an
      --      uninitialized array of length one, and setting the first element.
      --      Use `betaReduce` to apply `ixf` to the literal 0.

    constructFeatOpt Parallel (len :* (lam :$ (gix :$ arr2 :$ ix)) :* Nil)
        | Just (_,Lambda v1)   <- prjDecorCtx typeCtx lam
        , Just (_,GetIx)       <- prjDecor gix
        , Just (_,Variable v2) <- prjDecorCtx typeCtx ix
        , v1 == v2
        , v1 `notMember` infoVars (getInfo arr2)
        = constructFeat SetLength (len :* arr2 :* Nil)

    constructFeatOpt Sequential (len :* init :* ixf :* Nil)
        | Just 0 <- viewLiteral len
        = return $ literalDecor []
      -- TODO Optimize when length is one. This requires a way to create an
      --      uninitialized array of length one, and setting the first element.
      --      Use `betaReduce` to apply the step function.

    constructFeatOpt Append (a :* b :* Nil)
        | Just [] <- viewLiteral a = return b
        | Just [] <- viewLiteral b = return a

    constructFeatOpt GetIx ((op :$ _ :$ ixf) :* ix :* Nil)
        | Just (_, Parallel) <- prjDecor op
        = optimizeM $ betaReduce typeCtx (stripDecor ix) (stripDecor ixf)
          -- TODO should not need to drop the decorations

    constructFeatOpt GetIx ((op :$ len :$ arr) :* ix :* Nil)
        | Just (_, SetLength) <- prjDecor op
        = constructFeat GetIx (arr :* ix :* Nil)

    constructFeatOpt GetLength (arr :* Nil)
        | Just as <- viewLiteral arr = return $ literalDecor $ genericLength as

    constructFeatOpt GetLength (((prjDecor -> Just (_,op)) :$ a :$ _ :$ _) :* Nil)
        | Sequential <- op = return a
        | SetIx      <- op = constructFeat GetLength (a :* Nil)

    constructFeatOpt GetLength (((prjDecor -> Just (_,op)) :$ a :$ b) :* Nil)
        | Append <- op = do
            aLen <- constructFeat GetLength (a :* Nil)
            bLen <- constructFeat GetLength (b :* Nil)
            constructFeatOpt Add (aLen :* bLen :* Nil)
        | Parallel  <- op = return a
        | SetLength <- op = return a

    -- TODO remove this optimization when the singletonRange -> literal
    -- optimization in Feldspar.Core.Interpretation has been implemented
    constructFeatOpt GetLength (arr :* Nil)
        | len :> _ <- infoSize $ getInfo arr
        , isSingleton len
        = return $ literalDecor $ lowerBound len

    constructFeatOpt SetLength (len :* arr :* Nil)
        | Just 0 <- viewLiteral len = return $ literalDecor []

    constructFeatOpt SetLength ((getLength :$ arr') :* arr :* Nil)
        | Just (_,GetLength) <- prjDecor getLength
        , alphaEq arr arr'
        = return arr

    constructFeatOpt SetLength (len :* arr :* Nil)
        | rlen      <- infoSize $ getInfo len
        , rarr :> _ <- infoSize $ getInfo arr
        , isSingleton rlen
        , isSingleton rarr
        , rlen == rarr
        = return arr

    constructFeatOpt a args = constructFeatUnOpt a args

    constructFeatUnOpt = constructFeatUnOptDefault

