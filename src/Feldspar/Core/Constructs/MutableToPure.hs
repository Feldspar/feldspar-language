{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

module Feldspar.Core.Constructs.MutableToPure
    ( MutableToPure (..)
    ) where

import qualified Control.Exception as C
import Data.Array.IArray
import Data.Array.MArray (freeze)
import Data.Array.Unsafe (unsafeFreeze)
import System.IO.Unsafe

import Language.Syntactic
import Language.Syntactic.Constructs.Binding
import Language.Syntactic.Constructs.Binding.HigherOrder (CLambda)

import Feldspar.Lattice
import Feldspar.Core.Types
import Feldspar.Core.Interpretation
import Feldspar.Core.Constructs.Binding

data MutableToPure a where
  RunMutableArray :: Type a => MutableToPure (Mut (MArr a) :-> Full [a])
  WithArray       :: Type b => MutableToPure (MArr a :-> ([a] -> Mut b) :-> Full (Mut b))

instance Semantic MutableToPure
  where
    semantics RunMutableArray = Sem "runMutableArray" runMutableArrayEval
    semantics WithArray       = Sem "withArray"       withArrayEval

runMutableArrayEval :: forall a . Mut (MArr a) -> [a]
runMutableArrayEval m = unsafePerformIO $
                        do marr <- m
                           iarr <- unsafeFreeze marr
                           return (elems (iarr :: Array Integer a))

withArrayEval :: forall a b. MArr a -> ([a] -> Mut b) -> Mut b
withArrayEval ma f
    = do a <- f (elems (unsafePerformIO $ freeze ma :: Array Integer a))
         C.evaluate a

instance Typed MutableToPure
  where
    typeDictSym RunMutableArray = Just Dict
    typeDictSym _ = Nothing

semanticInstances ''MutableToPure

instance EvalBind MutableToPure where evalBindSym = evalBindSymDefault

instance AlphaEq dom dom dom env => AlphaEq MutableToPure MutableToPure dom env
  where
    alphaEqSym = alphaEqSymDefault

instance Sharable MutableToPure

instance Cumulative MutableToPure

instance SizeProp MutableToPure
  where
    sizeProp RunMutableArray (WrapFull arr :* Nil) = infoSize arr
    sizeProp WithArray (_ :* WrapFull fun :* Nil) = snd $ infoSize fun


instance ( MutableToPure :<: dom
         , Let :<: dom
         , (Variable :|| Type) :<: dom
         , CLambda Type :<: dom
         , OptimizeSuper dom
         ) =>
           Optimize MutableToPure dom
  where
    optimizeFeat opts sym@WithArray (arr :* fun@(lam :$ body) :* Nil)
      | Dict <- exprDict fun
      , Dict <- exprDict body
      , Just (SubConstr2 (Lambda _)) <- prjLambda lam
      = do
          arr' <- optimizeM opts arr
          let (szl :> sze) = infoSize (getInfo arr')
          fun' <- optimizeFunction opts (optimizeM opts) (mkInfo (szl :> sze)) fun
          constructFeat opts sym (arr' :* fun' :* Nil)

    optimizeFeat opts sym args = optimizeFeatDefault opts sym args

    constructFeatUnOpt opts RunMutableArray args = constructFeatUnOptDefaultTyp opts typeRep RunMutableArray args
    constructFeatUnOpt opts WithArray args       = constructFeatUnOptDefaultTyp opts (MutType typeRep) WithArray args

