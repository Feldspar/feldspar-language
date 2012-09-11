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
{-# LANGUAGE CPP #-}

module Feldspar.Core.Constructs.MutableToPure
    ( MutableToPure (..)
    ) where

import qualified Control.Exception as C
import Data.Array.IArray
#if __GLASGOW_HASKELL__>=704
import Data.Array.MArray (freeze)
import Data.Array.Unsafe (unsafeFreeze)
#else
import Data.Array.MArray (freeze, unsafeFreeze)
#endif
import System.IO.Unsafe

import Language.Syntactic
import Language.Syntactic.Interpretation.Semantics
import Language.Syntactic.Constructs.Binding

import Feldspar.Lattice
import Feldspar.Core.Types
import Feldspar.Core.Interpretation

data MutableToPure a where
  RunMutableArray :: Type a => MutableToPure (Mut (MArr a) :-> Full [a])
  WithArray       :: Type b => MutableToPure (MArr a :-> ([a] -> Mut b) :-> Full (Mut b))

instance WitnessCons MutableToPure
  where
    witnessCons RunMutableArray = ConsWit
    witnessCons WithArray       = ConsWit

instance MaybeWitnessSat TypeCtx MutableToPure
  where
    maybeWitnessSat _ RunMutableArray = Just SatWit
    maybeWitnessSat _ _               = Nothing

instance Semantic MutableToPure
  where
    semantics RunMutableArray = Sem "runMutableArray" runMutableArrayEval
    semantics WithArray       = Sem "withArray"       withArrayEval

runMutableArrayEval :: forall i a . Mut (MArr a) -> [a]
runMutableArrayEval m = unsafePerformIO $
                        do marr <- m
                           iarr <- unsafeFreeze marr
                           return (elems (iarr :: Array WordN a))

withArrayEval :: forall i a b. MArr a -> ([a] -> Mut b) -> Mut b
withArrayEval ma f
    = do a <- f (elems (unsafePerformIO $ freeze ma :: Array WordN a))
         C.evaluate a

instance ExprEq   MutableToPure where exprEq = exprEqSem; exprHash = exprHashSem
instance Render   MutableToPure where renderPart = renderPartSem
instance ToTree   MutableToPure
instance Eval     MutableToPure where evaluate = evaluateSem
instance EvalBind MutableToPure where evalBindSym = evalBindSymDefault
instance Sharable MutableToPure

instance AlphaEq dom dom dom env => AlphaEq MutableToPure MutableToPure dom env
  where
    alphaEqSym = alphaEqSymDefault

instance SizeProp MutableToPure
  where
    sizeProp RunMutableArray _ = universal
    sizeProp WithArray       _ = universal

instance (MutableToPure :<: dom, Optimize dom dom) => Optimize MutableToPure dom
  where
    constructFeatUnOpt RunMutableArray args = constructFeatUnOptDefaultTyp typeRep RunMutableArray args
    constructFeatUnOpt WithArray args       = constructFeatUnOptDefaultTyp (MutType typeRep) WithArray args

