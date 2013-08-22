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

module Feldspar.Core.Constructs.MutableArray
where

import Control.Monad
import Data.Array.IO

import Language.Syntactic
import Language.Syntactic.Constructs.Binding

import Feldspar.Lattice
import Feldspar.Core.Types
import Feldspar.Core.Interpretation

data MutableArray a
  where
    NewArr    :: Type a => MutableArray (Length :-> a :-> Full (Mut (MArr a)))
    NewArr_   :: Type a => MutableArray (Length :-> Full (Mut (MArr a)))
    GetArr    :: Type a => MutableArray (MArr a :-> Index :-> Full (Mut a))
    SetArr    :: MutableArray (MArr a :-> Index :-> a :-> Full (Mut ()))
    ArrLength :: MutableArray (MArr a :-> Full (Mut Length))
      -- TODO Should be pure?

instance Semantic MutableArray
  where
    semantics NewArr    = Sem "newMArr"   (\l -> newArray  (0,l-1))
    semantics NewArr_   = Sem "newMArr_"  (\l -> newListArray (0,l-1)
        [error $ "Undefined element at index " ++ show i | i <- [0..l-1]])
    semantics GetArr    = Sem "getMArr"   readArray
    semantics SetArr    = Sem "setMArr"   writeArray
    semantics ArrLength = Sem "arrLength" (getBounds >=> \(l,u) -> return (u-l+1))

instance Equality MutableArray where equal = equalDefault; exprHash = exprHashDefault
instance Render   MutableArray where renderArgs = renderArgsDefault
instance ToTree   MutableArray
instance Eval     MutableArray where evaluate = evaluateDefault
instance EvalBind MutableArray where evalBindSym = evalBindSymDefault
instance Sharable MutableArray

instance AlphaEq dom dom dom env => AlphaEq MutableArray MutableArray dom env
  where
    alphaEqSym = alphaEqSymDefault

instance SizeProp MutableArray
  where
    sizeProp NewArr  (WrapFull len :* _ :* Nil) = infoSize len :> universal
    sizeProp NewArr_ (WrapFull len :* Nil)      = infoSize len :> universal
    sizeProp GetArr  _                          = universal
    sizeProp SetArr  _                          = universal
    sizeProp ArrLength (WrapFull arr :* Nil)    = len
      where
        len :> _ = infoSize arr

instance (MutableArray :<: dom, Optimize dom dom) => Optimize MutableArray dom
  where
    constructFeatUnOpt opts NewArr    args = constructFeatUnOptDefaultTyp opts (MutType $ MArrType typeRep) NewArr args
    constructFeatUnOpt opts NewArr_   args = constructFeatUnOptDefaultTyp opts (MutType $ MArrType typeRep) NewArr_ args
    constructFeatUnOpt opts GetArr    args = constructFeatUnOptDefaultTyp opts (MutType typeRep) GetArr args
    constructFeatUnOpt opts SetArr    args = constructFeatUnOptDefaultTyp opts (MutType typeRep) SetArr args
    constructFeatUnOpt opts ArrLength args = constructFeatUnOptDefaultTyp opts (MutType typeRep) ArrLength args

