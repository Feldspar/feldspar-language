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

module Feldspar.Core.Constructs.MutableReference
where

import Data.IORef

import Language.Syntactic
import Language.Syntactic.Constructs.Binding
import Language.Syntactic.Constructs.Binding.HigherOrder

import Feldspar.Lattice
import Feldspar.Core.Types
import Feldspar.Core.Interpretation
import Feldspar.Core.Constructs.Binding
import Feldspar.Core.Constructs.Mutable


data MutableReference a
  where
    NewRef :: Type a => MutableReference (a :-> Full (Mut (IORef a)))
    GetRef :: Type a => MutableReference (IORef a :-> Full (Mut a))
    SetRef :: Type a => MutableReference (IORef a :-> a :-> Full (Mut ()))
    ModRef :: Type a => MutableReference (IORef a :-> (a -> a) :-> Full (Mut ()))

instance Semantic MutableReference
  where
    semantics NewRef = Sem "newRef" newIORef
    semantics GetRef = Sem "getRef" readIORef
    semantics SetRef = Sem "setRef" writeIORef
    semantics ModRef = Sem "modRef" (\r f -> readIORef r >>= writeIORef r . f)

semanticInstances ''MutableReference

instance EvalBind MutableReference where evalBindSym = evalBindSymDefault

instance AlphaEq dom dom dom env =>
    AlphaEq MutableReference MutableReference dom env
  where
    alphaEqSym = alphaEqSymDefault

instance Sharable MutableReference

instance Monotonic MutableReference

instance SizeProp MutableReference
  where
    sizeProp NewRef _ = universal
    sizeProp GetRef _ = universal
    sizeProp SetRef _ = universal
    sizeProp ModRef _ = universal

instance ( MutableReference :<: dom
         , MONAD Mut :<: dom
         , Project (CLambda Type) dom
         , Project (Variable :|| Type) dom
         , OptimizeSuper dom
         )
      => Optimize MutableReference dom
  where
    -- modifyRef _ id ==> return ()
    constructFeatUnOpt opts ModRef (_ :* (lam :$ body) :* Nil)
       | Just (SubConstr2 (Lambda v1)) <- prjLambda lam
       , Just (C' (Variable v2)) <- prjF body
       , v1 == v2
       = constructFeatUnOptDefaultTyp opts (MutType UnitType) Return (literalDecor () :* Nil)

    constructFeatUnOpt opts NewRef args = constructFeatUnOptDefaultTyp opts (MutType $ RefType typeRep) NewRef args
    constructFeatUnOpt opts GetRef args = constructFeatUnOptDefaultTyp opts (MutType typeRep) GetRef args
    constructFeatUnOpt opts SetRef args = constructFeatUnOptDefaultTyp opts (MutType typeRep) SetRef args
    constructFeatUnOpt opts ModRef args = constructFeatUnOptDefaultTyp opts (MutType typeRep) ModRef args

