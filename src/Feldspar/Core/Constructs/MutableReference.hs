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

module Feldspar.Core.Constructs.MutableReference
where

import Data.IORef

import Language.Syntactic
import Language.Syntactic.Constructs.Binding

import Feldspar.Lattice
import Feldspar.Core.Types
import Feldspar.Core.Interpretation

data MutableReference a
  where
    NewRef :: Type a => MutableReference (a :-> Full (Mut (IORef a)))
    GetRef :: Type a => MutableReference (IORef a :-> Full (Mut a))
    SetRef :: Type a => MutableReference (IORef a :-> a :-> Full (Mut ()))

instance Semantic MutableReference
  where
    semantics NewRef = Sem "newRef" newIORef
    semantics GetRef = Sem "getRef" readIORef
    semantics SetRef = Sem "setRef" writeIORef

instance Equality MutableReference where equal = equalDefault; exprHash = exprHashDefault
instance Render   MutableReference where renderArgs = renderArgsDefault
instance ToTree   MutableReference
instance Eval     MutableReference where evaluate = evaluateDefault
instance EvalBind MutableReference where evalBindSym = evalBindSymDefault
instance Sharable MutableReference
  -- Will not be shared anyway, because 'maybeWitnessSat' returns 'Nothing'

instance AlphaEq dom dom dom env =>
    AlphaEq MutableReference MutableReference dom env
  where
    alphaEqSym = alphaEqSymDefault

instance SizeProp MutableReference
  where
    sizeProp NewRef _ = universal
    sizeProp GetRef _ = universal
    sizeProp SetRef _ = universal

instance (MutableReference :<: dom, Optimize dom dom) =>
    Optimize MutableReference dom
  where
    constructFeatUnOpt NewRef args = constructFeatUnOptDefaultTyp (MutType $ RefType typeRep) NewRef args
    constructFeatUnOpt GetRef args = constructFeatUnOptDefaultTyp (MutType typeRep) GetRef args
    constructFeatUnOpt SetRef args = constructFeatUnOptDefaultTyp (MutType typeRep) SetRef args

