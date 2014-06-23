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

module Feldspar.Core.Constructs.Condition
    ( module Language.Syntactic.Constructs.Condition
    ) where

import Language.Syntactic
import Language.Syntactic.Constructs.Binding hiding (subst)
import Language.Syntactic.Constructs.Binding.HigherOrder (CLambda(..))
import Language.Syntactic.Constructs.Literal
import Language.Syntactic.Constructs.Condition

import Feldspar.Lattice
import Feldspar.Core.Types
import Feldspar.Core.Interpretation
import Feldspar.Core.Constructs.Eq
import Feldspar.Core.Constructs.Ord
import Feldspar.Core.Constructs.Logic
import Feldspar.Core.Constructs.Binding (subst)

import Data.Typeable (Typeable)

instance Sharable Condition

instance Creases Condition

instance SizeProp (Condition :|| Type)
  where
    sizeProp (C' Condition) (_ :* WrapFull t :* WrapFull f :* Nil)
        = infoSize t \/ infoSize f

instance ( (Condition :|| Type) :<: dom
         , (Logic     :|| Type) :<: dom
         , (EQ        :|| Type) :<: dom
         , (ORD       :|| Type) :<: dom
         , (Variable  :|| Type) :<: dom
         , CLambda Type :<: dom
         , Creases dom
         , OptimizeSuper dom
         )
      => Optimize (Condition :|| Type) dom
  where
    -- If the condition is a variable, substitute for True/False in each
    -- branch
    optimizeFeat opts s@(C' Condition) (c :* t :* f :* Nil)
        | Just (C' (Variable v)) <- prjF c
        = optimizeFeatDefault opts s $ c :* subst v (literal True) t
                                         :* subst v (literal False) f
                                         :* Nil

    -- If condition is (a == b) and either a or b is a variable,
    -- substitute for the other in the True branch
    optimizeFeat opts s@(C' Condition) (c@(op :$ a :$ b) :* t :* f :* Nil)
        | Just (C' Equal) <- prjF op
        , Just (C' (Variable v)) <- prjF b
        = optimizeFeatDefault opts s $ c :* subst v a t :* f :* Nil

        | Just (C' Equal) <- prjF op
        , Just (C' (Variable v)) <- prjF a
        = optimizeFeatDefault opts s $ c :* subst v b t :* f :* Nil

    -- If condition is (a /= b) and either a or b is a variable,
    -- substitute for the other in the False branch
    optimizeFeat opts s@(C' Condition) (c@(op :$ a :$ b) :* t :* f :* Nil)
        | Just (C' NotEqual) <- prjF op
        , Just (C' (Variable v)) <- prjF b
        = optimizeFeatDefault opts s $ c :* t :* subst v a f :* Nil

        | Just (C' NotEqual) <- prjF op
        , Just (C' (Variable v)) <- prjF a
        = optimizeFeatDefault opts s $ c :* t :* subst v b f :* Nil

    optimizeFeat opts sym args = optimizeFeatDefault opts sym args

    -- If the condition is a literal, shortcut the condition
    constructFeatOpt opts (C' Condition) (c :* t :* f :* Nil)
        | Just cl <- viewLiteral c = return $ if cl then t else f

    -- If the branches a Boolean literals, shortcut as a truth table
    constructFeatOpt opts (C' Condition) (c :* t :* f :* Nil)
        | BoolType <- infoType (getInfo t)
        , Just tl <- viewLiteral t
        , Just fl <- viewLiteral f
        = case (tl,fl) of
            (True,False) -> return c
            (False,True) -> constructFeat opts (c' Not) (c :* Nil)

    -- It the branches are equal, the choice doesn't matter
    constructFeatOpt _ (C' Condition) (_ :* t :* f :* Nil)
        | alphaEq t f = return t

    -- Invert a negated condition
    constructFeatOpt opts cond@(C' Condition) ((op :$ c) :* t :* f :* Nil)
        | Just (C' Not) <- prjF op
        = constructFeat opts cond (c :* f :* t :* Nil)

    constructFeatOpt opts a args = constructFeatUnOpt opts a args

    constructFeatUnOpt opts x@(C' _) = constructFeatUnOptDefault opts x

