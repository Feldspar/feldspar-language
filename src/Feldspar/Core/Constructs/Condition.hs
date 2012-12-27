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
import Language.Syntactic.Constructs.Binding
import Language.Syntactic.Constructs.Condition

import Feldspar.Lattice
import Feldspar.Core.Types
import Feldspar.Core.Interpretation
import Feldspar.Core.Constructs.Logic

instance Sharable Condition

instance SizeProp (Condition :|| Type)
  where
    sizeProp (C' Condition) (_ :* WrapFull t :* WrapFull f :* Nil)
        = infoSize t \/ infoSize f

instance ( (Condition :|| Type) :<: dom
         , (Logic     :|| Type) :<: dom
         , OptimizeSuper dom
         )
      => Optimize (Condition :|| Type) dom
  where
    constructFeatOpt _ (C' Condition) (c :* t :* f :* Nil)
        | Just c' <- viewLiteral c = return $ if c' then t else f

    constructFeatOpt _ (C' Condition) (_ :* t :* f :* Nil)
        | alphaEq t f = return t

    constructFeatOpt opts cond@(C' Condition) ((op :$ c) :* t :* f :* Nil)
        | Just (C' Not) <- prjF op
        = constructFeat opts cond (c :* f :* t :* Nil)

    constructFeatOpt opts a args = constructFeatUnOpt opts a args

    constructFeatUnOpt opts x@(C' _) = constructFeatUnOptDefault opts x

