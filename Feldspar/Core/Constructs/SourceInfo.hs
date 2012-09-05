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

module Feldspar.Core.Constructs.SourceInfo
    ( module Language.Syntactic.Constructs.Identity
    , module Language.Syntactic.Constructs.Decoration
    , SourceInfo1 (..)
    ) where


import Language.Syntactic
import Language.Syntactic.Constructs.Binding
import Language.Syntactic.Constructs.Decoration
import Language.Syntactic.Constructs.Identity

import Feldspar.Core.Types
import Feldspar.Core.Interpretation



-- | Kind @* -> *@ version of 'SourceInfo'
data SourceInfo1 a = SourceInfo1 SourceInfo

instance AlphaEq dom dom dom env =>
    AlphaEq
        (Decor SourceInfo1 (Identity TypeCtx))
        (Decor SourceInfo1 (Identity TypeCtx))
        dom
        env
  where
    alphaEqSym = alphaEqSymDefault

instance Sharable (Decor SourceInfo1 (Identity TypeCtx))
  where
    sharable _ = True

instance SizeProp (Identity TypeCtx)
  where
    sizeProp Id (WrapFull a :* Nil) = infoSize a

instance SizeProp (Decor SourceInfo1 (Identity TypeCtx))
  where
    sizeProp = sizeProp . decorExpr

instance (Decor SourceInfo1 (Identity TypeCtx) :<: dom, Optimize dom dom) =>
    Optimize (Decor SourceInfo1 (Identity TypeCtx)) dom
  where
    optimizeFeat (Decor (SourceInfo1 src) Id) (a :* Nil) =
        localSource src $ optimizeM a

    constructFeatOpt (Decor (SourceInfo1 src) Id) (a :* Nil) = return a

    constructFeatUnOpt = constructFeatUnOptDefault

