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

module Feldspar.Core.Constructs.SizeProp where

import Language.Syntactic
import Language.Syntactic.Constructs.Binding

import Feldspar.Lattice
import Feldspar.Core.Types
import Feldspar.Core.Interpretation

data PropSize a
  where
    PropSize :: (Type a, Type b) =>
        (Size a -> Size b) -> PropSize (a :-> b :-> Full b)

instance Semantic PropSize
  where
    semantics (PropSize _) = Sem "propSize" (const id)

instance Equality PropSize where equal = equalDefault; exprHash = exprHashDefault
instance Render   PropSize where renderArgs = renderArgsDefault
instance ToTree   PropSize
instance Eval     PropSize where evaluate = evaluateDefault
instance EvalBind PropSize where evalBindSym = evalBindSymDefault
instance Sharable PropSize

{-
instance SizeProp PropSize
  where
    sizeProp (PropSize prop) (WrapFull a :* WrapFull b :* Nil) =
        prop (infoSize a) /\ infoSize b
-}

instance AlphaEq dom dom dom env => AlphaEq PropSize PropSize dom env
  where
    alphaEqSym = alphaEqSymDefault

{-
instance (PropSize :<: dom, Optimize dom dom) => Optimize PropSize dom
  where
    constructFeatOpt (PropSize prop) (a :* b :* Nil) =
        return $ updateDecor (f (prop (infoSize $ getInfo a))) b
      where
        f :: Lattice (Size b) => Size b -> Info b -> Info b
        f newSize info = info {infoSize = infoSize info /\ newSize}

    constructFeatUnOpt = constructFeatUnOptDefault
-}

