{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

module Feldspar.Core.Constructs.Conversion
    ( Conversion (..)
    ) where

import Language.Syntactic
import Language.Syntactic.Constructs.Binding

import Feldspar.Range
import Feldspar.Lattice
import Feldspar.Core.Types
import Feldspar.Core.Interpretation

data Conversion a
  where
    F2I     :: (Type a, Integral a) => Conversion (Float :-> Full a)
    I2N     :: (Type a, Type b, Integral a, Num b
               ,Size a ~ Range a
               ) =>
               Conversion (a :-> Full b)
    B2I     :: (Type a, Integral a) => Conversion (Bool  :-> Full a)
    Round   :: (Type a, Integral a) => Conversion (Float :-> Full a)
    Ceiling :: (Type a, Integral a) => Conversion (Float :-> Full a)
    Floor   :: (Type a, Integral a) => Conversion (Float :-> Full a)

rangeToSize :: Lattice (Size a) => TypeRep a -> Range Integer -> Size a
rangeToSize (IntType _ _) r = rangeProp r
rangeToSize _ _             = universal

rangeProp :: forall a . (Bounded a, Integral a) => Range Integer -> Range a
rangeProp (Range l u)
    | withinBounds l && withinBounds u
        = range (fromIntegral l) (fromIntegral u)
    | otherwise = range minBound maxBound
  where withinBounds i = toInteger (minBound :: a) <= i &&
                         i <= toInteger (maxBound :: a)

instance Semantic Conversion
  where
    semantics F2I     = Sem "f2i"     truncate
    semantics I2N     = Sem "i2n"     (fromInteger.toInteger)
    semantics B2I     = Sem "b2i"     (\b -> if b then 1 else 0)
    semantics Round   = Sem "round"   round
    semantics Ceiling = Sem "ceiling" ceiling
    semantics Floor   = Sem "floor"   floor

instance Equality Conversion where equal = equalDefault; exprHash = exprHashDefault
instance Render   Conversion where renderArgs = renderArgsDefault
instance Eval     Conversion where evaluate = evaluateDefault
instance ToTree   Conversion
instance EvalBind Conversion where evalBindSym = evalBindSymDefault
instance Sharable Conversion

instance AlphaEq dom dom dom env => AlphaEq Conversion Conversion dom env
  where
    alphaEqSym = alphaEqSymDefault

{-
instance SizeProp Conversion
  where
    sizeProp F2I     _ = universal
    sizeProp i2n@I2N (WrapFull a :* Nil)
        = rangeToSize (resultType i2n) (mapMonotonic toInteger (infoSize a))
    sizeProp B2I     _ = universal
    sizeProp Round   _ = universal
    sizeProp Ceiling _ = universal
    sizeProp Floor   _ = universal

instance (Conversion :<: dom, Optimize dom dom) => Optimize Conversion dom
  where
    constructFeatOpt i2n@I2N (a :* Nil)
        | Just TypeEq <- typeEq (resultType i2n) (infoType $ getInfo a)
        = return a

    constructFeatOpt a args = constructFeatUnOpt a args

    constructFeatUnOpt = constructFeatUnOptDefault
-}

