{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

module Feldspar.Core.Constructs where

import Data.Typeable

import Language.Syntactic
import Language.Syntactic.Constructs.Binding.HigherOrder

import Feldspar.Core.Types
import Feldspar.Core.Interpretation
import Feldspar.Core.Constructs.Array
import Feldspar.Core.Constructs.Binding
import Feldspar.Core.Constructs.Bits
import Feldspar.Core.Constructs.Complex
import Feldspar.Core.Constructs.Condition
import Feldspar.Core.Constructs.ConditionM
import Feldspar.Core.Constructs.Conversion
import Feldspar.Core.Constructs.Eq
import Feldspar.Core.Constructs.Error
import Feldspar.Core.Constructs.FFI
import Feldspar.Core.Constructs.Floating
import Feldspar.Core.Constructs.Fractional
import Feldspar.Core.Constructs.Future
import Feldspar.Core.Constructs.Integral
import Feldspar.Core.Constructs.Literal
import Feldspar.Core.Constructs.Logic
import Feldspar.Core.Constructs.Loop
import Feldspar.Core.Constructs.Mutable
import Feldspar.Core.Constructs.MutableArray
import Feldspar.Core.Constructs.MutableReference
import Feldspar.Core.Constructs.MutableToPure
import Feldspar.Core.Constructs.NoInline
import Feldspar.Core.Constructs.Par
import Feldspar.Core.Constructs.Num
import Feldspar.Core.Constructs.Ord
import Feldspar.Core.Constructs.Save
import Feldspar.Core.Constructs.SizeProp
import Feldspar.Core.Constructs.SourceInfo
import Feldspar.Core.Constructs.Trace
import Feldspar.Core.Constructs.Tuple

--------------------------------------------------------------------------------
-- * Domain
--------------------------------------------------------------------------------

type FeldSymbols
    =   (Decor SourceInfo1 Identity :|| Type)
    :+: (Condition  :|| Type)
    :+: (FFI        :|| Type)
    :+: (Let        :|| Type)
    :+: (Literal    :|| Type)
    :+: (Select     :|| Type)
    :+: (Tuple      :|| Type)
    :+: (Array      :|| Type)
    :+: (BITS       :|| Type)
    :+: (COMPLEX    :|| Type)
    :+: (Conversion :|| Type)
    :+: (EQ         :|| Type)
    :+: (Error      :|| Type)
    :+: (FLOATING   :|| Type)
    :+: (FRACTIONAL :|| Type)
    :+: (FUTURE     :|| Type)
    :+: (INTEGRAL   :|| Type)
    :+: (Logic      :|| Type)
    :+: (Loop       :|| Type)
    :+: (NUM        :|| Type)
    :+: (NoInline   :|| Type)
    :+: (ORD        :|| Type)
    :+: (PropSize   :|| Type)
    :+: (Save       :|| Type)
    :+: (Trace      :|| Type)
    :+: ConditionM Mut
    :+: LoopM Mut
    :+: MONAD Mut
    :+: Mutable
    :+: MutableArray
    :+: MutableReference
    :+: MutableToPure
    :+: MONAD Par
    :+: ParFeature
    :+: Empty

-- TODO We are currently a bit inconsistent in that `Type` constraints are sometimes attached
--      separately using `(:||)` and sometimes baked into the symbol type. `Mutable` and
--      `MutableToPure` (at least) have `Type` baked in. Note that `(MutableToPure :|| Type)` would
--      currently not work, since `WithArray` has monadic result type.

type FeldDom = FODomain FeldSymbols Typeable Type

newtype FeldDomainAll a = FeldDomainAll { getFeldDomainAll :: HODomain FeldSymbols Typeable Type a }

instance Constrained FeldDomainAll
  where
    type Sat FeldDomainAll = Typeable
    exprDict (FeldDomainAll s) = exprDict s

deriving instance (Project sym FeldSymbols) => Project sym FeldDomainAll

instance (InjectC sym FeldSymbols a, Typeable a) => InjectC sym FeldDomainAll a
  where
    injC = FeldDomainAll . injC

toFeld :: ASTF (HODomain FeldSymbols Typeable Type) a -> ASTF FeldDomainAll a
toFeld = fold $ appArgs . Sym . FeldDomainAll
  -- TODO Use unsafeCoerce?

fromFeld :: ASTF FeldDomainAll a -> ASTF (HODomain FeldSymbols Typeable Type) a
fromFeld = fold $ appArgs . Sym . getFeldDomainAll
  -- TODO Use unsafeCoerce?

instance IsHODomain FeldDomainAll Typeable Type
  where
    lambda f = case lambda (fromFeld . f . toFeld) of
        Sym s -> Sym (FeldDomainAll s)



--------------------------------------------------------------------------------
-- * Front end
--------------------------------------------------------------------------------

newtype Data a = Data { unData :: ASTF FeldDomainAll a }

deriving instance Typeable1 Data

instance Type a => Syntactic (Data a)
  where
    type Domain (Data a)   = FeldDomainAll
    type Internal (Data a) = a
    desugar = unData
    sugar   = Data

type SyntacticFeld a = (Syntactic a, Domain a ~ FeldDomainAll, Typeable (Internal a))

-- | Specialization of the 'Syntactic' class for the Feldspar domain
class    (SyntacticFeld a, Type (Internal a)) => Syntax a
instance (SyntacticFeld a, Type (Internal a)) => Syntax a
  -- It would be possible to let 'Syntax' be an alias instead of giving separate
  -- instances for all types. However, this leads to horrible error messages.
  -- For example, if 'Syntax' is an alias, the following expression gives a huge
  -- type error:
  --
  -- > eval (forLoop 10 0 (const (+id)))
  --
  -- The type error is not very readable now either, but at least it fits on the
  -- screen.

reifyF :: SyntacticFeld a => a -> ASTF FeldDom (Internal a)
reifyF = reifyTop . fromFeld . desugar

instance Type a => Eq (Data a)
  where
    Data a == Data b = alphaEq (reifyF a) (reifyF b)

instance Type a => Show (Data a)
  where
    show = render . reifyF . unData

sugarSymF :: ( ApplySym sig b FeldDomainAll
             , SyntacticN c b
             , InjectC (feature :|| Type) (HODomain FeldSymbols Typeable Type) (DenResult sig)
             , Type (DenResult sig)
             )
          => feature sig -> c
sugarSymF sym = sugarN $ appSym' $ Sym $ FeldDomainAll $ injC $ c' sym

