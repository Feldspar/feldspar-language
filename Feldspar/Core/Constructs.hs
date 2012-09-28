{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
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

{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

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
    :+: (Condition :|| Type)
--    :+: ConditionM Mut
--    :+: FFI
--    :+: Let
    :+: (Literal :|| Type)
--    :+: (Select  :||| Type)
--    :+: (Tuple   :||| Type)
--    :+: Array
    :+: (BITS :|| Type)
--    :+: COMPLEX
    :+: (Conversion :|| Type)
    :+: (EQ :|| Type)
    :+: (Error :|| Type)
    :+: (FLOATING :|| Type)
    :+: (FRACTIONAL :|| Type)
--    :+: FUTURE
    :+: (INTEGRAL :|| Type)
    :+: (Logic :|| Type)
--    :+: Loop
--    :+: LoopM Mut
--    :+: MONAD Mut
--    :+: Mutable
--    :+: MutableArray
--    :+: MutableReference
--    :+: MutableToPure
--    :+: MONAD Par
    :+: (NUM :|| Type)
--    :+: NoInline
    :+: (ORD :|| Type)
--    :+: ParFeature
--    :+: PropSize
--    :+: Save
--    :+: Trace
    :+: Empty

data Empty a

instance Render Empty
instance AlphaEq Empty Empty dom env
  where
    alphaEqSym _ _ _ _ = return False
instance ToTree Empty
instance EvalBind Empty
  where
    evalBindSym _ _ = error "can't evaluate Empty"
instance Optimize Empty dom
  where
    constructFeatUnOpt _ _ = error "can't optimize Empty"

type FODomain dom constr = (Lambda :+: Variable :+: dom) :|| constr

type FeldDomain = FODomain FeldSymbols Typeable

type FeldDomainAll = HODomain FeldSymbols Typeable

--newtype FeldDomain a = FeldDomain (FeldSymbols a)


--deriving instance (sym :<: FeldSymbols) => sym :<: FeldDomain
--deriving instance (Project sym FeldSymbols) => Project sym FeldDomain

----instance (InjectC sym FeldSymbols a) => InjectC sym FeldDomain a
----    where
----      injC = injC . FeldDomain

----instance Constrained FeldDomain
--    where
--        type Sat FeldDomain = Sat FeldSymbols
--        exprDict (FeldDomain a) = exprDict a

--deriving instance Equality FeldDomain
--deriving instance Render   FeldDomain
--deriving instance ToTree   FeldDomain
--deriving instance Eval     FeldDomain
--deriving instance EvalBind FeldDomain

--instance VarEqEnv env => AlphaEq
--    FeldDomain
--    FeldDomain
--    ((Lambda :+: (Variable :+: ((FeldDomain :|| Eq) :| Show))) :|| Typeable)
--    env
--  where
--    alphaEqSym (FeldDomain a) aArgs (FeldDomain b) bArgs =
--        alphaEqSym a aArgs b bArgs

--instance AlphaEq
--    FeldDomain
--    FeldDomain
--    ((Lambda :+: (Variable :+: ((FeldDomain :|| Eq) :| Show))) :|| Typeable)
--    [(VarId, VarId)]
--  where
--    alphaEqSym (FeldDomain a) aArgs (FeldDomain b) bArgs =
--        alphaEqSym a aArgs b bArgs

{-
instance Equality dom => AlphaEq dom dom (Decor Info (dom :|| Typeable)) [(VarId,VarId)]
  where
    alphaEqSym = alphaEqSymDefault
-}

--deriving instance Sharable FeldDomain

{-
instance Optimize FeldDomain (Lambda TypeCtx :+: (Variable TypeCtx :+: FeldDomain))
  where
    optimizeFeat       (FeldDomain a) = optimizeFeat       a
    constructFeatOpt   (FeldDomain a) = constructFeatOpt   a
    constructFeatUnOpt (FeldDomain a) = constructFeatUnOpt a
-}




--------------------------------------------------------------------------------
-- * Front end
--------------------------------------------------------------------------------

newtype Data a = Data { unData :: ASTF FeldDomainAll a }

deriving instance Typeable1 Data

instance Type a => Syntactic (Data a) FeldDomainAll
  where
    type Internal (Data a) = a
    desugar = unData
    sugar   = Data

-- | Specialization of the 'Syntactic' class for the Feldspar domain
class
    ( Syntactic a FeldDomainAll
    , SyntacticN a (ASTF FeldDomainAll (Internal a))
    , Type (Internal a)
    ) =>
      Syntax a
  -- It would be possible to let 'Syntax' be an alias instead of giving separate
  -- instances for all types. However, this leads to horrible error messages.
  -- For example, if 'Syntax' is an alias, the following expression gives a huge
  -- type error:
  --
  -- > eval (forLoop 10 0 (const (+id)))
  --
  -- The type error is not very readable now either, but at least it fits on the
  -- screen.

instance Type a => Syntax (Data a)

instance Type a => Eq (Data a)
  where
    Data a == Data b = alphaEq (reify a) (reify b)

instance Type a => Show (Data a)
  where
    show (Data a) = render $ reify a

c' :: (Type (DenResult sig)) => feature sig -> (feature :|| Type) sig
c' = C'

sugarSymF :: ( ApplySym sig b dom
             , SyntacticN c b
             -- , InjectC (feature :|| Type) (AST dom) (DenResult sig)
             , InjectC (feature :|| Type) dom (DenResult sig)
             , Type (DenResult sig)
             )
          => feature sig -> c
sugarSymF sym = sugarSymC (c' sym)

