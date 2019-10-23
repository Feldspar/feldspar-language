{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}

module Feldspar.Core.Syntactic
    ( Syntactic(..)
    , ASTF
    , resugar
    , render
    , alphaEq
    , evalBind
    ) where

#ifndef INCREMENTAL_CSE

import Language.Syntactic
import Language.Syntactic.Constructs.Binding (alphaEq, evalBind)

#else

import Feldspar.Core.Reify

#endif
