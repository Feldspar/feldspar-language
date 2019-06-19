{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}

module Feldspar.Core.Syntactic
    ( Syntactic(..)
    , ASTF
    , resugar
    , stringTree
    , showAST
    , drawAST
    , writeHtmlAST
    , showDecorWith
    , drawDecorWith
    , render
    , alphaEq
    ) where

#ifndef INCREMENTAL_CSE

import Language.Syntactic
import Language.Syntactic.Constructs.Binding (alphaEq)
import Language.Syntactic.Constructs.Decoration (showDecorWith, drawDecorWith)

#else

#endif
