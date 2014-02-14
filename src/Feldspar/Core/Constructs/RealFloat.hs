{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Feldspar.Core.Constructs.RealFloat
    ( REALFLOAT (..)
    ) where

import Language.Syntactic
import Language.Syntactic.Constructs.Binding

import Feldspar.Core.Types
import Feldspar.Core.Interpretation

data REALFLOAT a
  where
    Atan2   :: (Type a, RealFloat a) => REALFLOAT (a :-> a :-> Full a)

instance Semantic REALFLOAT
  where
    semantics Atan2   = Sem "atan2" Prelude.atan2

semanticInstances ''REALFLOAT

instance EvalBind REALFLOAT where evalBindSym = evalBindSymDefault

instance AlphaEq dom dom dom env => AlphaEq REALFLOAT REALFLOAT dom env
  where
    alphaEqSym = alphaEqSymDefault

instance Sharable REALFLOAT

instance Monotonic REALFLOAT

instance SizeProp (REALFLOAT :|| Type)
  where
    sizeProp (C' s) = sizePropDefault s

instance ( (REALFLOAT :|| Type) :<: dom
         , OptimizeSuper dom)
      => Optimize (REALFLOAT :|| Type) dom
  where
    constructFeatUnOpt opts a@(C' _) = constructFeatUnOptDefault opts a

