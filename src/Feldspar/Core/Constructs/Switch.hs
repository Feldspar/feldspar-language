{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Feldspar.Core.Constructs.Switch where

import Language.Syntactic
import Language.Syntactic.Constructs.Binding

import Feldspar.Core.Types
import Feldspar.Core.Interpretation


data Switch a
  where
    Switch :: (Type b) => Switch (b :-> Full b)

instance Semantic Switch
  where
    semantics Switch = Sem "switch" id

semanticInstances ''Switch

instance EvalBind Switch where evalBindSym = evalBindSymDefault

instance AlphaEq dom dom dom env => AlphaEq Switch Switch dom env
  where
    alphaEqSym = alphaEqSymDefault

instance Sharable Switch

instance SizeProp (Switch :|| Type)
  where
    sizeProp (C' Switch) (WrapFull sz :* Nil) = infoSize sz

instance
    ( (Switch :|| Type) :<: dom
    , OptimizeSuper dom
    ) =>
      Optimize (Switch :|| Type) dom
  where
    constructFeatUnOpt opts x@(C' _) = constructFeatUnOptDefault opts x


