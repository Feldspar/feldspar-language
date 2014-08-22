{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Feldspar.Core.Constructs.MultiDim where

import Language.Syntactic
import Feldspar.Lattice
import Feldspar.Core.Types
import Feldspar.Core.Interpretation
import Feldspar.Core.Constructs.Binding

data MultiDIM a where
  MDShape   :: Type a => MultiDIM (MultiDim a :-> Full [Length])
  MDPayload :: Type a => MultiDIM (MultiDim a :-> Full [a])

instance Semantic MultiDIM where
  semantics MDShape   = Sem "shape" mdDim
  semantics MDPayload = Sem "arr" mdArr

semanticInstances ''MultiDIM

instance EvalBind MultiDIM where evalBindSym = evalBindSymDefault

instance AlphaEq dom dom dom env => AlphaEq MultiDIM MultiDIM dom env
  where
    alphaEqSym = alphaEqSymDefault

instance Sharable MultiDIM

instance Cumulative MultiDIM

instance SizeProp (MultiDIM :|| Type) where
  sizeProp (C' MDShape) (WrapFull md :* Nil) = len
    where (len,_) = infoSize md
  sizeProp (C' MDPayload) (WrapFull md :* Nil) = arr
    where (_,arr) = infoSize md

instance (SizeProp (MultiDim :|| Type)
         ,(MultiDim :|| Type) :<: dom
         ) =>
         Optimize (MultiDim :|| Type) dom where
  constructFeatUnOpt opts x@(C' _) = constructFeatUnOptDefault opts x
