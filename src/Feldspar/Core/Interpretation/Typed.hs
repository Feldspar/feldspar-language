{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverlappingInstances #-}

module Feldspar.Core.Interpretation.Typed
where

import Language.Syntactic
import Language.Syntactic.Constructs.Binding
import Language.Syntactic.Constructs.Binding.HigherOrder
import Language.Syntactic.Constructs.Decoration

import Feldspar.Core.Types (Type)

class Typed dom
  where
    dict :: dom a -> Maybe (Dict (Type (DenResult a)))

instance Typed (sub :|| Type)
  where dict (C' s) = Just Dict

instance Typed sub => Typed (sub :|| pred)
  where dict (C' s) = dict s

instance Typed sub => Typed (sub :| p)
  where dict (C s) = dict s

instance Typed (ArgConstr sub Type)
  where dict (ArgConstr s) = Nothing

instance (Typed sub, Typed sup) => Typed (sub :+: sup)
  where dict (InjL s) = dict s
        dict (InjR s) = dict s

instance (Typed sub) => Typed (Decor info sub)
  where dict (Decor _ s) = dict s

instance Typed Empty
  where dict _ = Nothing

instance Typed dom
  where dict _ = Nothing

typeDict :: Typed dom => ASTF dom a -> Maybe (Dict (Type a))
typeDict = simpleMatch (const . dict)

