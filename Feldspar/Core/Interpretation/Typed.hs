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
import Language.Syntactic.Constructs.Decoration

import Feldspar.Core.Types (Type)

class Typed dom
  where
    dict :: dom a -> Maybe (Dict (Type (DenResult a)))

instance Typed (sub :||| Type)
  where dict (C'' _) = Just Dict

instance Typed sub => Typed (sub :|| p)
  where dict (C'  s) = dict s

instance (Typed sub, Typed sup) => Typed (sub :+: sup)
  where dict (InjL s) = dict s
        dict (InjR s) = dict s

instance (Typed sub) => Typed (Decor info sub)
  where dict (Decor _ s) = dict s

instance Typed dom
  where dict _ = Nothing

typeDict :: Typed dom => ASTF dom a -> Maybe (Dict (Type a))
typeDict = simpleMatch (flip $ const dict)

data (expr :||| pred) sig
  where
    C'' :: pred (DenResult sig) => expr sig -> (expr :||| pred) sig

infixl 4 :|||

instance Equality dom => Equality (dom :||| pred)
  where
    equal (C'' a) (C'' b) = equal a b
    exprHash (C'' a)     = exprHash a

instance Render dom => Render (dom :||| pred)
  where
    renderArgs args (C'' a) = renderArgs args a

instance Eval dom => Eval (dom :||| pred)
  where
    evaluate (C'' a) = evaluate a

instance ToTree dom => ToTree (dom :||| pred)
  where
    toTreeArgs args (C'' a) = toTreeArgs args a

instance EvalBind dom => EvalBind (dom :||| pred)
  where
    evalBindSym (C'' a) = evalBindSym a

instance AlphaEq sub sub dom env => AlphaEq (sub :||| pred) (sub :||| pred) dom env
  where
    alphaEqSym (C'' a) aArgs (C'' b) bArgs = alphaEqSym a aArgs b bArgs

instance Constrained (dom :||| pred)
  where
    type Sat (dom :||| pred) = pred
    exprDict (C'' s) = Dict

