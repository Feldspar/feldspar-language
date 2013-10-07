{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}

-- | Witness 'Type' constraints

module Feldspar.Core.Interpretation.Typed
  ( Typed(..)
  , typeDict
  )
where

import Language.Syntactic
import Language.Syntactic.Constructs.Decoration

import Feldspar.Core.Types (Type)

-- | Class representing a possible dictionary to witness a 'Type'
-- constraint.
class Typed dom
  where
    typeDictSym :: dom a -> Maybe (Dict (Type (DenResult a)))
    typeDictSym _ = Nothing

instance Typed (sub :|| Type)
  where typeDictSym (C' _) = Just Dict

instance Typed sub => Typed (sub :|| pred)
  where typeDictSym (C' s) = typeDictSym s

instance Typed sub => Typed (sub :| pred)
  where typeDictSym (C s) = typeDictSym s

instance Typed (SubConstr2 c sub Type Top)
  where typeDictSym (SubConstr2 _) = Nothing

instance (Typed sub, Typed sup) => Typed (sub :+: sup)
  where typeDictSym (InjL s) = typeDictSym s
        typeDictSym (InjR s) = typeDictSym s

instance (Typed sub) => Typed (Decor info sub)
  where typeDictSym (Decor _ s) = typeDictSym s

instance Typed Empty
instance Typed dom

-- | Extract a possible 'Type' constraint witness from an 'AST'
typeDict :: Typed dom => ASTF dom a -> Maybe (Dict (Type a))
typeDict = simpleMatch (const . typeDictSym)

