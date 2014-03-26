{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Feldspar.Core.Constructs.Elements where

import Language.Syntactic

import Feldspar.Lattice (universal)
import Feldspar.Core.Types
import Feldspar.Core.Interpretation
import Feldspar.Core.Constructs.Binding
import Data.List (genericTake, sortBy)
import Data.Function (on)

data ElementsFeat a
  where
    EMaterialize :: Type a => ElementsFeat (Length :-> Elements a :-> Full [a])
    EWrite       :: Type a => ElementsFeat (Index :-> a :-> Full (Elements a))
    ESkip        :: Type a => ElementsFeat (Full (Elements a))
    EPar         :: Type a => ElementsFeat (Elements a :-> Elements a :-> Full (Elements a))
    EparFor        :: Type a => ElementsFeat (Length :-> (Index -> Elements a) :-> Full (Elements a))

instance Semantic ElementsFeat
  where
    semantics EMaterialize    = Sem "materialize" ematerialize
    semantics EWrite          = Sem "write" (\ix e -> Elements [(ix, e)])
    semantics ESkip           = Sem "skip" (Elements [])
    semantics EPar            = Sem "par" (\(Elements l) (Elements r) -> Elements (l ++ r))
    semantics EparFor         = Sem "parFor" eparFor

ematerialize :: Length -> Elements a -> [a]
ematerialize l (Elements xs) = map snd xs'
  where xs' = genericTake l $ sortBy (compare `on` fst) xs

eparFor :: Length -> (Index -> Elements a) -> Elements a
eparFor len ixf = Elements $ concatMap (\(Elements vs) -> vs) xs
      where xs = genericTake len $ map ixf [0..]

semanticInstances ''ElementsFeat

instance EvalBind ElementsFeat where evalBindSym = evalBindSymDefault

instance AlphaEq dom dom dom env => AlphaEq ElementsFeat ElementsFeat dom env
  where
    alphaEqSym = alphaEqSymDefault

instance Sharable ElementsFeat

instance Monotonic ElementsFeat

instance SizeProp (ElementsFeat :|| Type)
  where
    sizeProp (C' EMaterialize) (WrapFull len :* WrapFull arr :* Nil) = infoSize arr
    sizeProp (C' EWrite)       _                                     = universal
    sizeProp (C' ESkip)        _                                     = universal
    sizeProp (C' EPar)         (WrapFull p1 :* WrapFull p2 :* Nil)   = universal -- TODO: p1 U p2
    sizeProp (C' EparFor)        _                                   = universal

instance ( (ElementsFeat :|| Type) :<: dom
         , OptimizeSuper dom
         )
      => Optimize (ElementsFeat :|| Type) dom
  where
    constructFeatUnOpt opts x@(C' _) = constructFeatUnOptDefault opts x
