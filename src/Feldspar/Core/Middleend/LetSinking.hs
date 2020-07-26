{-# OPTIONS_GHC -Wall #-}

module Feldspar.Core.Middleend.LetSinking ( sinkLets ) where

import Feldspar.Compiler.Options (Options, Target(..), inTarget)
import Feldspar.Core.UntypedRepresentation

-- | Sink lets that are stuck between two lambdas.
-- Necessary invariant: lambdas can only appear in special places.
--
sinkLets :: Options -> UntypedFeld a -> UntypedFeld a
sinkLets opts = collectAtTop opts . go
  where go e@(In _ Variable{}) = e
        go (In r (Lambda v e))
         | (bs1, In r' (Lambda v' body)) <- collectLetBinders e
         , not $ null bs1
         = In r (Lambda v $ go (In r' (Lambda v' $ mkLets (bs1, body))))
        go (In r (Lambda v e)) = In r (Lambda v (go e))
        go (In r (LetFun (s, k, e1) e2)) = In r (LetFun (s, k, go e1) (go e2))
        go l@(In _ Literal{}) = l
        go (In r (App Let t [e1, In r' (Lambda x e2)]))
         = In r (App Let t [go e1, In r' (Lambda x $ go e2)])
        go (In r (App p t es)) = In r (App p t $ map go es)

-- | Converts let x = .. in .. \x2 -> e to \x2 -> let x = .. in e
--   for the top level expression when BA is a target.
collectAtTop :: Options -> UntypedFeld a -> UntypedFeld a
collectAtTop opts e
  | BA `inTarget` opts
  , (bs, e1) <- collectLetBinders e -- Get outermost let bindings
  , not $ null bs
  , (vs, body) <- collectBinders e1 -- Get all lambdas immediately within
  , not $ null vs
  = mkLam vs $ mkLets (bs, body)
  | otherwise = e
