module Feldspar.Core.Middleend.LetSinking ( sinkLets ) where

import Feldspar.Core.UntypedRepresentation
import Feldspar.Core.Interpretation (FeldOpts, Target(..), inTarget)

-- | Sink lets that are stuck between two lambdas.
-- Necessary invariant: lambdas can only appear in special places.
--
sinkLets :: FeldOpts -> AUntypedFeld a -> AUntypedFeld a
sinkLets opts = collectAtTop opts . go
  where go e@(AIn _ Variable{}) = e
        go (AIn r (Lambda v e))
         | (bs1, AIn r' (Lambda v' body)) <- collectLetBinders e
         , not $ null bs1
         = AIn r (Lambda v $ go (AIn r' (Lambda v' $ mkLets (bs1, body))))
        go (AIn r (Lambda v e)) = AIn r (Lambda v (go e))
        go (AIn r (LetFun (s, k, e1) e2)) = AIn r (LetFun (s, k, go e1) (go e2))
        go l@(AIn _ Literal{}) = l
        go (AIn r (App Let t [e1, AIn r' (Lambda x e2)]))
         = AIn r (App Let t [go e1, AIn r' (Lambda x $ go e2)])
        go (AIn r (App p t es)) = AIn r (App p t $ map go es)

collectAtTop :: FeldOpts -> AUntypedFeld a -> AUntypedFeld a
collectAtTop opts e
  | BA `inTarget` opts
  , (bs, e1) <- collectLetBinders e -- Get outermost let bindings
  , not $ null bs
  , (vs, body) <- collectBinders e1 -- Get all lambdas immediately within
  , not $ null vs
  = mkLam' vs $ mkLets (bs, body)
  | otherwise = e
