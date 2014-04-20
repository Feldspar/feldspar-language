module Feldspar.Core.Middleend.LetSinking ( sinkLets ) where

import Feldspar.Core.UntypedRepresentation

-- | Sink lets that are stuck between two lambdas.
-- Necessary invariant: lambdas can only appear in special places.
sinkLets :: UntypedFeld -> UntypedFeld
sinkLets = go
  where go e@(In Variable{}) = e
        go (In (Lambda v e))
         | (bs1, In (Lambda v' body)) <- collectLetBinders e
         , not $ null bs1
         = In (Lambda v $ go (In (Lambda v' $ mkLets (bs1, body))))
        go (In (Lambda v e)) = In (Lambda v (go e))
        go (In (LetFun (s, k, e1) e2)) = In (LetFun (s, k, go e1) (go e2))
        go l@(In Literal{}) = l
        go (In (App Let t [e1, In (Lambda x e2)])) = In (App Let t [go e1, In (Lambda x $ go e2)])
        go (In (App p t es)) = In (App p t $ map go es)
