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
         = In (Lambda v (In (Lambda v' (go $ mkLets (bs1, body)))))
        go (In (Lambda v e)) = In (Lambda v (go e))
        go (In (Let e1 (In (Lambda v e2)))) = In (Let (go e1) (In (Lambda v (go e2))))
        go (In (ForeignImport s t es)) = In (ForeignImport s t (map go es))
        go l@(In Literal{}) = l
        go (In (Tup2 e1 e2)) = In (Tup2 (go e1) (go e2))
        go (In (Tup3 e1 e2 e3)) = In (Tup3 (go e1) (go e2) (go e3))
        go (In (Tup4 e1 e2 e3 e4)) = In (Tup4 (go e1) (go e2) (go e3) (go e4))
        go (In (Tup5 e1 e2 e3 e4 e5)) = In (Tup5 (go e1) (go e2) (go e3) (go e4) (go e5))
        go (In (Tup6 e1 e2 e3 e4 e5 e6)) = In (Tup6 (go e1) (go e2) (go e3) (go e4) (go e5) (go e6))
        go (In (Tup7 e1 e2 e3 e4 e5 e6 e7)) = In (Tup7 (go e1) (go e2) (go e3) (go e4) (go e5) (go e6) (go e7))
        go (In (App p t es)) = In (App p t $ map go es)
