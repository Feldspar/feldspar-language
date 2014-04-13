module Feldspar.Core.Middleend.OptimizeUntyped ( optimize ) where

import Feldspar.Core.UntypedRepresentation

-- | General simplification. Could in theory be done at earlier stages.
optimize :: UntypedFeld -> UntypedFeld
optimize = go

go :: UntypedFeld -> UntypedFeld
go e@(In Variable{}) = e
go (In (Lambda v e)) = In (Lambda v (go e))
go (In (Let e1 (In (Lambda v e2)))) = In (Let (go e1) (In (Lambda v (go e2))))
go l@(In Literal{}) = l
go (In (App For _ [(In (Literal (LInt s sz 1))), e2@(In (Lambda _ body))]))
  = go $ In (App Bind (typeof body) [In (Literal (LInt s sz 0)), e2])
go (In (App p t es)) = In (App p t $ map go es)
