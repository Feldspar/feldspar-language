module Feldspar.Core.Middleend.OptimizeUntyped ( optimize ) where

import Feldspar.Core.UntypedRepresentation hiding (optimize)

-- | General simplification. Could in theory be done at earlier stages.
optimize :: UntypedFeld -> UntypedFeld
optimize = go

go :: UntypedFeld -> UntypedFeld
go e@(In Variable{}) = e
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
go e@(In PrimApp0{}) = e
go (In (PrimApp1 p t e)) = In (PrimApp1 p t (go e))
go (In (PrimApp2 For _ (In (Literal (LInt s sz 1))) e2@(In (Lambda _ body))))
  = go $ In (PrimApp2 Bind (typeof body) (In (Literal (LInt s sz 0))) e2)
go (In (PrimApp2 p t e1 e2)) = In (PrimApp2 p t (go e1) (go e2))
go (In (PrimApp3 p t e1 e2 e3)) = In (PrimApp3 p t (go e1) (go e2) (go e3))


