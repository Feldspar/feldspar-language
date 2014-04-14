module Feldspar.Core.Middleend.OptimizeUntyped ( optimize ) where

import Feldspar.Core.UntypedRepresentation

-- | General simplification. Could in theory be done at earlier stages.
optimize :: UntypedFeld -> UntypedFeld
optimize = go

go :: UntypedFeld -> UntypedFeld
go e@(In Variable{}) = e
go (In (Lambda v e)) = In (Lambda v (go e))
go (In (LetFun (s, f, e1) e2)) = In (LetFun (s, f, go e1) (go e2))
go l@(In Literal{}) = l

-- For 1 (\v -> body) ==> [0/v]body
go (In (App For _ [(In (Literal (LInt s sz 1))), e2@(In (Lambda v body))]))
  = go $ subst (In (Literal (LInt s sz 0))) v body

-- Create a 1 element long array (frequent with MultiDim) and immediately select that
-- element. Can e seen in the metrics test in feldspar-compiler.
-- (RunMutableArray (Bind (NewArr_ 1)
--                        (\v3 -> Then (SetArr v3 0 e3) (Return v3)))) ! 0
go e@(In (App GetIx _ [arr, (In (Literal (LInt _ _ 0)))]))
 | (In (App RunMutableArray _ [In (App Bind _ [In (App NewArr_ _ [l]), e'])])) <- arr
 , (In (Literal (LInt _ _ 1))) <- l
 , (In (Lambda v1 (In (App Then _  [sarr, ret])))) <- e'
 , (In (App SetArr _ [In (Variable v3), In (Literal (LInt _ _ 0)), e3])) <- sarr
 , (In (App Return _ [In (Variable v2)])) <- ret
 , v1 == v2
 , v1 == v3 = go e3

go (In (App p t es)) = In (App p t $ map go es)
