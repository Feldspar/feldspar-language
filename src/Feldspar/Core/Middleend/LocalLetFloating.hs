module Feldspar.Core.Middleend.LocalLetFloating (localLetFloating) where

import Feldspar.Core.UntypedRepresentation
import Feldspar.Core.Interpretation (FeldOpts)

-- | Float let bindings as far up as possible but not out of lambdas
--   or the branches of conditionals.
localLetFloating :: FeldOpts -> AUntypedFeld a -> AUntypedFeld a
localLetFloating opts = mkLets . go
  where go (AIn r (App Let t [eRhs, AIn r' (Lambda v eBody)]))
           = let (bsB,eBody1) = go eBody
                 (bsR,eRhs1) = go eRhs
                 bsOut = bsR ++ [(v, eRhs1)] ++ bsB
              in (bsOut, eBody1)
        go (AIn r (App Condition t [ec, et, ee]))
           = let (bs,ec1) = go ec
                 es = [ec1, mkLets $ go et, mkLets $ go ee]
              in (bs, AIn r $ App Condition t es)
        go (AIn r (App f t es))
           = let (bss,es1) = unzip $ map go es
              in (concat bss, AIn r $ App f t es1)
        go (AIn r (Lambda v e)) = ([], AIn r $ Lambda v $ mkLets $ go e)
        go e = ([],e)
