--
-- Copyright (c) 2019, ERICSSON AB
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright notice,
--       this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in the
--       documentation and/or other materials provided with the distribution.
--     * Neither the name of the ERICSSON AB nor the names of its contributors
--       may be used to endorse or promote products derived from this software
--       without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

module Feldspar.Core.Middleend.PushLets (pushLets) where

import Feldspar.Core.UntypedRepresentation
import Feldspar.Core.Middleend.Constructors

pushLets :: AUntypedFeld a -> AUntypedFeld a
pushLets = toExpr . go
  where go (AIn r (App Let t [rhs, AIn r1 (Lambda v body)]))
           | legalToInline rhs = push v (go rhs) r1 (toExpr $ go body)
           | otherwise = mkBinds ([(v, go rhs)], go body)
        go (AIn r (App op t es)) = aIn r $ app op t $ map go es
        go (AIn r (Lambda v e)) = aIn r $ lambda v $ go e
        go e = fromExpr e

data OCount = OC {low, high :: Int}
  deriving (Eq, Show)

data DSCount = DS {dynamic :: OCount, static :: Int}
  deriving (Eq, Show)

push :: Var -> AExpB a -> a -> AUntypedFeld a -> AExpB a
push v rhs r = snd . goA False False
  where goA lo pa (AIn r1 e) = (norm n1, aIn r1 $ if ph then eB else e1)
           where (n1,e1) = go lo (pa || ph) e
                 d1 = dynamic n1
                 ph = not pa && (high d1 > 1 || low d1 > 0 && static n1 > 1)
                 eB = unAnnotateB $ mkBinds ([(v, rhs)], aIn r1 e1)

        go lo pa e@(Variable u) = if u /= v then (zeroDS, fromRExpr e)
                                            else (oneDS, if pa then fromRExpr e else unAnnotateB rhs)
        go lo pa e@(Literal _) = (zeroDS, fromRExpr e)
        go lo pa (App Condition t [ec, et, ee]) = (n, app Condition t [ec1, et1, ee1])
           where (nc,ec1) = goA False pa ec
                 (nt,et1) = goA False pa et
                 (ne,ee1) = goA False pa ee
                 n = liftDS both nc (liftDS oneOf nt ne)
        go lo pa (App op t es) = (n, app op t es1)
           where (ns,es1) = unzip $ map (goA lo1 pa) es
                 n = foldr (liftDS both) zeroDS ns
                 lo1 = elem op [Parallel, Sequential, EparFor, ForLoop, WhileLoop, For, While]
        go lo pa e@(Lambda u _) | u == v = (zeroDS, fromRExpr e)
        go lo pa (Lambda u e) = (lamDS lo n1, lambda u $ e1)
           where (n1,e1) = goA False pa e

lamDS :: Bool -> DSCount -> DSCount
lamDS lo (DS d s) = if lo then DS (both d d) (s+s) else DS d s -- A loop body may be run several times

zeroDS, oneDS :: DSCount
zeroDS = DS (OC 0 0) 0
oneDS = DS (OC 1 1) 1

liftDS :: (OCount -> OCount -> OCount) -> DSCount -> DSCount -> DSCount
liftDS df l r = DS (dynamic l `df` dynamic r) (static l + static r)

both, oneOf :: OCount -> OCount -> OCount
both (OC ll hl) (OC lr hr) = OC (ll+lr) (hl+hr)
oneOf (OC ll hl) (OC lr hr) = OC (min ll lr) (max hl hr)

norm :: DSCount -> DSCount
norm ds@(DS (OC l h) s) = DS (OC (min 1 l) (min 1 h)) (min s 1)
