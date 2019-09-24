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

pushLets :: AUntypedFeld a -> AUntypedFeld a
pushLets = go
  where go (AIn r (App Let t [rhs, AIn r1 (Lambda v body)])) = push v (go rhs) r1 (go body)
        go (AIn r (App op t es)) = AIn r $ App op t $ map go es
        go (AIn r (Lambda v e)) = AIn r $ Lambda v $ go e
        go e = e

data OCount = OC {low, high :: Int}
  deriving (Eq, Show)

push :: Var -> AUntypedFeld a -> a -> AUntypedFeld a -> AUntypedFeld a
push v rhs@(AIn _ eRhs) r = snd . goA False False
  where goA lo pa (AIn r1 e) = (norm lo n1, AIn r1 $ if ph && not lo then eB else e1)
           where (n1,e1) = go lo (pa || ph) e
                 ph = not pa && high n1 > 1
                 eB = App Let (typeof $ AIn r1 e) [rhs, AIn r $ Lambda v $ AIn r1 e1]

        go lo pa e@(Variable u) = if u /= v then (OC 0 0, e)
                                            else (OC 1 1, if pa then e else eRhs)
        go lo pa e@(Literal _) = (OC 0 0, e)
        go lo pa (App Condition t [ec, et, ee]) = (n, App Condition t [ec1, et1, ee1])
           where (nc,ec1) = goA False pa ec
                 (nt,et1) = goA False pa et
                 (ne,ee1) = goA False pa ee
                 n = both nc (oneOf nt ne)
        go lo pa (App op t es) = (n, App op t es1)
           where (ns,es1) = unzip $ map (goA lo1 pa) es
                 n = foldr both (OC 0 0) ns
                 lo1 = elem op [Parallel, Sequential, EparFor, ForLoop, WhileLoop, For, While]
        go lo pa e@(Lambda u _) | u == v = (OC 0 0, e)
        go lo pa (Lambda u e) = (n, Lambda u e1)
           where (n1,e1) = goA False pa e
                 n = if lo then -- oneOf n1 $ OC 0 0
                                both n1 n1 -- A loop body may be run several times
                           else n1

both, oneOf :: OCount -> OCount -> OCount
both (OC ll hl) (OC lr hr) = OC (ll+lr) (hl+hr)
oneOf (OC ll hl) (OC lr hr) = OC (min ll lr) (max hl hr)

norm :: Bool -> OCount -> OCount
norm lo (OC l h) = if lo then OC l h else OC (min 1 l) (min 1 h)
