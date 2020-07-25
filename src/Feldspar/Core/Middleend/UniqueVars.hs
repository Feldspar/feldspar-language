{-# OPTIONS_GHC -Wall #-}

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

module Feldspar.Core.Middleend.UniqueVars (uniqueVars) where

import Feldspar.Core.UntypedRepresentation

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Control.Monad.State

type U a = State (S.Set VarId) a
type RRExp a = UntypedFeldF (AUntypedFeld a)

{- | Ensure that each variable binding binds a unique variable.
     This invariant is import since some back ends declare all
     variables at top level.
-}

uniqueVars :: AUntypedFeld a -> AUntypedFeld a
uniqueVars e = evalState (uniqA M.empty e) S.empty

uniqA :: M.Map VarId (RRExp a) -> AUntypedFeld a -> U (AUntypedFeld a)
uniqA vm (AIn r e) = do e1 <- uniq vm e
                        return $ AIn r e1

uniq :: M.Map VarId (RRExp a) -> RRExp a -> U (RRExp a)
uniq vm (Variable v) = return $ vm M.! varNum v
uniq vm (App op t es) = do es1 <- mapM (uniqA vm) es
                           return $ App op t es1
uniq _ (Literal l) = return $ Literal l
uniq vm (Lambda v e) = do v1 <- record v
                          e1 <- uniqA (M.insert (varNum v) (Variable v1) vm) e
                          return $ Lambda v1 e1
uniq _ e = error $ "UniqueVars.uniq: unimplemented expression: " ++ show e

record :: Var -> U Var
record v = do s <- get
              let n = varNum v
                  m = head [i | i <- [n, n+10000 ..], S.notMember i s]
              put $ m `S.insert` s
              return v{varNum = m}
