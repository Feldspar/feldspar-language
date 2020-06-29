{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

module Feldspar.Core.Eval (eval) where

import Feldspar.Core.Representation
import Feldspar.Core.Semantics

import qualified Data.Map.Strict as M
import Data.Typeable

data Closure where
  Clo :: Typeable a => a -> Closure

-- | Evaluate an expression
eval :: AExpr a -> a
eval = evalA M.empty

evalA :: CloEnv -> AExpr a -> a
evalA bm (_ :& e) = evalE bm e

evalE :: Typeable a => CloEnv -> Expr a -> a
evalE bm (Sym (Variable v)) = lookupCE "Eval.evalE" bm v
evalE bm (Sym (Lambda (Var n _)) :@ e) = \x -> evalA (M.insert n (Clo x) bm) e
evalE bm (Sym op) = semSem $ semantics op
evalE bm (f :@ e) = evalE bm f $ evalA bm e

type CloEnv = M.Map VarId Closure

lookupCE :: Typeable a => String -> CloEnv -> Var a -> a
lookupCE msg bm (v@(Var n _) :: Var a)
               = case M.lookup n bm of
                      Nothing -> error $ msg ++ ": lookupCE does not find variable " ++ show v
                      Just (Clo (x :: b))
                           -> case eqT :: Maybe (a :~: b) of
                                   Nothing -> error $ msg ++ ": lookupCE finds conflicing types for " ++ show v
                                   Just Refl -> x
