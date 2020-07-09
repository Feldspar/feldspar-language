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

module Feldspar.Core.Middleend.CreateTasks ( createTasks ) where

import Control.Monad.State

import Feldspar.Compiler.Options (Options(..), Target(..), inTarget)
import Feldspar.Core.UntypedRepresentation

-- | Create tasks from MkFuture and similar constructs.
-- Invariant: There are no MkFuture, ParFork or NoInline constructs in the output.
createTasks :: Options -> UntypedFeld -> UntypedFeld
createTasks opts e = evalState (go opts e) 0

go :: Options -> UntypedFeld -> State Integer UntypedFeld
go _   e@(In Variable{}) = return e
go env (In (Lambda v e)) = do
  e' <- go env e
  return $ In (Lambda v e')
go env (In (LetFun (s, k, e1) e2))
 = liftM2 (\e1' e2' -> In (LetFun (s, k, e1') e2')) (go env e1) (go env e2)
go _   l@(In Literal{}) = return l
go env (In (App p _ [e])) | p `elem` [MkFuture, ParFork] = do
  p'' <- go env p'
  i <- freshId
  let taskName = "task" ++ show i
      core = "task_core" ++ show i
      k = if p == MkFuture then Future else Par
  return $ In (LetFun (core, k, p'') (In (App (Call k taskName) t' vs')))
   where vs = fv e
         vs' = map (In . Variable) vs
         p' = mkLam vs e
         t' = FValType $ typeof e
go env (In (App NoInline _ [p])) = do
  p'' <- go env p'
  i <- freshId
  let name = "noinline" ++ show i
  return $ In (LetFun (name, None, p'') (In (App (Call None name) t' vs')))
   where vs = fv p
         vs' = map (In . Variable) vs
         p' = mkLam vs p
         t' = typeof p
go env (In (App Parallel t [l, e@(In (Lambda v body))])) | Wool `inTarget` env = do
  p'' <- go env p'
  i <- freshId
  let name  = "wool" ++ show i
      body' = In (Lambda v (In (App (Call Loop name) t' $ tail vs')))
  return $ In (LetFun (name, Loop, p'') (In (App Parallel t [l,body'])))
   where vs  = v:fv e -- Make sure index is outermost parameter.
         vs' = map (In . Variable) vs
         p'  = mkLam vs body
         t'  = typeof body
go env (In (App EparFor t [l, e@(In (Lambda v body))])) | Wool `inTarget` env = do
  p'' <- go env p'
  i <- freshId
  let name  = "wool" ++ show i
      body' = In (Lambda v (In (App (Call Loop name) t' $ tail vs')))
  return $ In (LetFun (name, Loop, p'') (In (App EparFor t [l, body'])))
   where vs  = v:fv e
         vs' = map (In . Variable) vs
         p'  = mkLam vs body
         t'  = typeof body
go env (In (App p t es)) = do
  es' <- mapM (go env) es
  return $ In (App p t es')

freshId :: State Integer Integer
freshId = do
   i <- get
   put (i + 1)
   return i
