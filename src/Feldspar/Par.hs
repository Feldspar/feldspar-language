--
-- Copyright (c) 2009-2011, ERICSSON AB
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

{-# LANGUAGE FlexibleContexts #-}

module Feldspar.Par
  ( P
  , IVar
  , runPar
  , new
  , get
  , put
  , fork
  , yield
  , spawn
  , pval
  , parMap
  , parMapM
  , divConq
  )
where
import Feldspar.Core.Frontend (Syntax)
import Feldspar.Core.Language hiding (pval)
import Feldspar.Core.Representation

runPar :: Syntax a => P a -> a
runPar = sugarSym1 ParRun

new :: Syntax a => P (IVar a)
new = sugarSym0 ParNew

get :: Syntax a => IVar a -> P a
get = sugarSym1 ParGet

put :: Syntax a => IVar a -> a -> P ()
put = sugarSym2 ParPut

fork :: P () -> P ()
fork = sugarSym1 ParFork

yield :: P ()
yield = sugarSym0 ParYield

spawn :: Syntax a => P a -> P (IVar a)
spawn p = do
    r <- new
    fork (p >>= put r)
    return r

pval :: Syntax a => a -> P (IVar a)
pval a = spawn (return a)

parMap :: Syntax b => (a -> b) -> [a] -> P [b]
parMap f xs = mapM (pval . f) xs >>= mapM get

parMapM :: Syntax b => (a -> P b) -> [a] -> P [b]
parMapM f xs = mapM (spawn . f) xs >>= mapM get

divConq :: Syntax b => (a -> Bool) -> (a -> [a]) -> ([b] -> b) -> (a -> b) -> a -> P b
divConq indiv split join f = go
  where
    go prob | indiv prob = return (f prob)
            | otherwise  = do
                sols <- parMapM go (split prob)
                return (join sols)
