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

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Feldspar.Core.Frontend.Switch where

import qualified Prelude as P

import Language.Syntactic (resugar)

import Feldspar.Core.Types
import Feldspar.Core.Frontend.Eq (Eq((==)))
import Feldspar.Core.Frontend.Array (parallel,append)
import Feldspar.Core.Frontend.Literal (value)
import Feldspar.Core.Frontend.Condition ((?))
import Feldspar.Core.Constructs
import Feldspar.Core.Constructs.Switch

-- | Select between the cases based on the value of the scrutinee.
select :: (Eq a, Syntax b) => Data a -> [(Data a, b)] -> b -> b
select s cs def = P.foldr (\(c,a) b -> c == s ? a P.$ b) def cs

{-# DEPRECATED select "select will generate a tree of if-statements. Use switch instead" #-}

-- | Select between the cases based on the value of the scrutinee.
switch :: (Eq a, Syntax b)
       => Data a -> b -> [(Data a, b)] -> b
switch s def [] = def
switch s def cs = sugarSymF Switch (P.foldr (\(c,a) b -> c == s ? a P.$ b) def cs)

