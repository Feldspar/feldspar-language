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

-- | A module for /virtual vectors/. Many of the functions defined here are
-- imitations of Haskell's list operations, and to a first approximation they
-- behave accordingly.
--
-- A virtual vector is normally guaranteed not to be present in the generated
-- code. The only exceptions are:
--
--   * when it is explicitly forced using the functions 'force' or 'desugar'
--
--   * when it is the input or output of a program
--
--   * when it is accessed by a function outside the "Feldspar.SimpleVector" API, for
--     example, 'condition' or 'forLoop'
--
-- Note also that most operations only introduce a small constant overhead on
-- the vector. The exceptions are
--
--   * 'fold'
--
--   * 'fold1'
--
--   * Functions that introduce storage (see above)
--
--   * \"Folding\" functions: 'sum', 'maximum', etc.
--
-- These functions introduce overhead that is linear in the length of the
-- vector.

module Feldspar.SimpleVector
    ( module Feldspar.SimpleVector.Internal
    ) where



import Feldspar()  -- For Haddock
import Feldspar.SimpleVector.Internal hiding (freezeVector, scan)

