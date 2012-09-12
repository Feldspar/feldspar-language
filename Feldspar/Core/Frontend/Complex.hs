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

module Feldspar.Core.Frontend.Complex
where

import Data.Complex (Complex)

import Feldspar.Core.Constructs
import Feldspar.Core.Constructs.Complex
import Feldspar.Core.Frontend.Num

import Language.Syntactic

complex :: (Numeric a, RealFloat a) => Data a -> Data a -> Data (Complex a)
complex = sugarSym MkComplex

realPart :: (Numeric a, RealFloat a) => Data (Complex a) -> Data a
realPart = sugarSym RealPart

imagPart :: (Numeric a, RealFloat a) => Data (Complex a) -> Data a
imagPart = sugarSym ImagPart

conjugate :: (Numeric a, RealFloat a) => Data (Complex a) -> Data (Complex a)
conjugate = sugarSym Conjugate

mkPolar :: (Numeric a, RealFloat a) => Data a -> Data a -> Data (Complex a)
mkPolar = sugarSym MkPolar

cis :: (Numeric a, RealFloat a) => Data a -> Data (Complex a)
cis = sugarSym Cis

magnitude :: (Numeric a, RealFloat a) => Data (Complex a) -> Data a
magnitude = sugarSym Magnitude

phase :: (Numeric a, RealFloat a) => Data (Complex a) -> Data a
phase = sugarSym Phase

polar :: (Numeric a, RealFloat a) => Data (Complex a) -> (Data a, Data a)
polar c = (magnitude c, phase c)

infixl 6 +.

(+.) :: (Numeric a, RealFloat a) => Data a -> Data a -> Data (Complex a)
(+.) = complex

iunit :: (Numeric a, RealFloat a) => Data (Complex a)
iunit = 0 +. 1

