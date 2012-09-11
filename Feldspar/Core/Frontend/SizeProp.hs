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

-- | The functions in this module can be used to help size inference (which, in
-- turn, helps deriving upper bounds of array sizes and helps optimization).

module Feldspar.Core.Frontend.SizeProp where



import Language.Syntactic

import Feldspar.Range
import Feldspar.Core.Types
import Feldspar.Core.Constructs
import Feldspar.Core.Constructs.SizeProp
import Feldspar.Core.Frontend.Literal



-- | An identity function affecting the abstract size information used during
-- optimization. The application of a 'SizeCap' is a /guarantee/ (by the caller)
-- that the argument is within a certain size (determined by the creator of the
-- 'SizeCap', e.g. 'sizeProp').
--
-- /Warning: If the guarantee is not fulfilled, optimizations become unsound!/
--
-- In general, the size of the resulting value is the intersection of the cap
-- size and the size obtained by ordinary size inference. That is, a 'SizeCap'
-- can only make the size more precise, not less precise.
type SizeCap a = Data a -> Data a

-- | @sizeProp prop a b@: A guarantee that @b@ is within the size @(prop sa)@,
-- where @sa@ is the size of @a@.
sizeProp :: (Syntax a, Type b) =>
    (Size (Internal a) -> Size b) -> a -> SizeCap b
sizeProp = sugarSym . PropSize

-- | A guarantee that the argument is within the given size
cap :: Type a => Size a -> SizeCap a
cap sz = sizeProp (const sz) (Data $ desugar ())

-- | @notAbove a b@: A guarantee that @b <= a@ holds
notAbove :: (Type a, Bounded a, Size a ~ Range a) => Data a -> SizeCap a
notAbove = sizeProp (Range minBound . upperBound)

-- | @notBelow a b@: A guarantee that @b >= a@ holds
notBelow :: (Type a, Bounded a, Size a ~ Range a) => Data a -> SizeCap a
notBelow = sizeProp (flip Range maxBound . lowerBound)

-- | @between l u a@: A guarantee that @l <= a <= u@ holds
between :: (Type a, Bounded a, Size a ~ Range a) =>
    Data a -> Data a -> SizeCap a
between l u = notBelow l . notAbove u

