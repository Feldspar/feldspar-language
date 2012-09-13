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

module Feldspar.Core.Frontend.Bits
where

import Prelude hiding (Integral(..))

import Data.Int
import Data.Word

import Language.Syntactic

import Feldspar.Range
import Feldspar.Core.Types
import Feldspar.Core.Constructs
import Feldspar.Core.Constructs.Bits
import Feldspar.Core.Frontend.Integral
import Feldspar.Core.Frontend.Literal

import qualified Data.Bits as B

infixl 5 .<<.,.>>.
infixl 4 ⊕

class (Type a, B.Bits a, Integral a, Bounded a, Size a ~ Range a) => Bits a
  where
    -- Logical operations
    (.&.)         :: Data a -> Data a -> Data a
    (.&.)         = sugarSymC BAnd
    (.|.)         :: Data a -> Data a -> Data a
    (.|.)         = sugarSymC BOr
    xor           :: Data a -> Data a -> Data a
    xor           = sugarSymC BXor
    complement    :: Data a -> Data a
    complement    = sugarSymC Complement

    -- Bitwise operations
    bit           :: Data Index -> Data a
    bit           = sugarSymC Bit
    setBit        :: Data a -> Data Index -> Data a
    setBit        = sugarSymC SetBit
    clearBit      :: Data a -> Data Index -> Data a
    clearBit      = sugarSymC ClearBit
    complementBit :: Data a -> Data Index -> Data a
    complementBit = sugarSymC ComplementBit
    testBit       :: Data a -> Data Index -> Data Bool
    testBit       = sugarSymC TestBit

    -- Movement operations
    shiftLU       :: Data a -> Data Index -> Data a
    shiftLU       = sugarSymC ShiftLU
    shiftRU       :: Data a -> Data Index -> Data a
    shiftRU       = sugarSymC ShiftRU
    shiftL        :: Data a -> Data IntN -> Data a
    shiftL        = sugarSymC ShiftL
    shiftR        :: Data a -> Data IntN -> Data a
    shiftR        = sugarSymC ShiftR
    rotateLU      :: Data a -> Data Index -> Data a
    rotateLU      = sugarSymC RotateLU
    rotateRU      :: Data a -> Data Index -> Data a
    rotateRU      = sugarSymC RotateRU
    rotateL       :: Data a -> Data IntN -> Data a
    rotateL       = sugarSymC RotateL
    rotateR       :: Data a -> Data IntN -> Data a
    rotateR       = sugarSymC RotateR
    reverseBits   :: Data a -> Data a
    reverseBits   = sugarSymC ReverseBits

    bitScan       :: Data a -> Data Index
    bitScan       = sugarSymC BitScan
    bitCount      :: Data a -> Data Index
    bitCount      = sugarSymC BitCount

    bitSize       :: Data a -> Data Index
    bitSize       = value . bitSize'

    bitSize'      :: Data a -> Index
    bitSize'      = const $ fromIntegral $ B.bitSize (undefined :: a)
    isSigned      :: Data a -> Data Bool
    isSigned      = sugarSymC IsSigned

(⊕)    :: (Bits a) => Data a -> Data a -> Data a
(⊕)    =  xor
(.<<.) :: (Bits a) => Data a -> Data Index -> Data a
(.<<.) =  shiftLU
(.>>.) :: (Bits a) => Data a -> Data Index -> Data a
(.>>.) =  shiftRU

instance Bits Word8
instance Bits Word16
instance Bits Word32
instance Bits Word64
instance Bits WordN
instance Bits Int8
instance Bits Int16
instance Bits Int32
instance Bits Int64
instance Bits IntN

