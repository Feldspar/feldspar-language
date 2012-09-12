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

module Feldspar.Algorithm.CRC where

import qualified Prelude

import Feldspar
import Feldspar.Vector

tstBit :: Bits a => Data a -> Data Index -> Data Bool
tstBit w b = w .&. (1 .<<. b) /= 0

makeCrcTable :: (Bits a) => Data a -> Vector1 a
makeCrcTable polynomial = indexed 256 $ \i -> forLoop 8 (i2n i .<<. (sz - 8)) step
  where
    sz       = bitSize polynomial
    step _ r = let r' = r .<<. 1
               in condition (tstBit r (sz-1)) (r' `xor` polynomial) r'

-- | Calculate the normal form CRC using a table
crcNormal :: (Bits a)
          => Vector1 a -> Data a -> Vector1 Word8 -> Data a
crcNormal table initial xs = fold step initial xs
  where
    sz         = bitSize initial
    step crc a = (table ! i2n ((i2n (crc .>>. (sz - 8)) .&. 0xFF) `xor` a)) `xor` (crc .<<. 8)

-- | Calculate the reflected form CRC using a table
-- needs reflected tables
crcReflected :: (Bits a)
             => Vector1 a -> Data a -> Vector1 Word8 -> Data a
crcReflected table = fold step
  where
    step crc a = (table ! i2n ((crc `xor` i2n a) .&. 0xFF)) `xor` (crc .>>. 8)

-- | Calculate normal form CRC from a polynominal
crcNaive :: (Bits a) => Data a -> Data a -> Vector1 Word8 -> Data a
crcNaive = crcNormal . makeCrcTable

-- Future work
--
data CRC a = CRC { name       :: String
                 , width      :: Index
                 , poly       :: a
                 , init       :: a
                 , reflectIn  :: Bool
                 , reflectOut :: Bool
                 , xorOut     :: a
                 }

crc16 :: CRC (Data Word16)
crc16 = CRC "CRC-16" 16 0x8005 0x0000 True True 0x0000

-- | Reflect the bottom b bits of value t
reflect :: (Bits a) => Data a -> Data Length -> Data a
reflect t b = forLoop b t $ \i v -> let mask = bit ((b-1)-i) in condition (testBit t i) (v .|. mask) (v .&. complement mask)

-- References
-- The functions in this module are inspired by the follow guide
-- http://www.ross.net/crc/download/crc_v3.txt

