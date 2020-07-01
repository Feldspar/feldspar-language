{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
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

module Feldspar.Algorithm.FFT
    ( fft
    , ifft
    )
  where

import qualified Prelude as P

import Feldspar
import Feldspar.Vector hiding (riffle)

-- | Radix-2 Decimation-In-Frequeny Fast Fourier Transformation of the given complex vector
--   The given vector must be power-of-two sized, (for example 2, 4, 8, 16, 32, etc.)
fft :: Pull1 (Complex Float) -> Pull1 (Complex Float)
fft v = bitRev steps $ fftCore steps v
    where steps = ilog2 (length v) - 1

-- | Radix-2 Decimation-In-Frequeny Inverse Fast Fourier Transformation of the given complex vector
--   The given vector must be power-of-two sized, (for example 2, 4, 8, 16, 32, etc.)
ifft :: Pull1 (Complex Float) -> Pull1 (Complex Float)
ifft v = bitRev steps $ ifftCore steps v
    where steps = ilog2 (length v) - 1

fftCore :: Data Index -> Pull1 (Complex Float) -> Pull1 (Complex Float)
fftCore n = composeOn stage (reverse (0...n))
  where
    stage k vec = indexed1 (length vec) ixf
      where
        ixf i = condition (testBit i k) (twid * (b - a)) (a+b)
          where
            a    = vec !! i
            b    = vec !! (i `xor` k2)
            twid = cis (-pi * i2f (lsbs k i) / i2f k2)
            k2   = 1 .<<. k

ifftCore :: Data Index -> Pull1 (Complex Float) -> Pull1 (Complex Float)
ifftCore n = map (/ complex (i2f (2^(n+1))) 0) . composeOn stage (reverse (0...n))
  where
    stage k vec = indexed1 (length vec) ixf
      where
        ixf i = condition (testBit i k) (twid * (b - a)) (a+b)
          where
            a    = vec !! i
            b    = vec !! (i `xor` k2)
            twid = cis (pi * i2f (lsbs k i) / i2f k2)
            k2   = 1 .<<. k

bitRev :: Type a => Data Index -> Pull1 a -> Pull1 a
bitRev n = composeOn riffle (1...n)

riffle :: Syntax a => Data Index -> Pull DIM1 a -> Pull DIM1 a
riffle k = permute (const $ rotBit k)

-- Helper functions
composeOn :: (Syntax a) => (b -> a -> a) -> Pull DIM1 b -> a -> a
composeOn f v i = fromZero $ fold (flip f) i v

rotBit :: Data Index -> Data Index -> Data Index
rotBit k i = lefts .|. rights
  where
    ir = i .>>. 1
    rights = ir .&. oneBits k
    lefts  = (((ir .>>. k) .<<. 1) .|. (i .&. 1)) .<<. k
