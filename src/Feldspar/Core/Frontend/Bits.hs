{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

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
import qualified Prelude as P

import Data.Int
import Data.Word

import Feldspar.Range (Range(..), UnsignedRep)
import Feldspar.Core.Types
import Feldspar.Core.Constructs
import Feldspar.Core.Constructs.Bits
import Feldspar.Core.Frontend.Integral
import Feldspar.Core.Frontend.Literal

import qualified Data.Bits as B

infixl 5 .<<.,.>>.
infixl 4 ⊕

class (Integral a) => Bits a
  where
    -- * Logical operations
    (.&.)         :: a -> a -> a
    (.|.)         :: a -> a -> a
    xor           :: a -> a -> a
    complement    :: a -> a

    -- * Bitwise operations
    bit           :: Data Index -> a
    setBit        :: a -> Data Index -> a
    clearBit      :: a -> Data Index -> a
    complementBit :: a -> Data Index -> a
    testBit       :: a -> Data Index -> Data Bool

    -- * Movement operations
    shiftLU       :: a -> Data Index -> a
    shiftRU       :: a -> Data Index -> a
    shiftL        :: a -> Data IntN -> a
    shiftR        :: a -> Data IntN -> a
    rotateLU      :: a -> Data Index -> a
    rotateRU      :: a -> Data Index -> a
    rotateL       :: a -> Data IntN -> a
    rotateR       :: a -> Data IntN -> a
    reverseBits   :: a -> a

    -- * Query operations
    bitScan       :: a -> Data Index
    bitCount      :: a -> Data Index

    bitSize       :: a -> Data Index
    bitSize'      :: a -> Index

    isSigned      :: a -> Data Bool
    isSigned'     :: a -> Bool

instance (Type a, B.Bits a, B.Bits (UnsignedRep a), Size a ~ Range a,
          Ord a, Ord (UnsignedRep a), Num a, Num (UnsignedRep a),
          P.Integral a, P.Integral (UnsignedRep a),
          Bounded a, Bounded (UnsignedRep a)) =>
         Bits (Data a) where
    (.&.)         = sugarSymF BAnd
    (.|.)         = sugarSymF BOr
    xor           = sugarSymF BXor
    complement    = sugarSymF Complement
    bit           = sugarSymF Bit
    setBit        = sugarSymF SetBit
    clearBit      = sugarSymF ClearBit
    complementBit = sugarSymF ComplementBit
    testBit       = sugarSymF TestBit
    shiftLU       = sugarSymF ShiftLU
    shiftRU       = sugarSymF ShiftRU
    shiftL        = sugarSymF ShiftL
    shiftR        = sugarSymF ShiftR
    rotateLU      = sugarSymF RotateLU
    rotateRU      = sugarSymF RotateRU
    rotateL       = sugarSymF RotateL
    rotateR       = sugarSymF RotateR
    reverseBits   = sugarSymF ReverseBits
    bitScan       = sugarSymF BitScan
    bitCount      = sugarSymF BitCount
    bitSize       = value . bitSize'
    bitSize'      = const $ fromIntegral $ finiteBitSize (undefined :: a)
    isSigned      = value . isSigned'
    isSigned'     = const $ B.isSigned (undefined :: a)

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708
finiteBitSize :: (B.FiniteBits b) => b -> Int
finiteBitSize = B.finiteBitSize
#else
finiteBitSize :: (B.Bits b) => b -> Int
finiteBitSize = B.bitSize
#endif

-- * Combinators

(⊕)    :: (Bits a) => a -> a -> a
(⊕)    =  xor
(.<<.) :: (Bits a) => a -> Data Index -> a
(.<<.) =  shiftLU
(.>>.) :: (Bits a) => a -> Data Index -> a
(.>>.) =  shiftRU

-- | Set all bits to one
allOnes :: Bits a => a
allOnes = complement 0

-- | Set the `n` lowest bits to one
oneBits :: Bits a => Data Index -> a
oneBits n = complement (allOnes .<<. n)

-- | Extract the `k` lowest bits
lsbs :: Bits a => Data Index -> a -> a
lsbs k i = i .&. oneBits k

