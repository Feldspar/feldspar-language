{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

{- | The ValueInfo module defines the types and basic operations for value set analysis.
     In this implementation, a value set is either

     * an inclusive range [lo,hi] wrapped in a constructor giving the type of elements in the range, or
     * a product (v1, ..., vk) of values denoting the cartesian product if the vi.
-}

module Feldspar.ValueInfo where

import Feldspar.Range
import Feldspar.Core.Types(IntN,WordN)

import Data.Int
import Data.Word

-- | The ValueInfo type wraps ranges of various types as well as cartesian
--   products into a universal domain.
data ValueInfo = VIBool   (Range Int) -- ^ We represent False as 0 and True as 1.
               | VIInt8   (Range Int8)
               | VIInt16  (Range Int16)
               | VIInt32  (Range Int32)
               | VIInt64  (Range Int64)
               | VIIntN   (Range IntN)
               | VIWord8  (Range Word8)
               | VIWord16 (Range Word16)
               | VIWord32 (Range Word32)
               | VIWord64 (Range Word64)
               | VIWordN  (Range WordN)
               | VIFloat  -- (Range Float)
               | VIDouble -- (Range Double)
               | VIProd [ValueInfo]
               deriving (Eq)

instance Show ValueInfo where
  show (VIBool l)   = "VIBool"
  show (VIInt8 l)   = "VIInt8"
  show (VIInt16 l)  = "VIInt16"
  show (VIInt32 l)  = "VIInt32"
  show (VIInt64 l)  = "VIInt64"
  show (VIIntN l)   = "VIIntN"
  show (VIWord8 l)  = "VIWord8"
  show (VIWord16 l) = "VIWord16"
  show (VIWord32 l) = "VIWord32"
  show (VIWord64 l) = "VIWord64"
  show (VIWordN l)  = "VIWordN"
  show (VIFloat)    = "VIFloat"
  show (VIDouble)   = "VIDouble"
  show (VIProd l)   = "VIProd"

