{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

{- | The ValueInfo module defines the types and basic operations for value set
     analysis. In this implementation, a value set is either

     * an inclusive range [lo, hi] wrapped in a constructor giving the type of
       elements in the range, or
     * a product (v1, ..., vk) of values denoting the cartesian product of the vi.
-}

module Feldspar.ValueInfo where

import Feldspar.Range
import Feldspar.Core.Types (IntN, WordN)

import Data.Int
import Data.Word

-- | The ValueInfo type wraps ranges of various types as well as cartesian
--   products into a universal domain.
--   The value info of a function is the value info of its return value.
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
  show (VIBool r)   = "VIBool " ++ show r
  show (VIInt8 r)   = "VIInt8 " ++ show r
  show (VIInt16 r)  = "VIInt16 " ++ show r
  show (VIInt32 r)  = "VIInt32 " ++ show r
  show (VIInt64 r)  = "VIInt64 " ++ show r
  show (VIIntN r)   = "VIIntN " ++ show r
  show (VIWord8 r)  = "VIWord8 " ++ show r
  show (VIWord16 r) = "VIWord16 " ++ show r
  show (VIWord32 r) = "VIWord32 " ++ show r
  show (VIWord64 r) = "VIWord64 " ++ show r
  show (VIWordN r)  = "VIWordN " ++ show r
  show (VIFloat)    = "VIFloat"
  show (VIDouble)   = "VIDouble"
  show (VIProd vs)  = "VIProd " ++ show vs

