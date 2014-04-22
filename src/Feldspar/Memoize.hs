{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE FlexibleContexts #-}
module Feldspar.Memoize where

import qualified Prelude as P
import qualified Data.Bits as B
import Feldspar.Range

import Feldspar

-- | Accelerate the function @f@ using a lookup table.
-- The table will span all possible input values.
tabulate :: (B.Bits i, P.Integral i, Type i, Syntax a, Size i ~ Range i
            ,B.Bits (UnsignedRep i),P.Ord (UnsignedRep i), P.Num (UnsignedRep i)
            ,P.Integral (UnsignedRep i), Bounded i, Bounded (UnsignedRep i))
         => (Data i -> a) -> Data i -> a
tabulate f i = tabulateLen (2 ^ bitSize i) f i

-- | Accelerate the function @f@ by creating a lookup table of the results for the
-- @len@ first argument values
--
-- Note. To really get a table the function must be closed after the
-- application to @i@
-- 
tabulateLen :: (P.Integral i, Type i, Syntax a, Size i ~ Range i)
            => Data Length -> (Data i -> a) -> Data i -> a
tabulateLen len f i = sugar $ share (parallel len (desugar.f.i2n)) (!i2n i)

