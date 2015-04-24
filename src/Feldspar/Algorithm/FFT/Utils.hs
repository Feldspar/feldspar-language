module Feldspar.Algorithm.FFT.Utils where

import qualified Prelude as P

import Feldspar
import Feldspar.Vector

withLen :: Data Length
        -> (Pull DIM1 a -> Pull DIM1 b)
        -> Pull DIM1 a -> Pull DIM1 b
withLen l f = newLen1 l . f . newLen1 l

