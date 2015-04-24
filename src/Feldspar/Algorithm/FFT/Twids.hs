module Feldspar.Algorithm.FFT.Twids where

import qualified Prelude

import Feldspar
import Feldspar.Vector

twid :: Data Index -> Data Length -> Data Index -> Data (Complex Double)
twid scale n k = share (1 / i2f n) $ \d -> cis (i2f scale * 2 * pi * i2f k * d)

twids :: Data Length -> Pull1 (Complex Double)
twids n = indexed1 (n `div` 2) $ twid (-1) n

itwids :: Data Length -> Pull1 (Complex Double)
itwids n = indexed1 (n `div` 2) $ twid 1 n

