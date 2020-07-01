-- This module implements FIR filters, and compositions of them.

-- The generated code looks OK, but the use of `force` leads to different space/time
-- characteristics.
--
-- In order to fuse filters without forcing the entire intermediate vector, one should use streams
-- with a cyclic buffer; see example in the Stream library.

import qualified Prelude
import Feldspar
import Feldspar.Vector
import Feldspar.Matrix
import Feldspar.Compiler



causalMap :: Syntax a => (Vector a -> a) -> Vector a -> Vector a
causalMap f = map (f . reverse) . inits

-- | FIR filter
fir
    :: Vector1 Float  -- ^ Coefficients
    -> Vector1 Float  -- ^ Input
    -> Vector1 Float
fir coeffs = causalMap (coeffs***)



--------------------------------------------------------------------------------
-- Composing filters

composition boundary as bs = fir as . boundary . fir bs

test1 :: Vector1 Float -> Vector1 Float -> Vector1 Float -> Vector1 Float
test1 = composition id -:: newLen 10 >-> newLen 10 >-> newLen 100 >-> id
  -- Loop structure:
  --
  --   parallel 100
  --     forLoop 10
  --       forLoop 10

test2 :: Vector1 Float -> Vector1 Float -> Vector1 Float -> Vector1 Float
test2 = composition force -:: newLen 10 >-> newLen 10 >-> newLen 100 >-> id
  -- Loop structure:
  --
  --   parallel 100
  --     forLoop 10
  --   parallel 100
  --     forLoop 10



--------------------------------------------------------------------------------
-- Filters with hard-coded coefficients

composition2 boundary = fir (value [0.3,0.4,0.5,0.6]) . boundary . fir (value [0.3,-0.4,0.5,-0.6])

test3 :: Vector1 Float -> Vector1 Float
test3 = composition2 id -:: newLen 100 >-> id

test4 :: Vector1 Float -> Vector1 Float
test4 = composition2 force -:: newLen 100 >-> id

