module Examples.Simple.Basics where

import qualified Prelude
import Feldspar
import Feldspar.Vector

-- Identity function for 32 bit integers.
example1 :: Data Int32 -> Data Int32
example1 = id

-- Constant function
example2 :: Data Int32
example2 = 2

-- A constant core vector
example3 :: Data [Int32]
example3 = value [42,1,2,3]

-- Examples showing some of the integer and boolean operations:

example4 :: Data Int32 -> Data Int32
example4 x = negate x

example5 :: Data Int32 -> Data Int32 -> Data Int32
example5 x y = x + y

example6 :: Data Int32 -> Data Int32 -> Data Bool
example6 x y = x == y

example7 :: Data Bool
example7 = 2 /= (2 :: Data Int32) -- Type of numeric literals sometimes have to be written explicitly.

example8 :: Data Bool -> Data Bool
example8 b = not b

-- Examples on using conditionals:

example9 :: Data Int32 -> Data Int32
example9 a = a < 5 ? (3 * (a + 20)) $ 30 * (a + 20)

example10 :: Data Int32 -> Data Int32
example10 a = a < 5 ? (3 * (a + a)) $ 30 * (a + a)
