module Feldspar.Tuple.Test (tests) where

import qualified Prelude as P
import Feldspar

import Test.Tasty
import Test.Tasty.QuickCheck

-- | Test that we can build and take apart tuples correctly.
--   In particular, the Syntactic instances for tuples are tested.

tests = testProperties "Feldspar.Tuple.Test"
    [ ("tup2",  eval tup2t  ==== facit  2 )
    , ("tup3",  eval tup3t  ==== facit  3 )
    , ("tup4",  eval tup4t  ==== facit  4 )
    , ("tup5",  eval tup5t  ==== facit  5 )
    , ("tup6",  eval tup6t  ==== facit  6 )
    , ("tup7",  eval tup7t  ==== facit  7 )
    , ("tup8",  eval tup8t  ==== facit  8 )
    , ("tup9",  eval tup9t  ==== facit  9 )
    , ("tup10", eval tup10t ==== facit 10 )
    , ("tup11", eval tup11t ==== facit 11 )
    , ("tup12", eval tup12t ==== facit 12 )
    , ("tup13", eval tup13t ==== facit 13 )
    , ("tup14", eval tup14t ==== facit 14 )
    , ("tup15", eval tup15t ==== facit 15 )
    ]

-- The conditional ensures that the tuple is actually built and
-- the scaling ensures that the tuple components are treated differently.
tup2t :: Data Word32 -> Data Word32
tup2t n = sumComps (n < 3 ? mkTup n $ mkTup (n+1))
  where sumComps (a, b) = a + 2*b
        mkTup m = (m, m+1)

tup3t :: Data Word32 -> Data Word32
tup3t n = sumComps (n < 3 ? mkTup n $ mkTup (n+1))
  where sumComps (a, b, c)
               = a + 2*b + 3*c
        mkTup m = (m, m+1, m+2)

tup4t :: Data Word32 -> Data Word32
tup4t n = sumComps (n < 3 ? mkTup n $ mkTup (n+1))
  where sumComps (a, b, c, d)
               = a + 2*b + 3*c + 4*d
        mkTup m = (m, m+1, m+2, m+3)

tup5t :: Data Word32 -> Data Word32
tup5t n = sumComps (n < 3 ? mkTup n $ mkTup (n+1))
  where sumComps (a, b, c, d, e)
               = a + 2*b + 3*c + 4*d + 5*e
        mkTup m = (m, m+1, m+2, m+3, m+4)

tup6t :: Data Word32 -> Data Word32
tup6t n = sumComps (n < 3 ? mkTup n $ mkTup (n+1))
  where sumComps (a, b, c, d, e, f)
               = a + 2*b + 3*c + 4*d + 5*e + 6*f
        mkTup m = (m, m+1, m+2, m+3, m+4, m+5)

tup7t :: Data Word32 -> Data Word32
tup7t n = sumComps (n < 3 ? mkTup n $ mkTup (n+1))
  where sumComps (a, b, c, d, e, f, g)
               = a + 2*b + 3*c + 4*d + 5*e + 6*f + 7*g
        mkTup m = (m, m+1, m+2, m+3, m+4, m+5, m+6)

tup8t :: Data Word32 -> Data Word32
tup8t n = sumComps (n < 3 ? mkTup n $ mkTup (n+1))
  where sumComps (a, b, c, d, e, f, g, h)
               = a + 2*b + 3*c + 4*d + 5*e + 6*f + 7*g + 8*h
        mkTup m = (m, m+1, m+2, m+3, m+4, m+5, m+6, m+7)

tup9t :: Data Word32 -> Data Word32
tup9t n = sumComps (n < 3 ? mkTup n $ mkTup (n+1))
  where sumComps (a, b, c, d, e, f, g, h, i)
               = a + 2*b + 3*c + 4*d + 5*e + 6*f + 7*g + 8*h + 9*i
        mkTup m = (m, m+1, m+2, m+3, m+4, m+5, m+6, m+7, m+8)

tup10t :: Data Word32 -> Data Word32
tup10t n = sumComps (n < 3 ? mkTup n $ mkTup (n+1))
  where sumComps (a, b, c, d, e, f, g, h, i, j)
               = a + 2*b + 3*c + 4*d + 5*e + 6*f + 7*g + 8*h + 9*i + 10*j
        mkTup m = (m, m+1, m+2, m+3, m+4, m+5, m+6, m+7, m+8, m+9)

tup11t :: Data Word32 -> Data Word32
tup11t n = sumComps (n < 3 ? mkTup n $ mkTup (n+1))
  where sumComps (a, b, c, d, e, f, g, h, i, j, k)
               = a + 2*b + 3*c + 4*d + 5*e + 6*f + 7*g + 8*h + 9*i + 10*j + 11*k
        mkTup m = (m, m+1, m+2, m+3, m+4, m+5, m+6, m+7, m+8, m+9, m+10)

tup12t :: Data Word32 -> Data Word32
tup12t n = sumComps (n < 3 ? mkTup n $ mkTup (n+1))
  where sumComps (a, b, c, d, e, f, g, h, i, j, k, l)
               = a + 2*b + 3*c + 4*d + 5*e + 6*f + 7*g + 8*h + 9*i + 10*j + 11*k + 12*l
        mkTup m = (m, m+1, m+2, m+3, m+4, m+5, m+6, m+7, m+8, m+9, m+10, m+11)

tup13t :: Data Word32 -> Data Word32
tup13t n = sumComps (n < 3 ? mkTup n $ mkTup (n+1))
  where sumComps (a, b, c, d, e, f, g, h, i, j, k, l, m)
               = a + 2*b + 3*c + 4*d + 5*e + 6*f + 7*g + 8*h + 9*i + 10*j + 11*k + 12*l + 13*m
        mkTup m = (m, m+1, m+2, m+3, m+4, m+5, m+6, m+7, m+8, m+9, m+10, m+11, m+12)

tup14t :: Data Word32 -> Data Word32
tup14t n = sumComps (n < 3 ? mkTup n $ mkTup (n+1))
  where sumComps (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
               = a + 2*b + 3*c + 4*d + 5*e + 6*f + 7*g + 8*h + 9*i + 10*j + 11*k + 12*l + 13*m + 14*n
        mkTup m = (m, m+1, m+2, m+3, m+4, m+5, m+6, m+7, m+8, m+9, m+10, m+11, m+12, m+13)

tup15t :: Data Word32 -> Data Word32
tup15t n = sumComps (n < 3 ? mkTup n $ mkTup (n+1))
  where sumComps (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
               = a + 2*b + 3*c + 4*d + 5*e + 6*f + 7*g + 8*h + 9*i + 10*j + 11*k + 12*l + 13*m + 14*n + 15*o
        mkTup m = (m, m+1, m+2, m+3, m+4, m+5, m+6, m+7, m+8, m+9, m+10, m+11, m+12, m+13, m+14)

-- | Compute the reference value, r is the arity of the tuple
facit :: Word32 -> Word32 -> Word32
facit r n = if n P.< 3 then val n else val (n+1)
  where val k = P.sum $ P.zipWith (*) [k..] [1..r]
