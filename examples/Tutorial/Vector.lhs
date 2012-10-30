% Vector library

  <br/>



*[This document needs to be extended.]*

The vector library provides an interface for vector operations similar to Haskell's list library. It is available through the module `Feldspar.Vector`:

\begin{code}
module Tutorial.Vector where

import qualified Prelude
import Feldspar
import Feldspar.Vector
\end{code}

Scalar product:

\begin{code}
scProd :: (Syntax a, Num a) => Vector a -> Vector a -> a
scProd a b = sum (zipWith (*) a b)
\end{code}

Specialize the type:

\begin{code}
scProdF = scProd :: Vector (Data Float) -> Vector (Data Float) -> Data Float
\end{code}

Testing:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*Tutorial.Vector> eval scProdF [1,2,3,4] [5,6,7,8::Float]
70.0
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Resulting core expression (with manually inserted white space):

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*Tutorial.Vector> printExpr scProdF
(\var0 -> (\var1 -> (
    forLoop (min (getLength var0) (getLength var1)) 0.0 (\var2 -> (\var3 ->
        (var3 + ((var0 ! var2) * (var1 ! var2)))
    ))
)))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note how `sum` and `zipWith` have been fused into a single `forLoop`.

