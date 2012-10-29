% Constraints

  <br/>



This file demonstrates how to use [patches](http://hackage.haskell.org/package/patch-combinators) to constrain types and sizes of Feldspar expressions.

\begin{code}
module UsersGuide.Patch where

import qualified Prelude
import Feldspar
import Feldspar.Vector
\end{code}



Type patches
============

Say we want to print the core expression resulting from the `scalarProd` function. Attempting this without giving any size constraints leads to an "ambiguous type" error:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*UsersGuide.Patch> printExpr scalarProd
<interactive>:26:1:
    Couldn't match type `syntactic-1.5.0:Language.Syntactic.Sugar.Domain
                           a0'
                  with `FeldDomain'
    The type variable `a0' is ambiguous
    Possible fix: add a type signature that fixes these type variable(s)
    In the expression: printExpr scalarProd
    In an equation for `it': it = printExpr scalarProd
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The solution is to provide a type signature for `scalarProd`:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*UsersGuide.Patch> printExpr (scalarProd :: Vector1 Float -> Vector1 Float -> Data Float)
...
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An alternative --- and less verbose --- way of specifying type constraints is to use *type patches* instead:

\begin{code}
test1 = printExpr $ scalarProd -:: tVec1 tFloat >-> id
\end{code}

The part to the right of `-::` is the "patch", which is applied as a wrapper around `scalarProd`. In general, patches can change the behavior of the wrapped function, but type patches, such as the one above, only have the effect of constraining the function's type. The patch `tVec1 tFloat >-> id` is composed of two smaller patches: `tVec1 tFloat` and `id` (the identity function). The first of these is applied to the first argument and constrains its type to `Vector1 Float`. The `id` patch simply leaves the result (that is, the partially applied `scalarProd`) untouched. Thus, the whole patch can be thought of as the following *partial type signature* for `scalarProd`:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
scalarProd :: Vector1 Float -> _  -- Not legal Haskell
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}



Avoiding length checks
======================

In the expression printed by `test1`, we see a `min` function computing the number of iterations as the minimum of the input vector lengths. It is possible to get rid of this comparison by giving the second vector the same length as the first:

\begin{code}
test2 = printExpr $ scalarProd
    -:: tVec1 tFloat >-> id
    -:: name (\a -> id >-> newLen (length a) >-> id)
\end{code}

The `name` combinator lets us bind the first argument (to the variable `a`) and use it in the patch. The actual patch is then

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
id >-> newLen (length a) >-> id
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}

which simply sets the length of the second argument to the length of `a`.



Size patches
============

[`UsersGuide.Size`](Size.html) describes how size analysis works and how to provide size constraints to improve the analysis. Size constraints are often convenient to use as patches.

The following example was used to demonstrate how the programmer can guide the analysis by providing guarantees about the ranges of values in the program:

\begin{code}
drop4 :: Data Index -> Vector (Data Word8) -> Vector (Data Word8)
drop4 n v = drop n' v'
  where
    n' = between 100 120 n
    v' = newLen (between 150 200 (length v)) v
\end{code}

Using patch combinators, `drop4` can be written more succinctly as

\begin{code}
drop4' :: Data Index -> Vector (Data Word8) -> Vector (Data Word8)
drop4' = drop -:: between 100 120 >-> (between 150 200 |> id) >-> id
\end{code}

The patch combinator `(|>)` takes a patch for the length (`between 150 200`) and a patch for the elements (`id`) and returns a patch for a whole vector.

