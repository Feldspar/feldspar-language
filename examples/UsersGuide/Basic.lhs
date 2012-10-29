% Basic usage

  <br/>



*[This document needs to be extended.]*

Getting started
===============

Feldspar is implemented as an embedded language in Haskell. To use Feldspar, simply import the `Feldspar` module in a Haskell source file:

\begin{code}
module UsersGuide.Basic where

import Feldspar
\end{code}

Since Feldspar redefines several standard Haskell identifiers, it is strongly recommended to import the standard `Prelude` qualified:

\begin{code}
import qualified Prelude
\end{code}

(Certain usefule `Prelude` identifiers that are not used by Feldspar are reexported by the `Feldspar` module.)

In order to try out the examples in this file, we just load the file in GHCi:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
terminal> ghci Basic.lhs
GHCi, version 7.6.1: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
[1 of 1] Compiling UsersGuide.Basic ( Basic.lhs, interpreted )
Ok, modules loaded: UsersGuide.Basic.
*UsersGuide.Basic>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We are now ready to define a simple program. The following function computes the length of the hypotenuse of a triangle, given the lengths of the two catheti:

\begin{code}
hypotenuse :: Data Float -> Data Float -> Data Float
hypotenuse c1 c2 = sqrt (square c1 + square c2)
  where
    square x = x*x
\end{code}

Note that this code is *identical* to the corresponding code in ordinary Haskell, except for the `Data` constructor in the type.

What makes `hypotenuse` different from the corresponding Haskell function can be seen when we try to evaluate it:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*UsersGuide.Basic> hypotenuse 3 4
(sqrt ((3.0 * 3.0) + (4.0 * 4.0)))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Hm... instead of computing a result, `hypotenuse` returned an unevaluated expression. In order to evaluate this expression, we have to use the function `eval`:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*UsersGuide.Basic> eval hypotenuse 3 4
5.0
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

