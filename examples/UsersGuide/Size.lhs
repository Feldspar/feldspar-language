% Size analysis

  <br/>



\begin{code}
module UsersGuide.Size where

import qualified Prelude
import Feldspar
import Feldspar.Vector
\end{code}

Feldspar programs are quite dynamic. For example, functions like `drop` can operate on and return vectors of arbitrary length. However, when generating high-performance code to run on embedded targets, it is usually advantageous to have programs that are as static as possible, for example, to avoid dynamic memory allocation. Feldspar approaches this problem by performing a static size analysis on the programs. The inferred sizes are also used for optimizations on the core language.

Consider the following function:

\begin{code}
f :: Data Word8 -> Data Word8
f a = min (a+2) 10
\end{code}

To see the results of size inference, we use the function `drawDecor`, which draws the AST decorated with (among other things) size information:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*UsersGuide.Size> drawDecor f
<<Word8 -> Word8 | Range {lowerBound = 0, upperBound = 10}>>
|
`- Lambda 0
   |
   `- <<Word8 | Range {lowerBound = 0, upperBound = 10}>>
      |
      `- min
         |
         +- <<Word8 | Range {lowerBound = 0, upperBound = 255}>>
         |  |
         |  `- (+)
         |     |
         |     +- <<Word8 | Range {lowerBound = 0, upperBound = 255}>>
         |     |  |
         |     |  `- var:0
         |     |
         |     `- <<Word8 | Range {lowerBound = 2, upperBound = 2}>>
         |        |
         |        `- 2
         |
         `- <<Word8 | Range {lowerBound = 10, upperBound = 10}>>
            |
            `- 10
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The size on the root of the tree says that the result of `f` is in the range [0,10].

Size constraints
================

Imagine we want to generate code for the `drop` function in such a way that we can statically allocate the result. Let us see what size inference gives us:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*UsersGuide.Size> drawDecor (drop :: Data Index -> Vector (Data Word8) -> Vector (Data Word8))
<<WordN -> [Word8] -> [Word8] | Range {lowerBound = 0, upperBound = 4294967295} :> Range {lowerBound = 0, upperBound = 255}>>
...
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The root has a size of the form `rl :> re`, where `rl` is the range of the vector's length and `re` is the range of the elements. As we see, no useful size could be inferred, since the analysis knows nothing about the length of the input vector or the number of elements we want to drop.

In cases where we --- the programmers --- have additional information about the sizes of the inputs, it is possible to help size inference to achieve more accurate results. Imagine that we know that the input has at most 200 elements and that we will always drop at least 100 elements. Then we can make a specialized version of `drop` that has this information hard-coded:

\begin{code}
drop2 :: Data Index -> Vector (Data Word8) -> Vector (Data Word8)
drop2 n v = drop n' v'
  where
    n' = max 100 n
    v' = newLen (min 200 (length v)) v
\end{code}

This lets the analysis infer the range [0,100] for the length of the result, which means that it is safe for the back end to allocate a 100-element array.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*UsersGuide.Size> drawDecor drop2
<<WordN -> [Word8] -> [Word8] | Range {lowerBound = 0, upperBound = 100} :> Range {lowerBound = 0, upperBound = 255}>>
...
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



Avoiding run-time checks
========================

The down-side of the technique used in `drop2` is that the use of `max` and `min` incurs a run-time overhead. If we are certain that the input has at most 200 elements and that we will always drop at least 100 elements, these checks are useless, and could just as well be removed. To do this, we simply replace `max` and `min` with `notBelow` and `notAbove` respectively:

\begin{code}
drop3 :: Data Index -> Vector (Data Word8) -> Vector (Data Word8)
drop3 n v = drop n' v'
  where
    n' = notBelow 100 n
    v' = newLen (notAbove 200 (length v)) v
\end{code}

Functions `notBelow` and `notAbove` should be used with care as they are only well-defined as long as the guarantee is fulfilled. These, and some other functions for guiding the size analysis are found in the module [`Feldspar.Core.Frontend.SizeProp`](http://hackage.haskell.org/package/feldspar-language).



Size-based optimization
=======================

If we look at the core expression resulting from `drop3`, we see that it still contains a run-time check:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*UsersGuide.Size> printExpr drop3
(\var0 -> (\var1 -> (letBind (getLength var1) (\var3 -> (parallel (var3 - (min var3 var0)) (\var2 -> (var1 ! (var2 + var0))))))))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The `min` function is used to check that we don't try to drop more elements than we have in the array. If we know that we will never try to drop too many elements, we can get rid of this check as well. To do this, we have to supply an upper bound for first argument and a lower bound for the length, using the `between` function:

\begin{code}
drop4 :: Data Index -> Vector (Data Word8) -> Vector (Data Word8)
drop4 n v = drop n' v'
  where
    n' = between 100 120 n
    v' = newLen (between 150 200 (length v)) v
\end{code}

This gives us a core expression without any run-time checks:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*UsersGuide.Size> printExpr drop4
(\var0 -> (\var1 -> (parallel ((getLength var1) - var0) (\var2 -> (var1 ! (var2 + var0))))))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Curiously, size analysis also figures out that the resulting vector length is in the range [30..100]:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*UsersGuide.Size> drawDecor drop4
<<WordN -> [Word8] -> [Word8] | Range {lowerBound = 30, upperBound = 100} :> Range {lowerBound = 0, upperBound = 255}>>
...
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

