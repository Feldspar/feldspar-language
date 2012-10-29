% Core arrays

  <br/>



*[This document needs to be extended.]*

It is generally not recommended to use core arrays directly. A more high-level interface is provided by the [vector library](Vector.html).

\begin{code}
module Tutorial.Array where

import qualified Prelude
import Feldspar
\end{code}

An array containing the powers of two:

\begin{code}
powsOfTwo :: Data Length -> Data [WordN]
powsOfTwo l = parallel l (^2)
\end{code}

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*Tutorial.Array> eval powsOfTwo 10
[0,1,4,9,16,25,36,49,64,81]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

