{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Mutable data structures, etc.

module Feldspar.Mutable
  ( module Feldspar.Core.Frontend.Mutable
  , module Feldspar.Core.Frontend.MutableArray
  , module Feldspar.Core.Frontend.MutableReference
  , module Feldspar.Core.Frontend.MutableToPure
  , module Feldspar.Core.Frontend.ConditionM
  , module Feldspar.Core.Frontend.LoopM

  , Buffer (..)
  , initBuffer'
  , initBuffer
  , newBuffer
  , newBuffer_
  , tM
  , initBuffer2
  ) where



import qualified Prelude

import Feldspar
import Feldspar.Core.Frontend.Mutable
import Feldspar.Core.Frontend.MutableArray
import Feldspar.Core.Frontend.MutableReference
import Feldspar.Core.Frontend.MutableToPure
import Feldspar.Core.Frontend.ConditionM
import Feldspar.Core.Frontend.LoopM
import Feldspar.Vector



-- | Indexable cyclic buffer
data Buffer a = Buffer
    { indexBuf :: Data Index -> M a
    , putBuf   :: a -> M ()
    , withBuf  :: forall b . Syntax b => (Pull DIM1 a -> M b) -> M b
    }

-- Another option would be to represent a buffer as its state (the counter and the array), but the
-- above representation leaves room for other implementations.

--- | Create a new cyclic buffer
initBuffer' :: forall a . Syntax a => Data (MArr (Internal a)) -> M (Buffer a)
initBuffer' buf = do
    l  <- arrLength buf
    ir <- newRef 0
    let get j = do
          i <- getRef ir
          fmap sugar $ getArr buf $ calcIndex l i j
        put a = do
          i <- getRef ir
          setRef ir ((i+1) `mod` l)
          setArr buf i $ desugar a
        with :: Syntax b => (Pull DIM1 a -> M b) -> M b
        with f = do
          i <- getRef ir
          withArray buf (f . freeze i)
    return (Buffer get put with)
  where
    calcIndex l i j = (l+i-j-1) `mod` l

    freeze :: Syntax b => Data Index -> Data [Internal b] -> Pull DIM1 b
    freeze i = permute (\l -> calcIndex l i) . map sugar . thawPull1

-- | Create a new cyclic buffer initalized by the given vector (which also determines the size)
initBuffer :: Syntax a => Pull DIM1 a -> M (Buffer a)
initBuffer buf = thawArray (freezePull1 $ map desugar buf) >>= initBuffer'

-- | Create a new cyclic buffer of the given length initialized by the given element
newBuffer :: Syntax a => Data Length -> a -> M (Buffer a)
newBuffer l init = newArr l (desugar init) >>= initBuffer'

-- | Create a new cyclic buffer of the given length without initialization
newBuffer_ :: Syntax a => Data Length -> M (Buffer a)
newBuffer_ l = newArr_ l >>= initBuffer'

tM :: Patch a a -> Patch (M a) (M a)
tM _ = id

initBuffer2' :: forall a . Syntax a => Data Length -> Data (MArr (Internal a))
                                    -> M (Buffer a)
initBuffer2' l buf = do
    ir <- newRef 0
    let get j = do
          i <- getRef ir
          fmap sugar $ getArr buf (j + i)
        put a = do
          i <- getRef ir
          setRef ir ((i+1) `mod` l)
          let a' = desugar a
          setArr buf i a'
          setArr buf (i+l) a'
        with :: Syntax b => (Pull DIM1 a -> M b) -> M b
        with f = do
          i <- getRef ir
          withArray buf (f . freeze i)
    return (Buffer get put with)
  where
    freeze :: Syntax b => Data Index -> Data [Internal b] -> Pull DIM1 b
    freeze i = take l . drop i . map sugar . thawPull1

-- | Create a new cyclic buffer. This implementation uses a buffer twice
--   as long as necessary to avoid all modulus operations when accessing
--   the elements.
initBuffer2 :: Syntax a => Pull DIM1 a -> M (Buffer a)
initBuffer2 buf = thawArray (freezePush1 $ dup $ map desugar buf) >>=
                  initBuffer2' (length buf)
