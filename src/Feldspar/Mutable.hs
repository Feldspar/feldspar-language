{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Mutable data structures, etc.

module Feldspar.Mutable where



import qualified Prelude

import Feldspar
import Feldspar.SimpleVector



-- | Indexable cyclic buffer
data Buffer a = Buffer
    { indexBuf :: Data Index -> M a
    , putBuf   :: a -> M ()
    , withBuf  :: forall b . Syntax b => (Vector a -> M b) -> M b
    }

-- Another option would be to represent a buffer as its state (the counter and the array), but the
-- above representation leaves room for other implementations.

--- | Create a new cyclic buffer
newBuffer :: forall a . Syntax a => Data Length -> a -> M (Buffer a)
newBuffer l init = do
    buf <- newArr l (desugar init)
    l  <- arrLength buf
    ir <- newRef 0
    let get j = do
          i <- getRef ir
          fmap sugar $ getArr buf $ calcIndex l i j
        put a = do
          i <- getRef ir
          setRef ir ((i+1) `mod` l)
          setArr buf i $ desugar a
        with :: Syntax b => (Vector a -> M b) -> M b
        with f = do
          i <- getRef ir
          withArray buf (f . freeze i)
    return (Buffer get put with)
  where
    calcIndex l i j = (l+i-j-1) `mod` l

    freeze :: Syntax b => Data Index -> Data [Internal b] -> Vector b
    freeze i = permute (\l -> calcIndex l i) . sugar

