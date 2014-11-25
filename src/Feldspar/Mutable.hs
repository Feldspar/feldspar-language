{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

-- | Mutable data structures, etc.

module Feldspar.Mutable where



import qualified Prelude

import Feldspar



-- | Indexable cyclic buffer
data Buffer a = Buffer
    { indexBuf :: Data Index -> M a
    , putBuf   :: a -> M ()
    }

-- Another option would be to represent a buffer as its state (the counter and the array), but the
-- above representation leaves room for other implementations.

-- | Create a new cyclic buffer
newBuffer :: Syntax a => Data Length -> a -> M (Buffer a)
newBuffer l init = do
    buf <- newArr l $ desugar init
    ir  <- newRef 0
    let get j = do
          i <- getRef ir
          fmap sugar $ getArr buf ((l+i-j-1) `mod` l)
        put a = do
          i <- getRef ir
          setRef ir ((i+1) `mod` l)
          setArr buf i $ desugar a
    return (Buffer get put)

