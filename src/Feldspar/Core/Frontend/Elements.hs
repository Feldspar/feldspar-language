module Feldspar.Core.Frontend.Elements
  ( materialize
  , write
  , par
  , parFor
  , skip
  ) where

import Feldspar.Core.Types
import Feldspar.Core.Constructs
import Feldspar.Core.Constructs.Elements

materialize :: Type a => Data Length -> Data (Elements a) -> Data [a]
materialize = sugarSymF EMaterialize

write :: Type a => Data Index -> Data a -> Data (Elements a)
write = sugarSymF EWrite

par :: Type a => Data (Elements a) -> Data (Elements a) -> Data (Elements a)
par = sugarSymF EPar

parFor :: Type a => Data Length -> (Data Index -> Data (Elements a)) -> Data (Elements a)
parFor = sugarSymF EparFor

skip :: Type a => Data (Elements a)
skip = sugarSymF ESkip

