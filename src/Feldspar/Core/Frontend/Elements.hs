module Feldspar.Core.Frontend.Elements
  ( materialize
  , write
  , par
  , parFor
  , skip
  ) where

import Language.Syntactic

import Feldspar.Core.Types
import Feldspar.Core.Constructs
import Feldspar.Core.Constructs.Elements

materialize :: Type a => Data Length -> Data (Elements a) -> Data [a]
materialize = sugarSymC EMaterialize

write :: Type a => Data Index -> Data a -> Data (Elements a)
write = sugarSymC EWrite

par :: Type a => Data (Elements a) -> Data (Elements a) -> Data (Elements a)
par = sugarSymC EPar

parFor :: Type a => Data Length -> (Data Index -> Data (Elements a)) -> Data (Elements a)
parFor = sugarSymC EparFor

skip :: Type a => Data (Elements a)
skip = sugarSymC ESkip

