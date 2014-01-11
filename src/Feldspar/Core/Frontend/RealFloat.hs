module Feldspar.Core.Frontend.RealFloat where

import qualified Prelude
import Prelude (Float,Double)

import Feldspar.Core.Types
import Feldspar.Core.Constructs
import Feldspar.Core.Constructs.RealFloat

-- Make new class, with "Data" in all the types

class (Type a, Prelude.RealFloat a) => RealFloat a where
  atan2 :: Data a -> Data a -> Data a
  atan2 = sugarSymF Atan2

instance RealFloat Float
instance RealFloat Double
