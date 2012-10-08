{-# LANGUAGE TemplateHaskell #-}

module Feldspar.Vector.Test where

import qualified Prelude as P
import qualified Data.List as P

import Feldspar
import Feldspar.Prelude
import Feldspar.Vector.Internal

import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

tests = $(testGroupGenerator)

prop_freeze_thaw = eval (freezeVector . thawVector) === (id :: [Index] -> [Index])
prop_thaw_freeze = eval (thawVector . freezeVector) === (id :: [Index] -> [Index])

prop_length = eval (length -:: tVec1 tIndex >-> tData tLength) === P.genericLength

prop_append = eval ((++) -:: tVec1 tIndex >-> id >-> id) === (P.++)
prop_take   = eval (take -:: tData tLength >-> tVec1 tIndex >-> id) === P.genericTake
prop_drop   = eval (drop -:: tData tLength >-> tVec1 tIndex >-> id) === P.genericDrop

