{-# LANGUAGE TemplateHaskell #-}

module Feldspar.Vector.Test where

import qualified Prelude as P
import qualified Data.List as P

import Feldspar
import Feldspar.Prelude
import Feldspar.Vector

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.QuickCheck

tests = $(testGroupGenerator)

-- TODO implement tests
--
-- prop_freeze_thaw = eval (freezeVector . thawVector) === (id :: [Index] -> [Index])
-- prop_thaw_freeze = eval (thawVector . freezeVector) === (id :: [Index] -> [Index])

-- prop_length = eval (length -:: tVec1 tIndex >-> tData tLength) === P.genericLength

-- prop_append = eval ((++) -:: tVec1 tIndex >-> id >-> id) === (P.++)
-- prop_take   = eval (take -:: tData tLength >-> tVec1 tIndex >-> id) === P.genericTake
-- prop_drop   = eval (drop -:: tData tLength >-> tVec1 tIndex >-> id) === P.genericDrop
-- prop_revrev = eval ((reverse . reverse) -:: tVec1 tIndex >-> id) === id

