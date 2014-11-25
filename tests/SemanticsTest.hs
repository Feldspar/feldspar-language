{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.QuickCheck

import Feldspar ((====), eval)
import Examples.Simple.Basics
import qualified Feldspar.Core.Test
import qualified Feldspar.Mutable.Test
import qualified Feldspar.Vector.Test

prop_example5 = eval example5 ==== (+)
prop_example9 = eval example9 ==== \a -> if a<5 then 3*(a+20) else 30*(a+20)

tests = $(testGroupGenerator)

main = defaultMain $ testGroup "Tests"
    [ tests
    , Feldspar.Mutable.Test.tests
    , Feldspar.Vector.Test.tests
    ]

