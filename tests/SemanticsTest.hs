{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

import Feldspar ((===), eval)
import Examples.Simple.Basics
import qualified Feldspar.Vector.Test

prop_example5 = eval example5 === (+)
prop_example9 = eval example9 === \a -> if a<5 then 3*(a+20) else 30*(a+20)

tests = $(testGroupGenerator)

main = defaultMain [ tests
                   , Feldspar.Vector.Test.tests
                   ]

