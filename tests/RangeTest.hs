module Main where

import Feldspar.Range.Test

import Test.Tasty

main = defaultMain $ testGroup "Tests" tests
