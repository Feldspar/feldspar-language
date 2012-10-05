module Main where

-- To generate the golden files use a script similiar to this one
-- > ghc -e 'B.writeFile "tests/gold/example9.txt" $ B.pack $ showDecor example9' tests/DecorationTests.hs -iexamples

import Test.Framework
import Test.Golden

import qualified Data.ByteString.Lazy.Char8 as B

import Feldspar (showDecor)
import Examples.Simple.Basics

tests = testGroup "DecorationTests"
    [ goldenVsString "example9" "tests/gold/example9.txt" $ return $ B.pack $ showDecor example9
    ]

main = defaultMain [tests]

