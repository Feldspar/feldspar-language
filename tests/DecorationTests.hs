module Main where

-- To generate the golden files use a script similiar to this one
-- > ghc -isrc -iexampes -itests -e 'B.writeFile "tests/gold/example9.txt" $ B.pack $ showDecor example9' tests/DecorationTests.hs -iexamples

import qualified Prelude

import Test.Framework
import Test.Golden

import qualified Data.ByteString.Lazy.Char8 as B

import Feldspar
import Examples.Simple.Basics



topLevelConsts :: Data Index -> Data Index -> Data Index
topLevelConsts a b = condition (a<5) (d ! (b+5)) (c ! (b+5))
  where
    c = value [1,2,3,4,5] :: Data [Index]
    d = value [2,3,4,5,6] :: Data [Index]

monadicSharing :: Data Index -> Data Index
monadicSharing a = runMutable $ do
    b  <- newRef a
    b' <- getRef b
    c  <- newRef (b'+3)
    c' <- getRef c
    return (c'+(b'+3))

tests = testGroup "DecorationTests"
    [ goldenVsFile "example9" "tests/gold/example9.txt" "tests/example9.txt" $ B.writeFile "tests/example9.txt" $ B.pack $ showDecor example9
    , goldenVsFile "topLevelConsts" "tests/gold/topLevelConsts.txt" "tests/topLevelConsts.txt" $ B.writeFile "tests/topLevelConsts.txt" $ B.pack $ showDecor topLevelConsts
    , goldenVsFile "monadicSharing" "tests/gold/monadicSharing.txt" "tests/monadicSharing.txt" $ B.writeFile "tests/monadicSharing.txt" $ B.pack $ showDecor monadicSharing
    ]

main = defaultMain [tests]

