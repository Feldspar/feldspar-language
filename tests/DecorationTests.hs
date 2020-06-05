{-# LANGUAGE DataKinds #-}

module Main where

-- To generate the golden files use a script similiar to this one
-- > ghc -isrc -iexampes -itests -e 'B.writeFile "tests/gold/example9.txt" $ B.pack $ showDecor example9' tests/DecorationTests.hs -iexamples

import qualified Prelude as P

import Test.Tasty
import Test.Tasty.Golden

import Feldspar
import Feldspar.Mutable
import Examples.Simple.Basics
import Feldspar.Applications.TFModel (tfModel)
import Feldspar.Core.NestedTuples


topLevelConsts :: Data Index -> Data Index -> Data Index
topLevelConsts a b = b < 5 ? (d ! (a + 5)) $ c ! (a + 5)
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

-- An example with nested sharing. Our first codeMotion would miss the opportunity to share `(a+b)`.
trickySharing :: Data Index -> Data Index
trickySharing x = (a+b+c) + (a+b) + (a+b+c)
  where
    a = x*3
    b = x*5
    c = x*7

-- We want no sharing between the two tuples in the result although they have a common tail
noshareT :: (Tuple '[Data Length, Data Length]
           , Tuple '[Data Length, Data Length])
noshareT = let two = 2 in (build $ tuple 1 two, build $ tuple 3 two)

-- We want sharing between the two tuples in the result since they are identical
shareT :: (Tuple '[Data Length, Data Length]
         , Tuple '[Data Length, Data Length])
shareT = (build $ tuple 1 2, build $ tuple 1 2)

selectT :: Data Length
selectT = sel First $ snd noshareT

ref :: P.String -> P.String
ref f = "tests/gold/" P.++ f

tests = testGroup "DecorationTests"
    [ goldenVsFile "example9" (ref "example9.txt") "tests/example9.txt" $ writeFile "tests/example9.txt" $ showDecor example9
    , goldenVsFile "topLevelConsts" (ref "topLevelConsts.txt") "tests/topLevelConsts.txt" $ writeFile "tests/topLevelConsts.txt" $ showDecor topLevelConsts
    , goldenVsFile "monadicSharing" (ref "monadicSharing.txt") "tests/monadicSharing.txt" $ writeFile "tests/monadicSharing.txt" $ showDecor monadicSharing
    , goldenVsFile "trickySharing" (ref "trickySharing.txt") "tests/trickySharing.txt" $ writeFile "tests/trickySharing.txt" $ showDecor trickySharing
    , goldenVsFile "noshareT" (ref "noshareT.txt") "tests/noshareT.txt" $ writeFile "tests/noshareT.txt" $ showDecor noshareT
    , goldenVsFile "shareT" (ref "shareT.txt") "tests/shareT.txt" $ writeFile "tests/shareT.txt" $ showDecor shareT
    , goldenVsFile "selectT" (ref "selectT.txt") "tests/selectT.txt" $ writeFile "tests/selectT.txt" $ showDecor selectT
    , goldenVsFile "tfModel" (ref "tfModel.txt") "tests/tfModel.txt" $ writeFile "tests/tfModel.txt" $ showUntyped defaultFeldOpts tfModel
    ]

main = defaultMain $ testGroup "Tests" [tests]
