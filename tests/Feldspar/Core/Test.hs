{-# LANGUAGE TemplateHaskell #-}

module Feldspar.Core.Test (coreTests) where

import qualified Prelude

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.QuickCheck

import qualified Data.List as DL

import Feldspar
import Feldspar.Mutable

coreTests = $(testGroupGenerator)

deinterleave :: (Type a) => Data [a] -> Data [a]
deinterleave input =
    let n     = getLength input
        cols  = 12
        rows  = 7
        steps = n `div` (rows * cols)
    in runMutableArray $ do
        arr <- newArr_ n
        oix <- newRef 0
        forM steps $ \s ->
          forM rows $ \r ->
            forM cols $ \c -> do
              ix <- getRef oix
              setArr arr ix $ input ! (s*rows*cols + c*rows + r)
              modifyRef oix (+1)
        forM (n - steps*rows*cols) $ const $ do
          ix <- getRef oix
          setArr arr ix $ input ! ix
          modifyRef oix (+1)
        return arr

withInterleaveable :: (Arbitrary a, Show a, Testable prop)
                   => ([a] -> prop) -> Property
withInterleaveable prop =
    forAll (elements [84..167]) $ \len ->
      forAll (vectorOf len arbitrary) prop

prop_deinterleave_preseves_elements :: Property
prop_deinterleave_preseves_elements =
    withInterleaveable $ \xs ->
      (DL.sort xs :: [WordN]) ==== DL.sort (eval deinterleave xs)
