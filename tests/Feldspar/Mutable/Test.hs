{-# LANGUAGE TemplateHaskell #-}

module Feldspar.Mutable.Test where



import Feldspar
import Feldspar.Mutable

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.QuickCheck



buffProg :: Length -> Data Length -> Data WordN
buffProg bl n = runMutable $ do
    buf <- newBuffer (value bl) (0 :: Data WordN)
    forM n $ \i -> putBuf buf i
    as <- Prelude.sequence [indexBuf buf (value j) | j <- [0 .. bl-1]]
    return (Prelude.foldr (+) 0 as)

prop_buff =
    forAll (choose (1,10)) $ \bl ->
      forAll (choose (1,100)) $ \n ->
        let bl' = fromIntegral bl
        in  eval (buffProg bl) n Prelude.== (sum $ take bl' $ reverse $ replicate bl' 0 ++ [0..n-1])

-- Test that `withBuf` followed by indexing behaves like `indexBuf`
prop_withBuf =
    forAll (choose (1,5)) $ \bl ->
      forAll (choose (1,15)) $ \n ->
        forAll (vector n) $ \as ->
          forAll (choose (0,15)) $ \i ->
            eval (prog1 as) bl i Prelude.== eval (prog2 as) bl i
  where
    prog1 :: [Data Word32] -> Data Length -> Data Index -> Data Word32
    prog1 as bl i = runMutable $ do
        buf <- newBuffer bl 0
        sequence_ [putBuf buf a | a <- as]
        indexBuf buf i

    prog2 :: [Data Word32] -> Data Length -> Data Index -> Data Word32
    prog2 as bl i = runMutable $ do
        buf <- newBuffer bl 0
        sequence_ [putBuf buf a | a <- as]
        withBuf buf $ \b -> return (b!i)

tests = $(testGroupGenerator)

