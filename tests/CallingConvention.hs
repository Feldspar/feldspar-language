{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Test that the compiler respects the calling conventions of Feldspar

module Main where

import qualified Prelude

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck

import Feldspar hiding (assert)
import Feldspar.Vector
import Feldspar.Compiler
import Feldspar.Compiler.Plugin
import Feldspar.Core.NestedTuples

import Control.Applicative

-- | Arbitrary instances for nested tuples
instance Arbitrary (Tuple TNil) where
  arbitrary = return TNil

instance (Arbitrary a, Arbitrary (Tuple b)) => Arbitrary (Tuple (a :* b)) where
  arbitrary = do a <- arbitrary
                 b <- arbitrary
                 return (a :* b)

vector1D :: Length -> Gen a -> Gen [a]
vector1D l = vectorOf (Prelude.fromIntegral l)

pairArg :: (Data Word8,Data IntN) -> Data IntN
pairArg (a,b) = i2n a + b

pairRes :: Data Word16 -> (Data WordN, Data IntN)
pairRes a = (i2n a, i2n a)

vecId :: Pull1 Word32 -> Pull1 Word32
vecId = id

vectorInPair :: (Pull1 WordN, Data WordN) -> (Data WordN, Pull1 WordN)
vectorInPair (v,a) = (a,v)

vectorInVector :: Pull DIM1 (Pull1 WordN) -> Data WordN
vectorInVector v = fromZero $ sum $ map (fromZero . sum) v

vectorInPairInVector :: Data WordN -> Pull DIM1 (Data WordN, Pull1 WordN)
vectorInPairInVector l = indexed1 l $ \i -> (i, indexed1 i id)

shTest :: Data Length -> Data Length
shTest n = runMutable $ do
             a <- newArr n 1
             c <- newArr n 2
             let d = n<5 ? a $ c
             setArr d 0 n
             b <- getArr a $ 0
             return b

arrayInStructR :: Data [Length] -> Data [Length]
arrayInStructR a = snd $ whileLoop (getLength a, a) (\(n,_) -> (n>0)) (\(n,a) -> (n-1, parallel (getLength a) (\ i -> a!i + 5)))

pairParamR :: (Data Index, Data Index) -> Data Index
pairParamR (x, _) = x

pairParam2R :: (Data Int16, Data Int16) ->
              ((Data Int16, Data Int16), (Data Int16, Data Int16))
pairParam2R c = (c, c)

copyPushR :: Pull1 Index -> DPush DIM1 Index
copyPushR v = let pv = toPush v in pv ++ pv

complexWhileCondR :: Data Int32 -> (Data Int32, Data Int32)
complexWhileCondR y = whileLoop (0,y) (\(a,b) -> ((\a b -> a * a < b * b) a (b-a))) (\(a,b) -> (a+1,b))

deepArrayCopyTest :: Data [[[Length]]] -> (Data [[[Length]]], Data [[[Length]]])
deepArrayCopyTest xs = (xs, xs)

loadFun ['pairArg]
loadFun ['pairRes]
loadFun ['vecId]
loadFun ['vectorInPair]
loadFun ['vectorInVector]
loadFun ['vectorInPairInVector]
loadFun ['shTest]
loadFun ['arrayInStructR]
loadFun ['pairParamR]
loadFun ['pairParam2R]
loadFun ['copyPushR]
loadFun ['complexWhileCondR]
loadFun ['deepArrayCopyTest]

prop_pairArg = eval pairArg ==== c_pairArg
prop_pairRes = eval pairRes ==== c_pairRes
prop_vecId (Small l) =
    forAll (vector1D l arbitrary) $ \xs ->
      eval vecId xs ==== c_vecId xs
prop_vectorInPair (Small l) =
    forAll (npair <$> vector1D l arbitrary <*> arbitrary) $ \p ->
      eval vectorInPair p ==== c_vectorInPair p
prop_vectorInVector (Small l1) (Small l2) =
    forAll (vector1D l1 (vector1D l2 arbitrary)) $ \v ->
      eval vectorInVector v ==== c_vectorInVector v
prop_vectorInPairInVector (Small l) = eval vectorInPairInVector l ==== c_vectorInPairInVector l
prop_shTest (Positive n) = eval shTest n ==== c_shTest n
prop_deepArrayCopyTest = eval deepArrayCopyTest ==== c_deepArrayCopyTest

prop_arrayInStruct = eval arrayInStructR ==== c_arrayInStructR
prop_pairParam = eval pairParamR ==== c_pairParamR
prop_pairParam2 = eval pairParam2R ==== c_pairParam2R
prop_copyPush = eval copyPushR ==== c_copyPushR
prop_complexWhileCond (Small n) = eval complexWhileCondR n ==== c_complexWhileCondR n

tests :: TestTree
tests = testGroup "CallingConvention"
    [ testProperty "pairArg" prop_pairArg
    , testProperty "pairRes" prop_pairRes
    , testProperty "vecId"   prop_vecId
    , testProperty "vectorInPair" prop_vectorInPair
    , testProperty "vectorInVector" prop_vectorInVector
    -- TODO: This test case will cause a segmentation fault due to issue #145
    -- , testProperty "vectorInPairInVector" prop_vectorInPairInVector
    , testProperty "arrayInStruct" prop_arrayInStruct
    , testProperty "pairParam" prop_pairParam
    , testProperty "pairParam2" prop_pairParam2
    , testProperty "copyPush" prop_copyPush
    , testProperty "complexWhileCond" prop_complexWhileCond
    , testProperty "deepArrayCopy" prop_deepArrayCopyTest
    ]

main :: IO ()
main = defaultMain tests
