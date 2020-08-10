{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Top level module to test Feldspar. All tests should be in @tests@
--   to keep the total runtime for tests as short as possible.
module Main (main) where

import Test.Tasty
import Test.Tasty.Golden (goldenVsFile)
import Test.Tasty.Golden.Advanced
import Test.Tasty.QuickCheck

#ifdef mingw32_HOST_OS
import System.Win32.Console (getConsoleCP, setConsoleCP)
#endif
import qualified Prelude
import Feldspar
import qualified Feldspar.Core.UntypedRepresentation as UT
import Feldspar.Core.ValueInfo (ValueInfo, topInfo)
import Feldspar.Vector
import qualified Feldspar.SimpleVector.Push as SP
import qualified Feldspar.SimpleVector as S
import Feldspar.Compiler
import Feldspar.Compiler.Plugin
import Feldspar.Core.NestedTuples

import Examples.Simple.Basics
import Feldspar.Applications.TFModel (tfModel)
import Feldspar.Core.Test
import Feldspar.Mutable.Test
import Feldspar.Range.Test
import Feldspar.Stream.Test (streamTests, vector1D)
import Feldspar.Tuple.Test
import Feldspar.Vector.Test

import Control.Exception (catch, throwIO)
import Control.Monad
import Control.Monad.Except (liftIO)
import Data.Monoid ((<>))
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Search as LB
import GHC.Paths (ghc)
import System.Exit (ExitCode (..))
import System.Process
import Text.Printf

-- The main function for this module is just "main = defaultMain tests"
-- but it is situated near the end of the module to avoid compilation errors
-- about splices.

-- | Read the header and C file and concatenate their contents for comparisons
vgReadFiles :: String -> IO LB.ByteString
vgReadFiles base = liftM LB.concat $ mapM (LB.readFile . (base<>)) [".h",".c"]

-- Compile and load example9 as c_example9 (using plugins)
loadFun ['example9]

topLevelConsts :: Data Index -> Data Index -> Data Index
topLevelConsts a b = a < 5 ? (d ! (b + 5)) $ c ! (b + 5)
  where
    c = value [1,2,3,4,5] :: Data [Index]
    d = value [2,3,4,5,6] :: Data [Index]

pairParam :: (Data Index, Data Index) -> Data Index
pairParam (x, _) = x

pairParam2 :: (Data Int16, Data Int16) ->
              ((Data Int16, Data Int16), (Data Int16, Data Int16))
pairParam2 c = (c, c)

pairRet :: Data Index -> (Data Index, Data Index)
pairRet x = x > 3 ? (3, 9) $ (7, 5)

-- One test starting.
metrics :: Pull1 IntN -> Pull1 IntN
            -> Pull DIM1 (Pull DIM1 (Data Index, Data Index)) -> Pull DIM1 (Pull1 IntN)
metrics s _ = scan (columnMetrics s) initialMetrics

initialMetrics :: Pull1 IntN
initialMetrics = replicate1 8 (-32678)

columnMetrics :: Pull1 IntN -> Pull1 IntN -> Pull DIM1 (Data Index, Data Index)
                  -> Pull1 IntN
columnMetrics s prev zf  = zipWith (metricFast prev) zf s

metricFast :: Pull1 IntN -> (Data Index, Data Index) -> Data IntN -> Data IntN
metricFast prev (z, _) _ = prev !! z
-- End one test.

copyPush :: Pull1 Index -> DPush DIM1 Index
copyPush v = let pv = toPush v in pv ++ pv

scanlPush :: SP.PushVector1 WordN -> S.Vector1 WordN -> SP.PushVector (SP.PushVector1 WordN)
scanlPush = SP.scanl const

concatV :: Pull DIM1 (Pull1 IntN) -> DPush DIM1 IntN
concatV xs = fromZero $ fold (++) empty xs

loadFun ['concatV]

concatVM :: Pull DIM1 (Pull1 IntN) -> Manifest DIM1 (Data IntN)
concatVM xs = fromZero $ fold (\l r -> store $ l ++ r) (store empty) xs

loadFun ['concatVM]

complexWhileCond :: Data Int32 -> (Data Int32, Data Int32)
complexWhileCond y = whileLoop (0, y) (\(a, b) -> (\a b -> a * a < b * b) a (b - a)) (\(a, b) -> (a + 1 ,b))

-- One test starting
divConq3 :: Pull DIM1 (Data IntN) -> DPush DIM1 IntN
divConq3 xs = concatV $ pmap (map (+1)) (segment 1024 xs)

pmap :: (Syntax a, Syntax b) => (a -> b) -> Pull DIM1 a -> Pull DIM1 b
pmap f = map await . force . map (future . f)

-- Note. @segment@ expects the length of @xs@ to be a multiple of @l@
segment :: Syntax a => Data Length -> Pull DIM1 a -> Pull DIM1 (Pull DIM1 a)
segment l xs = indexed1 clen (\ix -> take l $ drop (ix * l) xs)
  where clen = length xs `div` l

loadFun ['divConq3]

-- End one test.

-- | We rewrite `return x >>= \_ -> return y` into `return x >> return y`
--   This test ensures that we can still `return x` in the first action.
bindToThen :: Data Index -> Data Index
bindToThen y = runMutable $ do
    ref <- newRef y
    _ <- getRef ref
    getRef ref

switcher :: Data Word8 -> Data Bool -> Data Word8
switcher i = switch (value 0) [(True,i), (False,2)]

ivartest :: Data Index -> Data Index
ivartest a = share (future (a+1)) $ \a' -> await a' * 2

ivartest2 :: (Data Index, Data Index) -> (Data Index, Data Index)
ivartest2 a = share (future a) $ \a' -> await a'

arrayInStruct :: Data [Length] -> Data [Length]
arrayInStruct a = snd $ whileLoop (getLength a, a) (\(n, _) -> n > 0) (\(n, a) -> (n - 1, parallel (getLength a) (\i -> a!i + 5)))

arrayInStructInStruct :: Data (Length, (Length, [Length])) -> Data (Length, (Length, [Length]))
arrayInStructInStruct x = x

fut1 :: Future (Data IntN) -> Future (Data IntN)
fut1 x  = forLoop 20 x (\_ e -> future $ force $ await e)

not1 :: Data Bool -> Data Bool
not1 x = not x

issue128_ex1 :: Data WordN -> Data WordN
issue128_ex1 a = share (switch 45 [(1,10)] a) $ \b -> 1 == a ? b $ a

issue128_ex2 :: Data WordN -> Data WordN
issue128_ex2 a = share (switch 45 [(1,20)] a) $ \b -> 2 == a ? b $ a

issue128_ex3 :: Data WordN -> Data WordN
issue128_ex3 a = switch 45 [(1,10)] a + (2==a ? 2 $ a)

-- | Test that noinline is respected
noinline1 :: Data Bool -> Data Bool
noinline1 x = noInline $ not x

-- | Test that foreign imports with result type `M ()` can be used as monadic
--   actions. This expression cannot be created from the Feldspar front end.
foreignEffect :: UT.UntypedFeld ValueInfo
foreignEffect =
    UT.In (topInfo void) $ UT.App UT.Then void
        [ alert
        , UT.In (topInfo void) $ UT.App UT.Bind void
            [ getPos
            , UT.In (topInfo void) $
               UT.Lambda pos $ UT.In (topInfo void) $ UT.App UT.Then void
                  [ launchMissiles
                  , cleanUp
                  ]
            ]
        ]
  where
    void   = UT.MutType (UT.TupType [])
    pos    = UT.Var 77 posType B.empty
    alert  = UT.In (topInfo void) $ UT.App (UT.ForeignImport "alert") void []
    posType = 1 UT.:# UT.FloatType
    getPos = UT.In (topInfo posType) $
               UT.App (UT.ForeignImport "getPos") posType []
    launchMissiles =
      UT.In (topInfo void) $
        UT.App (UT.ForeignImport "launchMissiles") void
          [UT.In (topInfo posType) $ UT.Variable pos]
    cleanUp = UT.In (topInfo void) $
                UT.App (UT.ForeignImport "cleanUp") void []

tuples :: Data Int32 -> Data Int32
tuples a =
    share (a,a*3) $ \(a1,a2) ->
      share (a1+a2,a2+a1,a2) $ \(b1,b2,b3) ->
        share (b1+b2,b2+b3,b3+b1,b3) $ \(c1,c2,c3,c4) ->
          share (c1+c2,c2+c3,c3+c4,c4+c1,c4) $ \(d1,d2,d3,d4,d5) ->
            share (d1+d2,d2+d3,d3+d4,d4+d5,d5+d1,d5) $ \(e1,e2,e3,e4,e5,e6) ->
              share (e1+e2,e2+e3,e3+e4,e4+e5,e5+e6,e6+e1,e6) $ \(f1,f2,f3,f4,f5,f6,f7) ->
                share (f1+f2,f2+f3,f3+f4,f4+f5,f5+f6,f6+f7,f7+f1,f1+f2,f2+f3,f3+f4,f4+f5,f5+f6,f6+f7,f7+f1,f7*f1) $ \(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15) ->
                  g1+g2+g3+g4+g5+g6+g7+g8+g9+g10+g11+g12+g13+g14+g15

loadFun ['tuples]

deepArrayCopy :: Data [[[Length]]] -> (Data [[[Length]]], Data [[[Length]]])
deepArrayCopy xs = (xs, xs)

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
selectT = nfst $ snd noshareT

prop_concatV = forAll (vectorOf 3 (choose (0,5))) $ \ls ->
                 forAll (mapM (`vectorOf` arbitrary) ls) $ \xss ->
                   Prelude.concat xss === c_concatV xss

prop_concatVM = forAll (vectorOf 3 (choose (0,5))) $ \ls ->
                  forAll (mapM (`vectorOf` arbitrary) ls) $ \xss ->
                    Prelude.concat xss === c_concatVM xss

prop_divConq3 = forAll (choose (1,3)) $ \l ->
                  forAll (vectorOf (l*1024) arbitrary) $ \xs ->
                    map (+1) xs === c_divConq3 xs

-- | Arbitrary instances for nested tuples
instance Arbitrary (Tuple '[]) where
  arbitrary = return TNil

instance (Arbitrary a, Arbitrary (Tuple b)) => Arbitrary (Tuple (a ': b)) where
  arbitrary = do a <- arbitrary
                 b <- arbitrary
                 return (a :* b)

pairArg :: (Data Word8, Data IntN) -> Data IntN
pairArg (a, b) = i2n a + b

pairRes :: Data Word16 -> (Data WordN, Data IntN)
pairRes a = (i2n a, i2n a)

vecId :: Pull1 Word32 -> Pull1 Word32
vecId = id

vectorInPair :: (Pull1 WordN, Data WordN) -> (Data WordN, Pull1 WordN)
vectorInPair (v, a) = (a, v)

vectorInVector :: Pull DIM1 (Pull1 WordN) -> Data WordN
vectorInVector v = fromZero $ sum $ map (fromZero . sum) v

vectorInPairInVector :: Data WordN -> Pull DIM1 (Data WordN, Pull1 WordN)
vectorInPairInVector l = indexed1 l $ \i -> (i, indexed1 i id)

shTest :: Data Length -> Data Length
shTest n = runMutable $ do
             a <- newArr n 1
             c <- newArr n 2
             let d = n < 5 ? a $ c
             setArr d 0 n
             getArr a 0

loadFun ['pairArg]
loadFun ['pairRes]
loadFun ['vecId]
loadFun ['vectorInPair]
loadFun ['vectorInVector]
loadFun ['vectorInPairInVector]
loadFun ['shTest]
loadFun ['arrayInStruct]
loadFun ['pairParam]
loadFun ['pairParam2]
loadFun ['pairRet]
loadFun ['copyPush]
loadFun ['complexWhileCond]
loadFun ['deepArrayCopy]

prop_pairArg = eval pairArg ==== c_pairArg
prop_pairRes = eval pairRes ==== c_pairRes
prop_vecId (Small l) =
    forAll (vector1D l arbitrary) $ \xs ->
      eval vecId xs ==== c_vecId xs
prop_vectorInPair (Small l) =
    forAll (twotup <$> vector1D l arbitrary <*> arbitrary) $ \p ->
      eval vectorInPair p ==== c_vectorInPair p
prop_vectorInVector (Small l2) =
    forAll (choose (1, 3)) $ \l1 ->
      forAll (vector1D l1 (vector1D l2 arbitrary)) $ \v ->
        eval vectorInVector v ==== c_vectorInVector v
prop_vectorInPairInVector (Small l) = eval vectorInPairInVector l ==== c_vectorInPairInVector l
prop_shTest (Positive n) = eval shTest n ==== c_shTest n
prop_deepArrayCopy (Small l2) =
    forAll (choose (1, 3)) $ \l1 ->
      forAll (vector1D l1 (vector1D l2 arbitrary)) $ \xs ->
        eval deepArrayCopy xs ==== c_deepArrayCopy xs

prop_arrayInStruct =
  forAll (choose (0, 5)) $ \l ->
    forAll (vector1D l arbitrary) $ \xs ->
      eval arrayInStruct xs ==== c_arrayInStruct xs
prop_pairParam = eval pairParam ==== c_pairParam
prop_pairParam2 = eval pairParam2 ==== c_pairParam2
prop_pairRet = eval pairRet ==== c_pairRet
prop_copyPush = eval copyPush ==== c_copyPush
prop_complexWhileCond (NonNegative n) = eval complexWhileCond n ==== c_complexWhileCond n

-- | All tests to run
tests :: TestTree
tests = testGroup "RegressionTests" $
                  [ decorationTests, callingConventionTests, compilerTests
                  , externalProgramTests, mutableTests, streamTests
                  , vectorTests, tupleTests
                  ] Prelude.++ rangeTests

-- | Tests for decoration
decorationTests :: TestTree
decorationTests = testGroup "DecorationTests"
    [ goldenVsFile "example9" (goldDir <> "example9.txt") "tests/example9.txt"
      $ writeFile "tests/example9.txt" $ showDecor example9
    , goldenVsFile "topLevelConsts" (goldDir <> "topLevelConsts.txt") "tests/topLevelConsts.txt"
      $ writeFile "tests/topLevelConsts.txt" $ showDecor topLevelConsts
    , goldenVsFile "monadicSharing" (goldDir <> "monadicSharing.txt") "tests/monadicSharing.txt"
      $ writeFile "tests/monadicSharing.txt" $ showDecor monadicSharing
    , goldenVsFile "trickySharing" (goldDir <> "trickySharing.txt") "tests/trickySharing.txt"
      $ writeFile "tests/trickySharing.txt" $ showDecor trickySharing
    , goldenVsFile "noshareT" (goldDir <> "noshareT.txt") "tests/noshareT.txt"
      $ writeFile "tests/noshareT.txt" $ showDecor noshareT
    , goldenVsFile "shareT" (goldDir <> "shareT.txt") "tests/shareT.txt"
      $ writeFile "tests/shareT.txt" $ showDecor shareT
    , goldenVsFile "selectT" (goldDir <> "selectT.txt") "tests/selectT.txt"
      $ writeFile "tests/selectT.txt" $ showDecor selectT
    , goldenVsFile "tfModel" (goldDir <> "tfModel.txt") "tests/tfModel.txt"
      $ writeFile "tests/tfModel.txt" $ showUntyped defaultOptions tfModel
    ]

-- | Tests for calling convention
callingConventionTests :: TestTree
callingConventionTests = testGroup "CallingConvention"
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
    , testProperty "pairRet" prop_pairRet
    , testProperty "copyPush" prop_copyPush
    , testProperty "complexWhileCond" prop_complexWhileCond
    , testProperty "deepArrayCopy" prop_deepArrayCopy
    ]

compilerTests :: TestTree
compilerTests = testGroup "Compiler-RegressionTests"
    [ testProperty "example9 (plugin)" $ eval example9 ==== c_example9
    , testProperty "concatV (plugin)" prop_concatV
    , testProperty "concatVM (plugin)" prop_concatVM
    -- FIXME: divConq3 fails because the Feldspar runtime isn't included
    --        in the symbol loading.
    -- , testProperty "divConq3 (plugin)" prop_divConq3
    , testProperty "bindToThen" (\y -> eval bindToThen y === y)
    , testProperty "tuples (plugin)" $ eval tuples ==== c_tuples
    , mkGoldTest example9 "example9" defaultOptions
    , mkGoldTest pairParam "pairParam" defaultOptions
    , mkGoldTest pairParam "pairParam_ret" nativeRetOpts
    , mkGoldTest pairParam2 "pairParam2" defaultOptions
    , mkGoldTest pairRet "pairRet" defaultOptions
    , mkGoldTest concatV "concatV" defaultOptions
    , mkGoldTest concatVM "concatVM" defaultOptions
    , mkGoldTest complexWhileCond "complexWhileCond" defaultOptions
    , mkGoldTest topLevelConsts "topLevelConsts" defaultOptions
    , mkGoldTest topLevelConsts "topLevelConsts_native" nativeOpts
    , mkGoldTest metrics "metrics" defaultOptions
    , mkGoldTest scanlPush "scanlPush" defaultOptions
    , mkGoldTest divConq3 "divConq3" defaultOptions
    , mkGoldTest switcher "switcher" defaultOptions
    , mkGoldTest ivartest "ivartest" defaultOptions
    , mkGoldTest ivartest2 "ivartest2" defaultOptions
    , mkGoldTest arrayInStruct "arrayInStruct" defaultOptions
    , mkGoldTest arrayInStruct "arrayInStruct_wool" woolOpts
    , mkGoldTest arrayInStruct "arrayInStruct_openMP" openMPOpts
    , mkGoldTest arrayInStructInStruct "arrayInStructInStruct" defaultOptions
    , mkGoldTest fut1 "fut1" defaultOptions
    , mkGoldTest fut1 "fut1_ret" nativeRetOpts
    , mkGoldTest not1 "not1" defaultOptions
    , mkGoldTest not1 "not1_ret" nativeRetOpts
    , mkGoldTest issue128_ex1 "issue128_ex1" defaultOptions
    , mkGoldTest issue128_ex2 "issue128_ex2" defaultOptions
    , mkGoldTest issue128_ex3 "issue128_ex3" defaultOptions
    , mkGoldTest noinline1 "noinline1" defaultOptions
    , mkGoldTestUT foreignEffect "foreignEffect" defaultOptions
    , mkGoldTest tuples "tuples" defaultOptions
    , mkGoldTest deepArrayCopy "deepArrayCopy" defaultOptions
   -- Build tests.
    , mkBuildTest pairParam "pairParam" defaultOptions
    , mkBuildTest pairParam "pairParam_ret" nativeRetOpts
    , mkBuildTest pairRet "pairRet" defaultOptions
    , mkBuildTest concatV "concatV" defaultOptions
    , mkBuildTest concatVM "concatVM" defaultOptions
    , mkBuildTest topLevelConsts "topLevelConsts" defaultOptions
    , mkBuildTest topLevelConsts "topLevelConsts_native" nativeOpts
    , mkBuildTest metrics "metrics" defaultOptions
    , mkBuildTest copyPush "copyPush" defaultOptions
    , mkBuildTest scanlPush "scanlPush" defaultOptions
    , mkBuildTest divConq3 "divConq3" defaultOptions
    , mkBuildTest ivartest "ivartest" defaultOptions
    , mkBuildTest ivartest2 "ivartest2" defaultOptions
    , mkBuildTest arrayInStruct "arrayInStruct" defaultOptions
    , mkBuildTest arrayInStructInStruct "arrayInStructInStruct" defaultOptions
    , mkBuildTest fut1 "fut1" defaultOptions
    , mkBuildTest fut1 "fut1_ret" nativeRetOpts
    , mkBuildTest not1 "not1" defaultOptions
    , mkBuildTest not1 "not1_ret" nativeRetOpts
    , mkBuildTest issue128_ex1 "issue128_ex1" defaultOptions
    , mkBuildTest issue128_ex2 "issue128_ex2" defaultOptions
    , mkBuildTest issue128_ex3 "issue128_ex3" defaultOptions
    , mkBuildTest noinline1 "noinline1" defaultOptions
    , mkBuildTest tuples "tuples" defaultOptions
    , mkBuildTest deepArrayCopy "deepArrayCopy" defaultOptions
    ]

externalProgramTests :: TestTree
externalProgramTests = testGroup "ExternalProgram-RegressionTests"
    [ mkParseTest "example9" defaultOptions
    , mkParseTest "pairParam" defaultOptions
    , mkParseTest "pairParam_ret" defaultOptions
    , mkParseTest "pairParam2" defaultOptions
    , mkParseTest "pairRet" defaultOptions
    , mkParseTest "concatV" defaultOptions
    , mkParseTest "concatVM" defaultOptions
    , mkParseTest "complexWhileCond" defaultOptions
    , mkParseTest "topLevelConsts" defaultOptions
    , mkParseTest "topLevelConsts_native" nativeOpts
    -- TODO: Enable with a typed array representation (both tests).
    -- , mkParseTest "metrics" defaultOptions
    -- , mkParseTest "scanlPush" defaultOptions
    , mkParseTest "divConq3" defaultOptions
    , mkParseTest "switcher" defaultOptions
    , mkParseTest "ivartest" defaultOptions
    , mkParseTest "ivartest2" defaultOptions
    , mkParseTest "arrayInStruct" defaultOptions
    , mkParseTest "arrayInStructInStruct" defaultOptions
    , mkParseTest "not1" defaultOptions
    , mkParseTest "not1_ret" defaultOptions
    ]

-- | The main function
main :: IO ()
main = do
#ifdef mingw32_HOST_OS
  -- Set the codepage to a UTF-8 capable one. This is required for the
  -- non-ASCII characters in the decoration tests.
  -- The WinIO manager in GHC 8.12 will remove the need for this workaround.
  cp <- getConsoleCP
  setConsoleCP 65001
#endif
  defaultMain tests
   `catch` (\(e :: ExitCode)-> do
#ifdef mingw32_HOST_OS
    setConsoleCP cp -- Restore codepage before exit.
#endif
    throwIO e)

-- Helper functions
testDir, goldDir :: Prelude.FilePath
testDir = "tests/"
goldDir = "tests/gold/"

nativeOpts :: Options
nativeOpts = defaultOptions{useNativeArrays=True}
nativeRetOpts :: Options
nativeRetOpts = defaultOptions{useNativeReturns=True}
woolOpts :: Options
woolOpts = c99WoolPlatformOptions
openMPOpts :: Options
openMPOpts = c99OpenMpPlatformOptions

-- | Make a golden test for a Feldspar expression
mkGoldTest :: Syntactic a => a -> Prelude.FilePath -> Options -> TestTree
mkGoldTest fun n opts = do
    let ref = goldDir <> n
        new = testDir <> n
        act = compile fun n opts{outFileName = new}
        cmp = simpleCmp $ printf "Files '%s.{c,h}' and '%s.{c,h}' differ" ref new
        upd = LB.writeFile ref
    goldenTest n (vgReadFiles ref) (liftIO act >> vgReadFiles new) cmp upd

-- | Make a golden test for an untyped Feldspar expression
mkGoldTestUT :: UT.UntypedFeld ValueInfo -> Prelude.FilePath -> Options -> TestTree
mkGoldTestUT untyped n opts = do
    let ref = goldDir <> n
        new = testDir <> n
        act = compileUT untyped n opts{outFileName = new}
        cmp = simpleCmp $ printf "Files '%s.{c,h}' and '%s.{c,h}' differ" ref new
        upd = LB.writeFile ref
    goldenTest n (vgReadFiles ref) (liftIO act >> vgReadFiles new) cmp upd

-- | Compares two outputs with @(==)@
simpleCmp :: Prelude.Eq a => String -> a -> a -> IO (Maybe String)
simpleCmp e x y =
  return $ if x Prelude.== y then Nothing else Just e

mkParseTest :: Prelude.FilePath -> Options -> TestTree
mkParseTest n opts = do
    let ref = goldDir <> n
        new = testDir <> "ep-" <> n
        act = compileFile ref opts{outFileName = new}
        cmp = fuzzyCmp $ printf "Files '%s.{c,h}' and '%s.{c,h}' differ" ref new
        upd = LB.writeFile ref
    goldenTest n (vgReadFiles ref) (liftIO act >> vgReadFiles new) cmp upd

-- | Compares two outputs up to "EP-"-related prefixes
fuzzyCmp :: String -> LB.ByteString -> LB.ByteString -> IO (Maybe String)
fuzzyCmp e x y =
  return $ if x Prelude.== filterEp y then Nothing else Just e

-- | Removes "EP-"-related prefixes from the generated output.
filterEp :: LB.ByteString -> LB.ByteString
filterEp xs = LB.replace (B.pack "TESTS_EP-") (B.pack "TESTS_") xs'
  where xs' = LB.replace (B.pack "#include \"ep-") (B.pack "#include \"") xs

-- | Make a build test for a Feldspar expression
mkBuildTest :: Syntactic a => a -> Prelude.FilePath -> Options -> TestTree
mkBuildTest fun n opts = do
    let new = testDir <> n <> "_build_test"
        cfile = new <> ".c"
        act = do compile fun n opts{outFileName = new}
                 let ghcArgs = [cfile, "-c", "-optc -Isrc/clib/include", "-optc -std=c99", "-Wall"]
                 (ex, stdout, stderr) <- readProcessWithExitCode ghc ghcArgs ""
                 case ex of
                   ExitFailure{} ->
                    Prelude.error $ Prelude.unlines [show ex, stdout, stderr]
                   _ -> return ()
        cmp _ _ = return Nothing
        upd _ = return ()
    goldenTest n (return "") (liftIO act >> return "") cmp upd
