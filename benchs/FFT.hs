{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Feldspar (Length, Complex)
import Feldspar.Algorithm.FFT
import Feldspar.Compiler
import Feldspar.Compiler.Plugin
import Feldspar.Compiler.Marshal

import Foreign.Ptr
import Foreign.Marshal (new)
import Control.DeepSeq (NFData(..))
import Control.Exception (evaluate)

import BenchmarkUtils
import Criterion.Main

testdata :: [Complex Float]
testdata = cycle [1,2,3,4]

loadFunOpts ["-optc=-O2"] ['fft]
loadFunOpts ["-optc=-O2"] ['ifft]

len :: Length
len = 4096

sizes :: [[Length]]
sizes = map (map (*len)) [[1],[2],[4],[8]]

setupPlugins :: IO ()
setupPlugins = do
    _ <- evaluate c_fft_builder
    return ()

setupData :: [Length] -> IO (Ptr (SA (Complex Float)), Ptr (SA (Complex Float)))
setupData lengths = do
    d <- mkData testdata lengths
    ds <- allocSA $ fromIntegral $ product lengths :: IO (Ptr (SA (Complex Float)))
    return (ds, d)

mkComp :: [Length] -> Benchmark
mkComp ls = env (setupData ls) $ \ ~(o, d) ->
    mkBench "c_fft" ls (whnfIO $ c_fft_raw d o)

main :: IO ()
main = defaultMainWith (mkConfig "report_fft.html")
    [ env setupPlugins $ \_ -> bgroup "compiled" $ map mkComp sizes
    ]
