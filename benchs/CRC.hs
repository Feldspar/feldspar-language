{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

import Feldspar
import Feldspar.Vector
import Feldspar.Compiler.Plugin
import Feldspar.Compiler.Marshal (SA(..))
import Feldspar.Algorithm.CRC

import Foreign.Ptr
import Foreign.Marshal (malloc)
import Control.DeepSeq (NFData(..),force)
import Control.Exception (evaluate)

import BenchmarkUtils
import Criterion.Main

len :: Length
len = 16 * 1024

testdata :: [Word8]
testdata = Prelude.take (fromIntegral len) $ cycle [1,2,3,4]

naive :: Pull1 Word8 -> Data Word16
naive = crcNaive 0x8005 0

normal :: Pull1 Word8 -> Data Word16
normal v = share (makeCrcTable 0x8005) $ \t -> crcNormal t 0 v

h_naive :: [Word8] -> Word16
h_naive = eval naive
loadFun ['naive]

h_normal :: [Word8] -> Word16
h_normal = eval normal
loadFun ['normal]

setupPlugins :: IO ()
setupPlugins = do
    putStrLn "Compiling plugins"
    _  <- evaluate c_naive_builder
    _  <- evaluate c_normal_builder
    return ()

setupData :: Length -> IO [Word8]
setupData l = return $ Prelude.take (fromIntegral l) testdata

setupRaw :: Length -> IO (Ptr Word16, Ptr (SA Word8))
setupRaw l = do
    o  <- malloc
    pd <- pack (Prelude.take (fromIntegral l) testdata)
    return (o, pd)

main :: IO ()
main =
    defaultMainWith (mkConfig "report_crc.html")
      [
        env (setupData 1024) $ \d ->
          bgroup "evaluated"
            [ bench "h_naive"  $ nf h_naive  d
            , bench "h_normal" $ nf h_normal d
            ]
      , env setupPlugins $ \_ -> bgroup "compiled"
          [ env (setupData len) $ \d -> bgroup "marshal"
              [ bench "c_naive"  $ whnfIO $ c_naive_worker d
              , bench "c_normal" $ whnfIO $ c_normal_worker d
              ]
          , env (setupRaw len) $ \ ~(o, pd) -> bgroup "raw"
              [ bench "c_naive"  $ whnfIO $ c_naive_raw pd o
              , bench "c_normal" $ whnfIO $ c_normal_raw pd o
              ]
          ]
      ]
