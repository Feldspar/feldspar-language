{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Feldspar.Algorithm.FFT.Push
  ( fft
  , butterfly
  , twid
  , itwids
  , twids
  ) where


import qualified Prelude as P
import qualified Data.Complex

import Feldspar
import Feldspar.Mutable
import Feldspar.Vector

import Feldspar.Algorithm.FFT.Utils
import Feldspar.Algorithm.FFT.Twids

import Test.QuickCheck



-- | Utilities that should go into Feldspar.Vector
chnk :: (Pushy arr1 DIM1, Syntax b)
      => Data Length          -- ^ Number of chunks
      -> Data Length          -- ^ Size of the chunks
      -> (Pull DIM1 a -> arr1 DIM1 b) -- ^ Applied to every chunk
      -> Pull DIM1 a
      -> Push DIM1 b
chnk r c f v = Push loop $ extent v
  where loop func = forM r $ \i ->
                      do let (Push k _) = toPush $ f (take c (drop (c*i) v))
                         k (\(Z:.j) a -> func (Z:.(c*i + j)) a)

unhalve :: (Syntax a)
        => Pull DIM1 (a,a) -> Push DIM1 a
unhalve xs = unpairWith id (\(Z:.i) -> Z:.(i + length xs)) xs

stride :: Data Length -> Data Length
       -> (Data Index -> a -> M b)
       -> Data Index -> (a,a) -> M b
stride n k f ix (a1,a2) = f (n*ix) a1 >> f (n*ix+k) a2


-- | DFT2 for Decimation-In-Frequency
dft2 :: Num a => a -> (a, a) -> (a,a)
dft2 w (x0,x1) = (x0+x1, (x0-x1)*w)

butterfly :: (Syntax a, Num a)
          => (a -> (a,a) -> (a,a))
          -> Pull DIM1 a -> Pull DIM1 a -> Push DIM1 a
butterfly f ws = unhalve . zipWith f ws . uncurry zip . halve

-- | Cooley-Tukey Radix-2 Decimation In Frequency Fast Fourier Transfrom
fft :: (Syntax a, Num a)
    => Pull DIM1 a -> Pull DIM1 a -> Pull DIM1 a
fft ws vs = forLoop (ilog2 len) vs stage
  where
    len = length vs
    stage s = withLen len
            $ toPull
            . store
            . chnk (1 .<<. s) (len .>>. s) (butterfly dft2 (ixmap (.<<. s) ws))

