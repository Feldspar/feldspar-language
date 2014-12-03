{-# LANGUAGE TemplateHaskell #-}

module Feldspar.Stream.Test where



import qualified Prelude as P
import qualified Data.List as List

import Feldspar
import Feldspar.Stream
import Feldspar.Vector

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.QuickCheck

import Test.QuickCheck



scProd :: Num a => [a] -> [a] -> a
scProd a b = P.sum $ P.zipWith (*) a b



-- Reference implementation of FIR filter
firRef :: Num a => [a] -> [a] -> [a]
firRef coeffs inp = [scProd coeffs is | is <- P.map P.reverse $ P.tail $ List.inits inp]

firFeld :: [Int32] -> [Int32] -> [Int32]
firFeld coeffs = eval (freezePull1 . streamAsVector (fir (toPull $ value1 coeffs)) . thawPull1)

prop_fir (NonEmpty coeffs) = firRef coeffs ==== firFeld coeffs



-- Reference implementation of IIR filter
iirRef :: Num a => [a] -> [a] -> [a] -> [a]
iirRef as bs inp = outp
  where
    inps  = P.map P.reverse $ P.tail $ List.inits inp
    outps = P.map P.reverse $ P.tail $ List.inits (0:outp)

    outp = [scProd bs is - scProd as os | (is,os) <- P.zip inps outps]

-- | Same as 'iir' in "Feldspar.Stream", but without the fractional constraint, to avoid rounding
-- errors when testing
iirInt :: Numeric a => Pull1 a -> Pull1 a -> Stream (Data a) -> Stream (Data a)
iirInt a b inp =
    recurrenceIO (replicate1 (length b) 0) inp
                 (replicate1 (length a) 0)
                 (\i o -> (scalarProd b i - scalarProd a o))

iirFeld :: [Int32] -> [Int32] -> [Int32] -> [Int32]
iirFeld as bs = eval (freezePull1 . iirVec . thawPull1)
  where
    iirVec = streamAsVector (iirInt (toPull $ value1 as) (toPull $ value1 bs))

prop_iir (NonEmpty as) (NonEmpty bs) = iirRef as bs ==== iirFeld as bs



tests = $(testGroupGenerator)

