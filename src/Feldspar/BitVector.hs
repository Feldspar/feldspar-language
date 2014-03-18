{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

--
-- Copyright (c) 2009-2011, ERICSSON AB
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright notice,
--       this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in the
--       documentation and/or other materials provided with the distribution.
--     * Neither the name of the ERICSSON AB nor the names of its contributors
--       may be used to endorse or promote products derived from this software
--       without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

-- | A 'Vector' interface to packed sequences of bits
--
module Feldspar.BitVector where

import qualified Prelude
import Data.Word
import Data.List (inits)
import Data.Proxy

import Language.Syntactic hiding (fold)

import Feldspar.Prelude
import Feldspar hiding (sugar, desugar, resugar)
import qualified Feldspar.Vector as Vec

-- * Types and classes

-- | A 'Unit' is the internal representation of a 'BitVector'
class (Type w, Numeric w, Bits w, Integral w) => Unit w
  where
    width :: Proxy w -> Length

instance Unit Word8
  where
    width _ = 8

instance Unit Word16
  where
    width _ = 16

instance Unit Word32
  where
    width _ = 32

data BitVector w
     = BitVector
     { segments         :: [Segment w]
     }

data Segment w
    = Segment
    { numUnits  :: Data Length
    , elements  :: Data Index -> Data w
    }

-- * Feldspar integration of BitVector

type instance Elem      (BitVector w) = Data Bool
type instance CollIndex (BitVector w) = Data Index
type instance CollSize  (BitVector w) = Data Length

instance (Unit a) => Syntactic (BitVector a)
  where
    type Domain (BitVector a)   = FeldDomain
    type Internal (BitVector a) = [a]
    desugar = desugar . freezeBitVector
    sugar   = unfreezeBitVector . sugar

-- * Operations

length :: forall w . (Unit w) => BitVector w -> Data Length
length bv = Prelude.sum $ Prelude.map segmentLen $ segments bv
  where
    segmentLen s = numUnits s * w
    w = value $ width (Proxy :: Proxy w)

numOfUnits :: (Unit w) => BitVector w -> Data Length
numOfUnits bv = Prelude.sum $ Prelude.map numUnits $ segments bv

freezeBitVector :: forall w . (Unit w) => BitVector w -> Data [w]
freezeBitVector bv = freezeSegments $ segments bv
  where
    freezeSegments segs = case segs of
        []      -> value []
        (s:ss)  -> parallel (numUnits s) (elements s) `append` freezeSegments ss

unfreezeBitVector :: forall w . (Unit w) => Data [w] -> BitVector w
unfreezeBitVector ws = BitVector [Segment (getLength ws) (ws!)]

{- TODO
-- | Variant of `unfreezeBitVector` with additional static size information.
unfreezeBitVector' :: forall w . (Unit w) => Length -> Data [w] -> BitVector w
unfreezeBitVector' len arr = unfreezeBitVector $ cap (r :> elemSize) arr
  where
    (_ :> elemSize) = dataSize arr
    singleton :: a -> Range a
    singleton x = Range x x
    r = (singleton (fromIntegral len),singleton (fromIntegral len)
        ,singleton (fromIntegral len))
-}

-- | Transforms a bool vector to a bitvector.
-- Length of the vector has to be divisible by the wordlength,
-- otherwise booleans at the end will be dropped.
fromVector :: forall w . (Unit w, Size w ~ Range w) => Vec.Pull1 Bool -> BitVector w
fromVector v = BitVector
    { segments = [Segment wl (loop w')]
        -- TODO: Should Vector segments be transformed to BitVector segments
        -- for the sake of efficiency?
    }
  where
    w' = value $ width (Proxy :: Proxy w)
    wl = Vec.length v `div` w'
    loop n ix = forLoop n 0 $ \i st ->
        st `shiftLU` 1 .|. (v Vec.!! (w' * ix + i) ? 1 $ 0)

toVector :: forall w . (Unit w, Size w ~ Range w) => BitVector w -> Vec.Pull1 Bool
toVector bv = Vec.indexed1 (length bv) (bv!)

instance (Unit w, Size w ~ Range w) => Indexed (BitVector w)
  where
    bv ! i = help 0 (segments bv)
      where
        help _      [] = false
            -- XXX Should be an error here...
        help accum [s] = ixf s accum i
        help accum (s:ss) = (i < accum + numUnits s * w)
            ? ixf s accum i
            $ help (accum + numUnits s * w) ss
        w = value $ width (Proxy :: Proxy w)
        ixf s accum ix = testBit (elements s ((ix - accum) `div` w)) (w - 1 - ((ix - accum) `mod` w))

fromBits :: forall w . (Unit w) => [Bool] -> BitVector w
fromBits bs = unfreezeBitVector $ value xs
  where
    xs = [ conv (Proxy :: Proxy w) $ Prelude.take w (Prelude.drop (i*w) bs) | i <- [0..Prelude.length bs `Prelude.div` w Prelude.- 1]]
    w = fromInteger $ toInteger $ width (Proxy :: Proxy w)
    conv :: (Unit w) => Proxy w -> [Bool] -> w
    conv _ = Prelude.foldl (\n b -> if b then n Prelude.* 2 Prelude.+ 1 else n Prelude.* 2) 0

fromUnits :: (Unit w) => [w] -> BitVector w
fromUnits = unfreezeBitVector . value

replUnit :: (Unit w) => Data Length -> w -> BitVector w
replUnit n u = BitVector [Segment n $ const $ value u]

indexed :: (Unit w, Size w ~ Range w) =>
    Data Length -> (Data Index -> Data Bool) -> BitVector w
indexed l ixf = fromVector $ Vec.indexed1 l ixf

map :: (Unit w, Size w ~ Range w) =>
    (Data Bool -> Data Bool) -> BitVector w -> BitVector w
map f bv = boolFun1 f res
  where
    res f' = BitVector $
        Prelude.map (\s -> s{elements = f' . elements s}) $ segments bv

takeUnits :: forall w . (Unit w) =>
    Data Length -> BitVector w -> BitVector w
takeUnits len bv = help len [] $ segments bv
  where
    help _ acc [] = BitVector acc
    help n acc (s:ss) = (n < numUnits s)
        ? BitVector (acc Prelude.++ [s{numUnits = n}])
        $ help (n - numUnits s) (acc Prelude.++ [s]) ss

dropUnits :: forall w . (Unit w) =>
    Data Length -> BitVector w -> BitVector w
dropUnits len bv = help len $ segments bv
  where
    help _ [] = BitVector []
    help n (s:ss) = (n < numUnits s)
        ? BitVector (s':ss)
        $ help (n - numUnits s) ss
      where
        s' = Segment
            { numUnits = numUnits s - n
            , elements = \i -> elements s (i + n)
            }

(++) :: forall w . (Unit w) =>
    BitVector w -> BitVector w -> BitVector w
(BitVector ss) ++ (BitVector zs) = BitVector $ ss Prelude.++ zs

drop :: forall w . (Unit w, Size w ~ Range w) =>
    Data Length -> Data w -> BitVector w -> BitVector w
drop len end bv = dropSegments len $ segments bv
  where
    w = value $ width (Proxy :: Proxy w)
    dropSegments _ [] = BitVector []
    dropSegments n (s:ss) = (n < sLen)
        ? dropUnits n s ss
        $ dropSegments (n - sLen) ss
      where
        sLen = numUnits s * w
    dropUnits n s ss = dropBits bitsToDrop (s':ss)
      where
        s' = Segment
            { numUnits = numUnits s - wordsToDrop
            , elements = \i -> elements s (i + wordsToDrop)
            }
        wordsToDrop = n `div` w
        bitsToDrop = n `mod` w
    dropBits _ [] = BitVector []
    dropBits n (s:ss) = (n > 0)
        ? BitVector (s' : segments bv')
        $ BitVector (s:ss)
      where
        s' = Segment
            { numUnits = numUnits s - 1
            , elements = \i ->
                (elements s i `shiftLU` n)
                .|.
                (elements s (i+1) `shiftRU` (w-n))
            }
        bv' = addBits (w - n) (elements s (numUnits s - 1) `shiftLU` n) ss
    addBits n bs [] = BitVector [Segment 1 $ const $ bs .|. (end `shiftRU` n)]
    addBits n bs (s:ss) = (numUnits s > 0)
        ? BitVector (s' : segments bv')
        $ addBits n bs ss
      where
        s' = Segment
            { numUnits = 1
            , elements = const $ bs .|. (elements s 0 `shiftRU` n)
            }
        bv' = dropBits (w - n) (s:ss)

fold :: forall w a. (Syntax a, Unit w, Size w ~ Range w) =>
    (a -> Data Bool -> a) -> a -> BitVector w -> a
fold _ ini (BitVector []) = ini
fold f ini (BitVector (s:ss)) = fold f (forLoop (numUnits s) ini f') $ BitVector ss
  where
    f' :: Data Index -> a -> a
    f' i st = Prelude.snd $ forLoop w (elements s i, st) f''
    f'' :: Data Index -> (Data w,a) -> (Data w,a)
    f'' _ (unit,st) = (unit `shiftLU` 1, f st $ testBit unit $ w-1)
    w = value $ width (Proxy :: Proxy w)

zipWith :: forall w. (Unit w, Size w ~ Range w) =>
    (Data Bool -> Data Bool -> Data Bool)
    -> BitVector w
    -> BitVector w
    -> BitVector w
zipWith f bv bw = boolFun2 f res
  where
    res f' = Prelude.foldl (++) (BitVector [])
        [ zipSegments f' s z | s <- segIdxs bv, z <- segIdxs bw ]
    segIdxs bvec = Prelude.zip (segments bvec) $
        Prelude.map (Prelude.sum . Prelude.map numUnits) $
        inits $ segments bvec
    zipSegments f' (s,sStart) (z,zStart) = BitVector
        [ Segment
            { numUnits = end - start
            , elements = \i ->
                f' (elements s (i+sOffset)) (elements z (i+zOffset))
            }
        ]
      where
        sEnd = sStart + numUnits s
        zEnd = zStart + numUnits z
        start = max sStart zStart
        end = min sEnd zEnd
        sOffset = start - sStart
        zOffset = start - zStart

head :: (Unit w, Size w ~ Range w) => BitVector w -> Data Bool
head = (!0)

tail :: forall w. (Unit w, Size w ~ Range w) => Data Bool -> BitVector w -> BitVector w
tail b = drop 1 (b2i b `shiftLU` (w - 1))
  where
    w = value $ width (Proxy :: Proxy w)

-- * Boolean functions extended to words

boolFun1 :: (Syntax t, Unit w, Size w ~ Range w) =>
    (Data Bool -> Data Bool)
    -> ((Data w -> Data w) -> t)
    -> t
boolFun1 f c = f true
        ? (f false ? c (const $ complement 0) $ c id)
        $ (f false ? c complement $ c (const 0))

boolFun2 :: (Syntax t, Unit w, Size w ~ Range w) =>
    (Data Bool -> Data Bool -> Data Bool)
    -> ((Data w -> Data w -> Data w) -> t)
    -> t
boolFun2 f c =
    f true true
    ? ( f true false
        ? ( f false true
            ? ( f false false
                ? (c $ \_ _ -> complement 0)
                $ (c $ (.|.))
              )
            $ ( f false false
                ? (c $ \x y -> x .|. complement y)
                $ (c $ \x _ -> x)
              )
          )
        $ ( f false true
            ? ( f false false
                ? (c $ \x y -> complement x .|. y)
                $ (c $ \_ y -> y)
              )
            $ ( f false false
                ? (c $ \x y -> complement (x `xor` y))
                $ (c $ (.&.))
              )
          )
      )
    $ ( f true false
        ? (f false true
            ? ( f false false
                ? (c $ \x y -> complement (x .&. y))
                $ (c $ \x y -> x `xor` y)
              )
            $ ( f false false
                ? (c $ \_ y -> complement y)
                $ (c $ \x y -> x .&. complement y)
              )
          )
        $ ( f false true
            ? ( f false false
                ? (c $ \x _ -> complement x)
                $ (c $ \x y -> complement x .&. y)
              )
            $ ( f false false
                ? (c $ \x y -> complement (x .|. y))
                $ (c $ \_ _ -> 0)

              )
          )
      )

-- * Patch combinators for bitvectors

tBV :: Patch w w -> Patch (BitVector w) (BitVector w)
tBV _ = id
