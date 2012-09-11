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

{-# LANGUAGE UndecidableInstances #-}

module Feldspar.BitVector where

import qualified Prelude
import qualified Data.TypeLevel
import Data.Int
import Data.Word
import Data.Typeable
import Data.List (inits)
import qualified Data.TypeLevel as TL

import Language.Syntactic

import Feldspar.Wrap
import Feldspar.Prelude
import Feldspar hiding (sugar, desugar, resugar)
import qualified Feldspar.Vector as Vec

-- * Types and classes

data T a = T
  -- TODO Use `Data.Proxy.Proxy` instead

class (Type w, Numeric w, Bits w, Integral w) => Unit w
  where
    width :: T w -> Length

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

instance (Unit a) => Syntactic (BitVector a) FeldDomainAll
  where
    type Internal (BitVector a) = [a]
    desugar = desugar . freezeBitVector
    sugar   = unfreezeBitVector . sugar

instance (Unit a) => Syntax (BitVector a)

-- * Operations

length :: forall w . (Unit w) => BitVector w -> Data Length
length bv = Prelude.sum $ Prelude.map segmentLen $ segments bv
  where
    segmentLen s = numUnits s * w
    w = value $ width (T :: T w)

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
fromVector :: forall w . (Unit w, Size w ~ Range w) => Vec.Vector (Data Bool) -> BitVector w
fromVector v = BitVector
    { segments = [Segment wl (loop w)]
        -- TODO: Should Vector segments be transformed to BitVector segments
        -- for the sake of efficiency?
    }
  where
    w = value $ width (T :: T w)
    wl = Vec.length v `div` w
    loop n ix = forLoop n 0 $ \i st ->
        st `shiftLU` 1 .|. (v ! (w * ix + i) ? (1,0))

toVector :: forall w . (Unit w, Size w ~ Range w) => BitVector w -> Vec.Vector (Data Bool)
toVector bv = Vec.indexed (length bv) (bv!)

instance (Unit w, Size w ~ Range w) => Indexed (BitVector w)
  where
    bv ! i = help 0 (segments bv)
      where
        help accum [] = false
            -- XXX Should be an error here...
        help accum [s] = bit s accum i
        help accum (s:ss) = i < accum + numUnits s * w ?
            ( bit s accum i
            , help (accum + numUnits s * w) ss
            )
        w = value $ width (T :: T w)
        bit s accum i = testBit (elements s ((i - accum) `div` w)) (w - 1 - ((i - accum) `mod` w))

fromBits :: forall w . (Unit w) => [Bool] -> BitVector w
fromBits bs = unfreezeBitVector $ value xs
  where
    xs = [ conv (T :: T w) $ Prelude.take w (Prelude.drop (i*w) bs) | i <- [0..Prelude.length bs `Prelude.div` w Prelude.- 1]]
    w = fromInteger $ toInteger $ width (T :: T w)
    conv :: (Unit w) => T w -> [Bool] -> w
    conv _ = Prelude.foldl (\n b -> if b then n Prelude.* 2 Prelude.+ 1 else n Prelude.* 2) 0

fromUnits :: (Unit w) => [w] -> BitVector w
fromUnits = unfreezeBitVector . value

replUnit :: (Unit w) => Data Length -> w -> BitVector w
replUnit n u = BitVector [Segment n $ const $ value u]

indexed :: (Unit w, Size w ~ Range w) =>
    Data Length -> (Data Index -> Data Bool) -> BitVector w
indexed l ixf = fromVector $ Vec.indexed l ixf

map :: (Unit w, Size w ~ Range w) =>
    (Data Bool -> Data Bool) -> BitVector w -> BitVector w
map f bv = boolFun1 f result
  where
    result f' = BitVector $
        Prelude.map (\s -> s{elements = f' . elements s}) $ segments bv

takeUnits :: forall w . (Unit w) =>
    Data Length -> BitVector w -> BitVector w
takeUnits len bv = help len [] $ segments bv
  where
    help n acc [] = BitVector acc
    help n acc (s:ss) = n < numUnits s ?
        ( BitVector (acc Prelude.++ [s{numUnits = n}])
        , help (n - numUnits s) (acc Prelude.++ [s]) ss
        )

dropUnits :: forall w . (Unit w) =>
    Data Length -> BitVector w -> BitVector w
dropUnits len bv = help len $ segments bv
  where
    help n [] = BitVector []
    help n (s:ss) = n < numUnits s ?
        ( BitVector $ s':ss
        , help (n - numUnits s) ss
        )
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
    w = value $ width (T :: T w)
    dropSegments n [] = BitVector []
    dropSegments n (s:ss) = n < sLen ?
        ( dropUnits n s ss
        , dropSegments (n - sLen) ss
        )
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
    dropBits n [] = BitVector []
    dropBits n (s:ss) = n > 0 ?
        ( BitVector $ s' : segments bv'
        , BitVector (s:ss)
        )
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
    addBits n bs (s:ss) = numUnits s > 0 ?
        ( BitVector $ s' : segments bv'
        , addBits n bs ss
        )
      where
        s' = Segment
            { numUnits = 1
            , elements = const $ bs .|. (elements s 0 `shiftRU` n)
            }
        bv' = dropBits (w - n) (s:ss)

fold :: forall w a. (Syntax a, Unit w, Size w ~ Range w) =>
    (a -> Data Bool -> a) -> a -> BitVector w -> a
fold f ini (BitVector []) = ini
fold f ini (BitVector (s:ss)) = fold f (forLoop (numUnits s) ini f') $ BitVector ss
  where
    f' :: Data Index -> a -> a
    f' i st = Prelude.snd $ forLoop w (elements s i, st) f''
    f'' :: Data Index -> (Data w,a) -> (Data w,a)
    f'' _ (unit,st) = (unit `shiftLU` 1, f st $ testBit unit $ w-1)
    w = value $ width (T :: T w)

zipWith :: forall w. (Unit w, Size w ~ Range w) =>
    (Data Bool -> Data Bool -> Data Bool)
    -> BitVector w
    -> BitVector w
    -> BitVector w
zipWith f bv bw = boolFun2 f result
  where
    result f' = Prelude.foldl (++) (BitVector [])
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
    w = value $ width (T :: T w)

-- * Boolean functions extended to words

boolFun1 :: (Syntax t, Unit w, Size w ~ Range w) =>
    (Data Bool -> Data Bool)
    -> ((Data w -> Data w) -> t)
    -> t
boolFun1 f c = f true ?
        ( f false ? (c (const $ complement 0), c id)
        , f false ? (c complement, c (const 0))
        )

boolFun2 :: (Syntax t, Unit w, Size w ~ Range w) =>
    (Data Bool -> Data Bool -> Data Bool)
    -> ((Data w -> Data w -> Data w) -> t)
    -> t
boolFun2 f c =
    f true true ?
    ( f true false ?
      ( f false true ?
        ( f false false ?
          ( c $ \a b -> complement 0
          , c $ \a b -> a .|. b
          )
        , f false false ?
          ( c $ \a b -> a .|. complement b
          , c $ \a _ -> a
          )
        )
      , f false true ?
        ( f false false ?
          ( c $ \a b -> complement a .|. b
          , c $ \a b -> b
          )
        , f false false ?
          ( c $ \a b -> complement (a `xor` b)
          , c $ \a b -> a .&. b
          )
        )
      )
    , f true false ?
      ( f false true ?
        ( f false false ?
          ( c $ \a b -> complement (a .&. b)
          , c $ \a b -> a `xor` b
          )
        , f false false ?
          ( c $ \a b -> complement b
          , c $ \a b -> a .&. complement b
          )
        )
      , f false true ?
        ( f false false ?
          ( c $ \a b -> complement a
          , c $ \a b -> complement a .&. b
          )
        , f false false ?
          ( c $ \a b -> complement (a .|. b)
          , c $ \a b -> 0
          )
        )
      )
    )

-- * Wrapping for bitvectors

instance (Unit w) => Wrap (BitVector w) (Data [w]) where
    wrap = freezeBitVector

instance (Wrap t u, Unit w, TL.Nat s) => Wrap (BitVector w -> t) (Data' s [w] -> u) where
    wrap f = \(Data' d) -> wrap $ f $ unfreezeBitVector $ setLength s' d where
        s' = fromInteger $ toInteger $ TL.toInt (undefined :: s)

-- * Patch combinators for bitvectors

tBV :: Patch w w -> Patch (BitVector w) (BitVector w)
tBV _ = id
