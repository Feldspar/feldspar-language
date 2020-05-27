{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

--
-- Copyright (c) 2019, ERICSSON AB
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

module Feldspar.Core.Semantics (Semantics(..), semantics) where

import Control.Monad
import Control.Monad.Par.Scheds.TraceInternal (yield)
import Data.Array.IArray
import Data.Array.IO
import Data.Array.MArray (freeze)
import Data.Array.Unsafe (unsafeFreeze)
import Data.Bits
import Data.Complex
import Data.Function (on)
import Data.IORef
import Data.List
import Data.Maybe
import Data.Typeable
import Feldspar.Core.Representation
import Feldspar.Core.Tuple
import Feldspar.Core.NestedTuples
import Feldspar.Core.Types
import qualified Control.Exception as C
import qualified Control.Monad.Par as CMP
import System.IO.Unsafe

data Semantics a = Sem {semStr :: String, semSem :: a}

semantics :: Op a -> Semantics a
-- Feldspar.Core.Constructs.Array
semantics Append    = Sem "(++)" (++)
semantics GetIx     = Sem "(!)" evalGetIx
semantics GetLength = Sem "getLength" genericLength
semantics SetLength = Sem "setLength"
        (\n as -> genericTake n (as ++ repeat err))
      where
        err = error "reading uninitialized array element"
semantics Parallel = Sem "parallel"
        (\len ixf -> genericTake len $ map ixf [0..])
semantics Sequential = Sem "sequential"
        (\len i step -> genericTake len $
                        snd $ mapAccumL (\a ix -> swap (step ix a)) i [0..])
      where swap (a,b) = (b,a)
semantics SetIx = Sem "setIx" evalSetIx
-- Feldspar.Core.Constructs.Binding
semantics Let = Sem "let" (flip ($))
-- Feldspar.Core.Constructs.Bits
semantics BAnd          = Sem "(.&.)"      (.&.)
semantics BOr           = Sem "(.|.)"      (.|.)
semantics BXor          = Sem "xor"        xor
semantics Complement    = Sem "complement" complement
semantics Bit           = Sem "bit"           (bit . fromIntegral)
semantics SetBit        = Sem "setBit"        (liftIntWord setBit)
semantics ClearBit      = Sem "clearBit"      (liftIntWord clearBit)
semantics ComplementBit = Sem "complementBit" (liftIntWord complementBit)
semantics TestBit       = Sem "testBit"       (liftIntWord testBit)
semantics ShiftLU       = Sem "shiftLU"     (liftIntWord shiftL)
semantics ShiftRU       = Sem "shiftRU"     (liftIntWord shiftR)
semantics ShiftL        = Sem "shiftL"      (liftInt shiftL)
semantics ShiftR        = Sem "shiftR"      (liftInt shiftR)
semantics RotateLU      = Sem "rotateLU"    (liftIntWord rotateL)
semantics RotateRU      = Sem "rotateRU"    (liftIntWord rotateR)
semantics RotateL       = Sem "rotateL"     (liftInt rotateL)
semantics RotateR       = Sem "rotateR"     (liftInt rotateR)
semantics ReverseBits   = Sem "reverseBits" evalReverseBits
semantics BitScan       = Sem "bitScan"  evalBitScan
semantics BitCount      = Sem "bitCount" evalBitCount
-- Feldspar.Core.Constructs.Complex
semantics MkComplex = Sem "complex"   (:+)
semantics RealPart  = Sem "creal"     realPart
semantics ImagPart  = Sem "cimag"     imagPart
semantics Conjugate = Sem "conjugate" conjugate
semantics MkPolar   = Sem "mkPolar"   mkPolar
semantics Magnitude = Sem "magnitude" magnitude
semantics Phase     = Sem "phase"     phase
semantics Cis       = Sem "cis"       cis
-- Feldspar.Core.Constructs.Condition
semantics Condition = Sem "condition" (\ c t e -> if c then t else e)
-- Feldspar.Core.Constructs.ConditionM
semantics ConditionM = Sem "if" ifM
      where
        ifM cond e t = if cond then e else t
-- Feldspar.Core.Constructs.Conversion
semantics F2I     = Sem "f2i"     truncate
semantics I2N     = Sem "i2n"     (fromInteger.toInteger)
semantics B2I     = Sem "b2i"     (\b -> if b then 1 else 0)
semantics Round   = Sem "round"   round
semantics Ceiling = Sem "ceiling" ceiling
semantics Floor   = Sem "floor"   floor
-- Feldspar.Core.Constructs.Elements
semantics EMaterialize    = Sem "materialize" ematerialize
semantics EWrite          = Sem "write" (\ix e -> Elements [(ix, e)])
semantics ESkip           = Sem "skip" (Elements [])
semantics EPar            = Sem "par" (\(Elements l) (Elements r) -> Elements (l ++ r))
semantics EparFor         = Sem "parFor" eparFor
-- Feldspar.Core.Constructs.Eq
semantics Equal    = Sem "(==)" (==)
semantics NotEqual = Sem "(/=)" (/=)
-- Feldspar.Core.Constructs.Error
semantics Undefined    = Sem "undefined" undefined
semantics (Assert msg) = Sem "assert"
        (\cond a -> if cond then a else error ("Assert failed: " ++ msg))
-- Feldspar.Core.Constructs.FFI
-- semantics (ForeignImport name f) = Sem name f
-- Feldspar.Core.Constructs.Floating
semantics Pi      = Sem "pi"      Prelude.pi
semantics Exp     = Sem "exp"     Prelude.exp
semantics Sqrt    = Sem "sqrt"    Prelude.sqrt
semantics Log     = Sem "log"     Prelude.log
semantics Pow     = Sem "(**)"    (Prelude.**)
semantics LogBase = Sem "logBase" Prelude.logBase
semantics Sin     = Sem "sin"     Prelude.sin
semantics Tan     = Sem "tan"     Prelude.tan
semantics Cos     = Sem "cos"     Prelude.cos
semantics Asin    = Sem "asin"    Prelude.asin
semantics Atan    = Sem "atan"    Prelude.atan
semantics Acos    = Sem "acos"    Prelude.acos
semantics Sinh    = Sem "sinh"    Prelude.sinh
semantics Tanh    = Sem "tanh"    Prelude.tanh
semantics Cosh    = Sem "cosh"    Prelude.cosh
semantics Asinh   = Sem "asinh"   Prelude.asinh
semantics Atanh   = Sem "atanh"   Prelude.atanh
semantics Acosh   = Sem "acosh"   Prelude.acosh
-- Feldspar.Core.Constructs.Fractional
semantics DivFrac = Sem "(/)" (/)
-- Feldspar.Core.Constructs.Future
semantics MkFuture = Sem "future" FVal
semantics Await    = Sem "await"  unFVal
-- Feldspar.Core.Constructs.Integral
semantics Quot = Sem "quot" quot
semantics Rem  = Sem "rem"  rem
semantics Div  = Sem "div"  div
semantics Mod  = Sem "mod"  mod
semantics IExp = Sem "(^)"  (^)
-- Feldspar.Core.Constructs.Literal
-- Feldspar.Core.Constructs.Logic
semantics And = Sem "(&&)" (&&)
semantics Or  = Sem "(||)" (||)
semantics Not = Sem "not"  not
-- Feldspar.Core.Constructs.Loop
semantics While = Sem "while" while
      where
        while cond body = do
                            c <- cond
                            when c (body >> while cond body)
semantics For = Sem "for" for
      where
        for 0 _    = return ()
        for l body = forM_ [0..l-1] body
semantics ForLoop = Sem "forLoop" forLoop
      where
        forLoop 0 initial _    = initial
        forLoop l initial body = foldl (flip body) initial [0..l-1]
semantics WhileLoop = Sem "whileLoop" whileLoop
      where
        whileLoop initial cond body = go initial
          where
            go st | cond st   = go $ body st
                  | otherwise = st
-- Feldspar.Core.Constructs.MutableArray
semantics NewArr    = Sem "newMArr"  $ \l -> newArray (mkBounds $ toInteger l)
semantics NewArr_   = Sem "newMArr_" $ \l -> newListArray (mkBounds $ toInteger l)
        [error $ "Undefined element at index " ++ show (i::Integer) | i <- [0..]]
semantics GetArr    = Sem "getMArr"  $ \arr i -> readArray arr (toInteger i)
semantics SetArr    = Sem "setMArr"  $ \arr i -> writeArray arr (toInteger i)
semantics ArrLength = Sem "arrLength" (getBounds >=> \(l,u) -> return $ fromInteger (u-l+1))
-- Feldspar.Core.Constructs.Mutable
semantics Run = Sem "runMutable" unsafePerformIO
semantics Bind = Sem "(>>=)" (>>=)
semantics Then = Sem "(>>)" (>>)
semantics When = Sem "when" when
semantics Return = Sem "return" return
-- Feldspar.Core.Constructs.MutableReference
semantics NewRef = Sem "newRef" newIORef
semantics GetRef = Sem "getRef" readIORef
semantics SetRef = Sem "setRef" writeIORef
semantics ModRef = Sem "modRef" (\r f -> readIORef r >>= writeIORef r . f)
-- Feldspar.Core.Constructs.MutableToPure
semantics RunMutableArray = Sem "runMutableArray" runMutableArrayEval
semantics WithArray       = Sem "withArray"       withArrayEval
-- Nested tuples
semantics Cons  = Sem "cons"  (\ x y -> x :* y)
semantics Nil   = Sem "nil"   TNil
semantics Car   = Sem "car"   (\ (x :* _) -> x)
semantics Cdr   = Sem "cdr"   (\ (_ :* y) -> y)
semantics Tup   = Sem "tup"   Tuple
-- Feldspar.Core.Constructs.NoInline
semantics NoInline  = Sem "NoInline" id
-- Feldspar.Core.Constructs.Num
semantics Abs  = Sem "abs" abs
semantics Sign = Sem "signum" signum
semantics Add  = Sem "(+)" (+)
semantics Sub  = Sem "(-)" (-)
semantics Mul  = Sem "(*)" (*)
-- Feldspar.Core.Constructs.Ord
semantics LTH = Sem "(<)"  (<)
semantics GTH = Sem "(>)"  (>)
semantics LTE = Sem "(<=)" (<=)
semantics GTE = Sem "(>=)" (>=)
semantics Min = Sem "min"  min
semantics Max = Sem "max"  max
-- Feldspar.Core.Constructs.Par
semantics ParRun    = Sem "runPar" CMP.runPar
semantics ParNew    = Sem "new" CMP.new
semantics ParGet    = Sem "get" CMP.get
semantics ParPut    = Sem "put" CMP.put_
semantics ParFork   = Sem "fork" CMP.fork
semantics ParYield  = Sem "yield" yield
-- Feldspar.Core.Constructs.RealFloat
semantics Atan2   = Sem "atan2" Prelude.atan2
-- Feldspar.Core.Constructs.Save
semantics Save = Sem "save" id
semantics Tup2 = Sem "(,)" (,)
semantics Tup3 = Sem "(,,)" (,,)
semantics Tup4 = Sem "(,,,)" (,,,)
semantics Tup5 = Sem "(,,,,)" (,,,,)
semantics Tup6 = Sem "(,,,,,)" (,,,,,)
semantics Tup7 = Sem "(,,,,,,)" (,,,,,,)
semantics Tup8 = Sem "(,,,,,,,)" (,,,,,,,)
semantics Tup9 = Sem "(,,,,,,,,)" (,,,,,,,,)
semantics Tup10 = Sem "(,,,,,,,,,)" (,,,,,,,,,)
semantics Tup11 = Sem "(,,,,,,,,,,)" (,,,,,,,,,,)
semantics Tup12 = Sem "(,,,,,,,,,,,)" (,,,,,,,,,,,)
semantics Tup13 = Sem "(,,,,,,,,,,,,)" (,,,,,,,,,,,,)
semantics Tup14 = Sem "(,,,,,,,,,,,,,)" (,,,,,,,,,,,,,)
semantics Tup15 = Sem "(,,,,,,,,,,,,,,)" (,,,,,,,,,,,,,,)
semantics Sel1 = Sem "sel1" sel1
semantics Sel2 = Sem "sel2" sel2
semantics Sel3 = Sem "sel3" sel3
semantics Sel4 = Sem "sel4" sel4
semantics Sel5 = Sem "sel5" sel5
semantics Sel6 = Sem "sel6" sel6
semantics Sel7 = Sem "sel7" sel7
semantics Sel8 = Sem "sel8" sel8
semantics Sel9 = Sem "sel9" sel9
semantics Sel10 = Sem "sel10" sel10
semantics Sel11 = Sem "sel11" sel11
semantics Sel12 = Sem "sel12" sel12
semantics Sel13 = Sem "sel13" sel13
semantics Sel14 = Sem "sel14" sel14
semantics Sel15 = Sem "sel15" sel15

-- | Support for Array
evalGetIx as i
    | 0 <= i && i < len = genericIndex as i
    | otherwise = error $ unwords
        [ "getIx: accessing index"
        , show i
        , "outside the bounds of an array of length"
        , show len
        ]
    where
      len = genericLength as

evalSetIx as i v
    | 0 <= i && i < len = genericTake i as ++ [v] ++ genericDrop (i+1) as
    | otherwise = error $ unwords
        [ "setIx: assigning index"
        , show i
        , "outside the bounds of an array of length"
        , show len
        ]
    where
      len = genericLength as

-- | Support for Bits
liftIntWord :: (a -> Int -> b) -> a -> WordN -> b
liftIntWord f x = f x . fromIntegral

liftInt :: (a -> Int -> b) -> a -> IntN -> b
liftInt f x = f x . fromIntegral

evalReverseBits :: (Num b, FiniteBits b) => b -> b
evalReverseBits b = revLoop b 0 (0 `asTypeOf` b)
  where
    bSz = finiteBitSize b
    revLoop x i n | i >= bSz    = n
                  | testBit x i = revLoop x (i+1) (setBit n (bSz - i - 1))
                  | otherwise   = revLoop x (i+1) n

evalBitScan :: (FiniteBits b) => b -> WordN
evalBitScan b =
   if isSigned b
   then scanLoop b (testBit b (finiteBitSize b - 1)) (finiteBitSize b - 2) 0
   else scanLoop b False (finiteBitSize b - 1) 0
  where
    scanLoop x t i n | i Prelude.< 0            = n
                     | testBit x i Prelude./= t = n
                     | otherwise                = scanLoop x t (i-1) (n+1)

evalBitCount :: (FiniteBits b) => b -> WordN
evalBitCount b = loop b (finiteBitSize b - 1) 0
  where
    loop x i n | i Prelude.< 0 = n
               | testBit x i   = loop x (i-1) (n+1)
               | otherwise     = loop x (i-1) n


-- | Support for Elements
ematerialize :: Length -> Elements a -> [a]
ematerialize l (Elements xs) = map snd xs'
  where xs' = genericTake l $ sortBy (compare `on` fst) xs

eparFor :: Length -> (Index -> Elements a) -> Elements a
eparFor len ixf = Elements $ concatMap (\(Elements vs) -> vs) xs
      where xs = genericTake len $ map ixf [0..]


-- | Support for MultableArray
-- | Calculate array bounds. If the length is zero, flip the arguments to
-- make an empty range
mkBounds :: Integer -> (Integer,Integer)
mkBounds l = (0, pred l)


-- | Support for MutableToPure
runMutableArrayEval :: forall a . Mut (MArr a) -> [a]
runMutableArrayEval m = unsafePerformIO $
                        do marr <- m
                           iarr <- unsafeFreeze marr
                           return (elems (iarr :: Array Integer a))

withArrayEval :: forall a b. MArr a -> ([a] -> Mut b) -> Mut b
withArrayEval ma f
    = do a <- f (elems (unsafePerformIO $ freeze ma :: Array Integer a))
         C.evaluate a
