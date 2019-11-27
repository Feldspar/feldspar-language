{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

module Feldspar.Core.Language where

import Feldspar.Core.Reify
import Feldspar.Core.Representation as R
import Feldspar.Core.Types as T
import Feldspar.Core.Constructs (Data(..), Syntax(..), SyntacticFeld(..), FeldDomain)
import Feldspar.Core.Middleend.FromTypeUtil (untypeType, literal)
import Feldspar.Core.NestedTuples

import Feldspar.Core.Collection
import Feldspar.Range
import Feldspar.Core.Collection
import Feldspar.Core.Interpretation hiding (SizeProp, sizeProp, literal)

import Control.Monad.Cont (runCont, cont)
import Data.Typeable (Typeable)
import Control.Applicative
import Control.Monad (zipWithM_)
import Data.Complex (Complex)
import Data.Int
import Data.IORef
import Data.List (genericLength)
import Data.Patch
import Data.Word
import Data.Hash (Hashable)

import qualified Data.ByteString.Char8 as B
import qualified Data.Bits as B
import Prelude.EDSL
import Prelude (Float, Double, Rational, ($), foldr)
import qualified Prelude as P

--------------------------------------------------
-- Array.hs
--------------------------------------------------

parallel :: Type a => Data Length -> (Data Index -> Data a) -> Data [a]
parallel n f = sugarSym2 R.Parallel n f


sequential :: (Type a, Syntax s) =>
              Data Length -> s -> (Data Index -> s -> (Data a,s)) -> Data [a]
sequential = sugarSym3 Sequential


append :: Type a => Data [a] -> Data [a] -> Data [a]
append = sugarSym2 Append

getLength :: Type a => Data [a] -> Data Length
getLength = sugarSym1 GetLength

-- | Change the length of the vector to the supplied value. If the supplied
-- length is greater than the old length, the new elements will have undefined
-- value.
setLength :: Type a => Data Length -> Data [a] -> Data [a]
setLength = sugarSym2 SetLength

getIx :: Type a => Data [a] -> Data Index -> Data a
getIx = sugarSym2 GetIx

setIx :: Type a => Data [a] -> Data Index -> Data a -> Data [a]
setIx = sugarSym3 SetIx

type instance Elem      (Data [a]) = Data a
type instance CollIndex (Data [a]) = Data Index
type instance CollSize  (Data [a]) = Data Length

instance Type a => Indexed (Data [a])
  where
    (!) = getIx

instance Type a => Sized (Data [a])
  where
    collSize    = getLength
    setCollSize = setLength

instance (Type a, Type b) => CollMap (Data [a]) (Data [b])
  where
    collMap f arr = parallel (getLength arr) (f . getIx arr)

-- | Array patch
(|>) :: (Sized a, CollMap a a) =>
    Patch (CollSize a) (CollSize a) -> Patch (Elem a) (Elem a) -> Patch a a
(sizePatch |> elemPatch) a =
    collMap elemPatch $ setCollSize (sizePatch (collSize a)) a

--------------------------------------------------
-- Binding.hs
--------------------------------------------------

-- | Share an expression in the scope of a function
share :: (Syntax a, Syntax b) => a -> (a -> b) -> b
share = sugarSym2 Let

-- | Share the intermediate result when composing functions
(.<) :: (Syntax b, Syntax c) => (b -> c) -> (a -> b) -> a -> c
(.<) f g a = share (g a) f

infixr 9 .<

-- | Share an expression in the scope of a function
($<) :: (Syntax a, Syntax b) => (a -> b) -> a -> b
($<) = flip share

infixr 0 $<

--------------------------------------------------
-- Bits.hs
--------------------------------------------------

infixl 5 .<<.,.>>.
infixl 4 ⊕

class (Type a, B.Bits a, Integral a, Bounded a, Size a ~ Range a) => Bits a
  where
    -- * Logical operations
    (.&.)         :: Data a -> Data a -> Data a
    (.&.)         = sugarSym2 BAnd
    (.|.)         :: Data a -> Data a -> Data a
    (.|.)         = sugarSym2 BOr
    xor           :: Data a -> Data a -> Data a
    xor           = sugarSym2 BXor
    complement    :: Data a -> Data a
    complement    = sugarSym1 Complement

    -- * Bitwise operations
    bit           :: Data Index -> Data a
    bit           = sugarSym1 Bit
    setBit        :: Data a -> Data Index -> Data a
    setBit        = sugarSym2 SetBit
    clearBit      :: Data a -> Data Index -> Data a
    clearBit      = sugarSym2 ClearBit
    complementBit :: Data a -> Data Index -> Data a
    complementBit = sugarSym2 ComplementBit
    testBit       :: Data a -> Data Index -> Data Bool
    testBit       = sugarSym2 TestBit

    -- * Movement operations
    shiftLU       :: Data a -> Data Index -> Data a
    shiftLU       = sugarSym2 ShiftLU
    shiftRU       :: Data a -> Data Index -> Data a
    shiftRU       = sugarSym2 ShiftRU
    shiftL        :: Data a -> Data IntN -> Data a
    shiftL        = sugarSym2 ShiftL
    shiftR        :: Data a -> Data IntN -> Data a
    shiftR        = sugarSym2 ShiftR
    rotateLU      :: Data a -> Data Index -> Data a
    rotateLU      = sugarSym2 RotateLU
    rotateRU      :: Data a -> Data Index -> Data a
    rotateRU      = sugarSym2 RotateRU
    rotateL       :: Data a -> Data IntN -> Data a
    rotateL       = sugarSym2 RotateL
    rotateR       :: Data a -> Data IntN -> Data a
    rotateR       = sugarSym2 RotateR
    reverseBits   :: Data a -> Data a
    reverseBits   = sugarSym1 ReverseBits

    -- * Query operations
    bitScan       :: Data a -> Data Index
    bitScan       = sugarSym1 BitScan
    bitCount      :: Data a -> Data Index
    bitCount      = sugarSym1 BitCount

    bitSize       :: Data a -> Data Index
    bitSize       = value . bitSize'
    bitSize'      :: Data a -> Index
    bitSize'      = const $ P.fromIntegral $ finiteBitSize (undefined :: a)

    isSigned      :: Data a -> Data Bool
    isSigned      = value . isSigned'
    isSigned'     :: Data a -> Bool
    isSigned'     = const $ B.isSigned (undefined :: a)

finiteBitSize :: (B.FiniteBits b) => b -> Int
finiteBitSize = B.finiteBitSize

instance Bits Word8
instance Bits Word16
instance Bits Word32
instance Bits Word64
instance Bits WordN
instance Bits Int8
instance Bits Int16
instance Bits Int32
instance Bits Int64
instance Bits IntN

-- * Combinators

(⊕)    :: (Bits a) => Data a -> Data a -> Data a
(⊕)    =  xor
(.<<.) :: (Bits a) => Data a -> Data Index -> Data a
(.<<.) =  shiftLU
(.>>.) :: (Bits a) => Data a -> Data Index -> Data a
(.>>.) =  shiftRU

-- | Set all bits to one
allOnes :: Bits a => Data a
allOnes = complement 0

-- | Set the `n` lowest bits to one
oneBits :: Bits a => Data Index -> Data a
oneBits n = complement (allOnes .<<. n)

-- | Extract the `k` lowest bits
lsbs :: Bits a => Data Index -> Data a -> Data a
lsbs k i = i .&. oneBits k

--------------------------------------------------
-- Complex.hs
--------------------------------------------------

complex :: (Numeric a, P.RealFloat a) => Data a -> Data a -> Data (Complex a)
complex = sugarSym2 MkComplex

realPart :: (Numeric a, P.RealFloat a) => Data (Complex a) -> Data a
realPart = sugarSym1 RealPart

imagPart :: (Numeric a, P.RealFloat a) => Data (Complex a) -> Data a
imagPart = sugarSym1 ImagPart

conjugate :: (Numeric a, P.RealFloat a) => Data (Complex a) -> Data (Complex a)
conjugate = sugarSym1 Conjugate

mkPolar :: (Numeric a, P.RealFloat a)
    => Data a  -- ^ Amplitude
    -> Data a  -- ^ Angle
    -> Data (Complex a)
mkPolar = sugarSym2 MkPolar

cis :: (Numeric a, P.RealFloat a) => Data a -> Data (Complex a)
cis = sugarSym1 Cis

magnitude :: (Numeric a, P.RealFloat a) => Data (Complex a) -> Data a
magnitude = sugarSym1 Magnitude

phase :: (Numeric a, P.RealFloat a) => Data (Complex a) -> Data a
phase = sugarSym1 Phase

polar :: (Numeric a, P.RealFloat a) => Data (Complex a) -> (Data a, Data a)
polar c = (magnitude c, phase c)

infixl 6 +.

(+.) :: (Numeric a, P.RealFloat a) => Data a -> Data a -> Data (Complex a)
(+.) = complex

iunit :: (Numeric a, P.RealFloat a) => Data (Complex a)
iunit = 0 +. 1

--------------------------------------------------
-- Condition.hs
--------------------------------------------------

condition :: (Syntax a) => Data Bool -> a -> a -> a
condition c t f = sugarSym3 Condition c t f

-- | Condition operator. Use as follows:
-- > cond1 ? ex1 $
-- > cond2 ? ex2 $
-- > cond3 ? ex3 $
-- >   exDefault
(?) :: (Syntax a) => Data Bool -> a -> a -> a
(?) = condition

infixl 1 ?

--------------------------------------------------
-- ConditionM.hs
--------------------------------------------------

ifM :: Syntax a => Data Bool -> M a -> M a -> M a
ifM = sugarSym3 ConditionM

whenM :: Data Bool -> M () -> M ()
whenM c ma = ifM c ma (return ())

unlessM :: Data Bool -> M () -> M ()
unlessM c = ifM c (return ())

--------------------------------------------------
-- Conversion.hs
--------------------------------------------------

i2f :: (Integral a, Numeric b, P.RealFloat b) => Data a -> Data b
i2f = i2n

f2i :: (Integral a, Numeric b, P.RealFloat b) => Data b -> Data a
f2i = sugarSym1 F2I

i2n :: (Integral a, Numeric b) => Data a -> Data b
i2n = sugarSym1 I2N

b2i :: Integral a => Data Bool -> Data a
b2i = sugarSym1 B2I

truncate :: (Integral a, Numeric b, P.RealFloat b) => Data b -> Data a
truncate = f2i

round :: (Integral a, Numeric b, P.RealFloat b) => Data b -> Data a
round = sugarSym1 Round

ceiling :: (Integral a, Numeric b, P.RealFloat b) => Data b -> Data a
ceiling = sugarSym1 Ceiling

floor :: (Integral a, Numeric b, P.RealFloat b) => Data b -> Data a
floor = sugarSym1 Floor

--------------------------------------------------
-- Elements.hs
--------------------------------------------------

materialize :: Type a => Data Length -> Data (Elements a) -> Data [a]
materialize = sugarSym2 EMaterialize

write :: Type a => Data Index -> Data a -> Data (Elements a)
write = sugarSym2 EWrite

par :: Type a => Data (Elements a) -> Data (Elements a) -> Data (Elements a)
par = sugarSym2 EPar

parFor :: Type a => Data Length -> (Data Index -> Data (Elements a)) -> Data (Elements a)
parFor = sugarSym2 EparFor

skip :: Type a => Data (Elements a)
skip = sugarSym0 ESkip

--------------------------------------------------
-- Eq.hs
--------------------------------------------------

infix 4 ==
infix 4 /=

-- | Redefinition of the standard 'P.Eq' class for Feldspar
class (Type a) => Eq a
  where
    (==) :: Data a -> Data a -> Data Bool
    (==) = sugarSym2 Equal
    (/=) :: Data a -> Data a -> Data Bool
    (/=) = sugarSym2 NotEqual

instance Eq ()
instance Eq Bool
instance Eq Float
instance Eq Double
instance Eq Word8
instance Eq Word16
instance Eq Word32
instance Eq Word64
instance Eq WordN
instance Eq Int8
instance Eq Int16
instance Eq Int32
instance Eq Int64
instance Eq IntN

instance (Eq a, Eq b)                               => Eq (a,b)
instance (Eq a, Eq b, Eq c)                         => Eq (a,b,c)
instance (Eq a, Eq b, Eq c, Eq d)                   => Eq (a,b,c,d)
instance (Eq a, Eq b, Eq c, Eq d, Eq e)             => Eq (a,b,c,d,e)
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f)       => Eq (a,b,c,d,e,f)
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g) => Eq (a,b,c,d,e,f,g)

instance (Eq a, P.RealFloat a) => Eq (Complex a)

--------------------------------------------------
-- Error.hs
--------------------------------------------------

undef :: Syntax a => a
undef = sugarSym0 Undefined

-- | Assert that the condition holds or fail with message
assertMsg :: Syntax a => String -> Data Bool -> a -> a
assertMsg s = sugarSym2 (Assert s)

-- | Assert that the condition holds, the conditions string representation is used as the message
assert :: Syntax a => Data Bool -> a -> a
assert cond = assertMsg (show cond) cond

err :: Syntax a => String -> a
err msg = assertMsg msg false undef

--------------------------------------------------
-- FFI.hs
--------------------------------------------------

{- Omitted
foreignImport :: ( Type (DenResult a)
                 , SyntacticN c b
                 , ApplySym a b FeldDomain
                 )
              => String -> Denotation a -> c
foreignImport name f = sugarSymF (ForeignImport name f)
-}

--------------------------------------------------
-- Floating.hs
--------------------------------------------------

-- Make new class, with "Data" in all the types

infixr 8 **

class (Fraction a, P.Floating a) => Floating a where
  pi        :: Data a
  pi        =  sugarSym0 Pi
  exp       :: Data a -> Data a
  exp       =  sugarSym1 Exp
  sqrt      :: Data a -> Data a
  sqrt      =  sugarSym1 Sqrt
  log       :: Data a -> Data a
  log       =  sugarSym1 Log
  (**)      :: Data a -> Data a -> Data a
  (**)      =  sugarSym2 Pow
  logBase   :: Data a -> Data a -> Data a
  logBase   =  sugarSym2 LogBase
  sin       :: Data a -> Data a
  sin       =  sugarSym1 Sin
  tan       :: Data a -> Data a
  tan       =  sugarSym1 Tan
  cos       :: Data a -> Data a
  cos       =  sugarSym1 Cos
  asin      :: Data a -> Data a
  asin      =  sugarSym1 Asin
  atan      :: Data a -> Data a
  atan      =  sugarSym1 Atan
  acos      :: Data a -> Data a
  acos      =  sugarSym1 Acos
  sinh      :: Data a -> Data a
  sinh      =  sugarSym1 Sinh
  tanh      :: Data a -> Data a
  tanh      =  sugarSym1 Tanh
  cosh      :: Data a -> Data a
  cosh      =  sugarSym1 Cosh
  asinh     :: Data a -> Data a
  asinh     =  sugarSym1 Asinh
  atanh     :: Data a -> Data a
  atanh     =  sugarSym1 Atanh
  acosh     :: Data a -> Data a
  acosh     =  sugarSym1 Acosh

instance Floating Float
instance Floating Double

instance (Fraction a, P.RealFloat a) => Floating (Complex a)

π :: Floating a => Data a
π = pi
--------------------------------------------------
-- Fractional.hs
--------------------------------------------------

-- | Fractional types. The relation to the standard 'Fractional' class is
-- @instance `Fraction` a => `Fractional` (`Data` a)@
class (Fractional a, Numeric a) => Fraction a
  where
    fromRationalFrac :: Rational -> Data a
    fromRationalFrac = value . fromRational

    divFrac :: Data a -> Data a -> Data a
    divFrac = sugarSym2 DivFrac

instance Fraction Float
instance Fraction Double

instance (Fraction a, P.RealFloat a) => Fraction (Complex a)

instance (Fraction a) => Fractional (Data a)
  where
    fromRational = fromRationalFrac
    (/)          = divFrac

--------------------------------------------------
-- Future.hs
--------------------------------------------------

newtype Future a = Future { unFuture :: Data (FVal (Internal a)) }

later :: (Syntax a, Syntax b) => (a -> b) -> Future a -> Future b
later f = future . f . await

pval :: (Syntax a, Syntax b) => (a -> b) -> a -> b
pval f x = await $ force $ future (f x)

instance Syntax a => Syntactic (Future a)
  where
    type Domain (Future a)   = FeldDomain
    type Internal (Future a) = FVal (Internal a)
    desugar = desugar . unFuture
    sugar   = Future . sugar

future :: Syntax a => a -> Future a
future = sugarSym1 MkFuture

await :: Syntax a => Future a -> a
await = sugarSym1 Await

--------------------------------------------------
-- Integral.hs
--------------------------------------------------

class (Ord a, Numeric a, BoundedInt a, P.Integral a, Size a ~ Range a) => Integral a
  where
    quot :: Data a -> Data a -> Data a
    quot = sugarSym2 Quot
    rem  :: Data a -> Data a -> Data a
    rem  = sugarSym2 Rem
    div  :: Data a -> Data a -> Data a
    div  = sugarSym2 Div
    mod  :: Data a -> Data a -> Data a
    mod  = sugarSym2 Mod
    (^)  :: Data a -> Data a -> Data a
    (^)  = sugarSym2 IExp

divSem :: Integral a
       => Data a -> Data a -> Data a
divSem x y = (x > 0 && y < 0 || x < 0 && y > 0) && rem x y /= 0 ?   quot x y P.- 1
                                                                P.$ quot x y

instance Integral Word8
instance Integral Word16
instance Integral Word32
instance Integral Word64
instance Integral WordN
instance Integral Int8
instance Integral Int16
instance Integral Int32
instance Integral Int64
instance Integral IntN

--------------------------------------------------
-- Literal.hs
--------------------------------------------------

-- value :: Syntax a => Internal a -> a
-- value = sugarSymF . Literal

false :: Data Bool
false = value False

true :: Data Bool
true = value True

instance Syntactic ()
  where
    type Domain ()   = FeldDomain
    type Internal () = ()
    desugar _ = ASTF (toCExpr $ toAExpr $ Literal ()) 0 -- Maybe more helpers?
    sugar _ = ()

--------------------------------------------------
-- Logic.hs
--------------------------------------------------

infixr 3 &&
infixr 3 &&*
infixr 2 ||
infixr 2 ||*

not :: Data Bool -> Data Bool
not = sugarSym1 Not

(&&) :: Data Bool -> Data Bool -> Data Bool
(&&) = sugarSym2 And

(||) :: Data Bool -> Data Bool -> Data Bool
(||) = sugarSym2 Or


-- | Lazy conjunction, second argument only evaluated if necessary
(&&*) :: Data Bool -> Data Bool -> Data Bool
a &&* b =  a ? b $ false

-- | Lazy disjunction, second argument only evaluated if necessary
(||*) :: Data Bool -> Data Bool -> Data Bool
a ||* b = a ? true $ b

--------------------------------------------------
-- Loop.hs
--------------------------------------------------

forLoop :: Syntax a => Data Length -> a -> (Data Index -> a -> a) -> a
forLoop = sugarSym3 ForLoop

whileLoop :: Syntax a => a -> (a -> Data Bool) -> (a -> a) -> a
whileLoop = sugarSym3 WhileLoop

--------------------------------------------------
-- LoopM.hs
--------------------------------------------------

forM :: (Syntax a) => Data Length -> (Data Index -> M a) -> M ()
forM = sugarSym2 For

whileM :: Syntax a => M (Data Bool) -> M a -> M ()
whileM = sugarSym2 While

--------------------------------------------------
-- MutableArray.hs
--------------------------------------------------

-- | Create a new 'Mutable' Array and intialize all elements
newArr :: Type a => Data Length -> Data a -> M (Data (MArr a))
newArr = sugarSym2 NewArr

-- | Create a new 'Mutable' Array but leave the elements un-initialized
newArr_ :: Type a => Data Length -> M (Data (MArr a))
newArr_ = sugarSym1 NewArr_

-- | Create a new 'Mutable' Array and initialize with elements from the
-- list
newListArr :: forall a. Type a => [Data a] -> M (Data (MArr a))
newListArr xs = do arr <- newArr_ (value $ genericLength xs)
                   zipWithM_ (setArr arr . value) [0..] xs
                   return arr

-- | Extract the element at index
getArr :: Type a => Data (MArr a) -> Data Index -> M (Data a)
getArr = sugarSym2 GetArr

-- | Replace the value at index
setArr :: Type a => Data (MArr a) -> Data Index -> Data a -> M ()
setArr = sugarSym3 SetArr

-- | Modify the element at index
modifyArr :: Type a
          => Data (MArr a) -> Data Index -> (Data a -> Data a) -> M ()
modifyArr arr i f = getArr arr i >>= setArr arr i . f

-- | Query the length of the array
arrLength :: Type a => Data (MArr a) -> M (Data Length)
arrLength = sugarSym1 ArrLength

-- | Modify all elements
mapArray :: Type a => (Data a -> Data a) -> Data (MArr a) -> M (Data (MArr a))
mapArray f arr = do
    len <- arrLength arr
    forArr len (flip (modifyArr arr) f)
    return arr

forArr :: Syntax a => Data Length -> (Data Index -> M a) -> M ()
forArr = sugarSym2 For

-- | Swap two elements
swap :: Type a
     => Data (MArr a) -> Data Index -> Data Index -> M ()
swap a i1 i2 = do
    tmp <- getArr a i1
    getArr a i2 >>= setArr a i1
    setArr a i2 tmp

--------------------------------------------------
-- Mutable.hs
--------------------------------------------------

newtype M a = M { unM :: Mon FeldDomain Mut a }
  deriving (Functor, Applicative, Monad)

instance Syntax a => Syntactic (M a)
  where
    type Domain (M a)   = FeldDomain
    type Internal (M a) = Mut (Internal a)
    desugar = desugar . unM
    sugar   = M . sugar

instance P.Eq (Mut a) where
 (==) = undefined

instance Show (Mut a) where
  show = undefined

runMutable :: Syntax a => M a -> a
runMutable = sugarSym1 Run

when :: Data Bool -> M () -> M ()
when = sugarSym2 When

unless :: Data Bool -> M () -> M ()
unless = when . not

instance Type a => Type (Mut a)
  where
    typeRep       = T.MutType typeRep
    sizeOf as     = undefined
    toTarget n as = undefined

--------------------------------------------------
-- MutableReference.hs
--------------------------------------------------

newtype Ref a = Ref { unRef :: Data (IORef (Internal a)) }

instance Syntax a => Syntactic (Ref a)
  where
    type Domain (Ref a)   = FeldDomain
    type Internal (Ref a) = IORef (Internal a)
    desugar = desugar . unRef
    sugar   = Ref . sugar

newRef :: Syntax a => a -> M (Ref a)
newRef = sugarSym1 NewRef

getRef :: Syntax a => Ref a -> M a
getRef = sugarSym1 GetRef

setRef :: Syntax a => Ref a -> a -> M ()
setRef = sugarSym2 SetRef

modifyRef :: Syntax a => Ref a -> (a -> a) -> M ()
modifyRef = sugarSym2 ModRef

--------------------------------------------------
-- MutableToPure.hs
--------------------------------------------------

withArray :: (Type a, Syntax b) => Data (MArr a) -> (Data [a] -> M b) -> M b
withArray = sugarSym2 WithArray

runMutableArray :: Type a => M (Data (MArr a)) -> Data [a]
runMutableArray = sugarSym1 RunMutableArray

freezeArray :: Type a => Data (MArr a) -> M (Data [a])
freezeArray marr = withArray marr return

thawArray :: Type a => Data [a] -> M (Data (MArr a))
thawArray arr = do
  marr <- newArr_ (getLength arr)
  forM (getLength arr) (\ix ->
    setArr marr ix (getIx arr ix)
   )
  return marr

--------------------------------------------------
-- Nested tuples
--------------------------------------------------

class SyntacticTup a where
    type DomainTup a :: * -> *
    type InternalTup a
    desugarTup :: RTuple a -> ASTF (DomainTup a) (RTuple (InternalTup a))
    sugarTup   :: ASTF (DomainTup a) (RTuple (InternalTup a)) -> RTuple a

instance (Type (Internal a), TypeF (RTuple (InternalTup b)), Syntactic a, SyntacticTup b, Typeable (InternalTup b))
         => SyntacticTup (a :* b) where
    type DomainTup (a :* b) = FeldDomain
    type InternalTup (a :* b) = Internal a :* InternalTup b
    desugarTup (x :* xs) = sugarSym2 Cons x xs
    sugarTup e = sugar (sugarSym1 Car $ Data e) :* sugarTup (sugarSym1 Cdr $ Data e)

instance SyntacticTup TNil where
    type DomainTup TNil = FeldDomain
    type InternalTup TNil = TNil
    desugarTup TNil = sugarSym0 Nil
    sugarTup _ = TNil

instance SyntacticTup a => Syntactic (RTuple a) where
    type Domain (RTuple a) = DomainTup a
    type Internal (RTuple a) = RTuple (InternalTup a)
    desugar t = desugarTup t
    sugar e = sugarTup e

car :: (Syntax a, SyntacticTup b, Type (RTuple (InternalTup b)), Typeable (InternalTup b))
    => RTuple (a :* b) -> a
car = sugarSym1 Car

cdr :: (Syntax a, SyntacticTup b, Type (RTuple (InternalTup b)), Typeable (InternalTup b))
    => RTuple (a :* b) -> RTuple b
cdr = sugarSym1 Cdr

instance (Show (RTuple (InternalTup a)),
          P.Eq (Tuple (InternalTup a)),
          TypeF (RTuple (InternalTup a)),
          Typeable (InternalTup a),
          SyntacticTup a)
      => Syntactic (Tuple a) where
    type Domain (Tuple a) = DomainTup a
    type Internal (Tuple a) = Tuple (InternalTup a)
    desugar (Tuple x) = sugarSym1 Tup x
    sugar e = Tuple $ sugar $ sugarSym1 UnTup e

--------------------------------------------------
-- NoInline.hs
--------------------------------------------------

noInline :: (Syntax a) => a -> a
noInline = sugarSym1 NoInline

--------------------------------------------------
-- Num.hs
--------------------------------------------------

-- There are three possibilities for making a `Num` instance for `Data`:
--   1. instance (Type a, Num a, Num (Size a)) => Num (Data a)
--   2. instance Num (Data Word8)
--      instance Num (Data Word16)
--      instance Num (Data Word32)
--      ...
--   3. The implementation in this module
-- #1 has the problem with #1 that it leaks implementation details.
-- #2 has the problem that it is verbose: The methods have to be implemented in each instance
-- (which, of course, can be taken care of using TemplateHaskell).
-- #3 avoids the above problems, but does so at the expense of having two numeric classes, which may
-- be confusing to the user.

class (Type a, Num a, Num (Size a), Hashable a) => Numeric a
  where
    fromIntegerNum :: Integer -> Data a
    fromIntegerNum =  value . fromInteger
    absNum         :: Data a -> Data a
    absNum         =  sugarSym1 Abs
    signumNum      :: Data a -> Data a
    signumNum      =  sugarSym1 Sign
    addNum         :: Data a -> Data a -> Data a
    addNum         =  sugarSym2 Add
    subNum         :: Data a -> Data a -> Data a
    subNum         =  sugarSym2 Sub
    mulNum         :: Data a -> Data a -> Data a
    mulNum         =  sugarSym2 Mul

instance Numeric Word8
instance Numeric Word16
instance Numeric Word32
instance Numeric Word64
instance Numeric WordN
instance Numeric Int8
instance Numeric Int16
instance Numeric Int32
instance Numeric Int64
instance Numeric IntN

instance Numeric Float
instance Numeric Double

instance (Type a, P.RealFloat a, Hashable a) => Numeric (Complex a)

instance (Numeric a) => Num (Data a)
  where
    fromInteger = fromIntegerNum
    abs         = absNum
    signum      = signumNum
    (+)         = addNum
    (-)         = subNum
    (*)         = mulNum


--------------------------------------------------
-- Ord.hs
--------------------------------------------------

infix 4 <
infix 4 >
infix 4 <=
infix 4 >=

-- | Redefinition of the standard 'Prelude.Ord' class for Feldspar
class (Eq a, P.Ord a, P.Ord (Size a)) => Ord a where
  (<)  :: Data a -> Data a -> Data Bool
  (<)  =  sugarSym2 LTH
  (>)  :: Data a -> Data a -> Data Bool
  (>)  =  sugarSym2 GTH

  (<=) :: Data a -> Data a -> Data Bool
  (<=) =  sugarSym2 LTE
  (>=) :: Data a -> Data a -> Data Bool
  (>=) =  sugarSym2 GTE

  min :: Data a -> Data a -> Data a
  min = sugarSym2 Min
  max :: Data a -> Data a -> Data a
  max = sugarSym2 Max

instance Ord ()
instance Ord Bool
instance Ord Word8
instance Ord Int8
instance Ord Word16
instance Ord Int16
instance Ord Word32
instance Ord Int32
instance Ord Word64
instance Ord Int64
instance Ord WordN
instance Ord IntN
instance Ord Float
instance Ord Double
--------------------------------------------------
-- Par.hs
--------------------------------------------------


newtype P a = P { unP :: Mon FeldDomain Par a }
  deriving (Functor, Applicative, Monad)

instance Syntax a => Syntactic (P a)
  where
    type Domain (P a)   = FeldDomain
    type Internal (P a) = Par (Internal a)
    desugar = desugar . unP
    sugar   = P . sugar

newtype IVar a = IVar { unIVar :: Data (IV (Internal a)) }

instance Syntax a => Syntactic (IVar a)
  where
    type Domain (IVar a)   = FeldDomain
    type Internal (IVar a) = IV (Internal a)
    desugar = desugar . unIVar
    sugar   = IVar . sugar

instance P.Eq (Par a) where
 (==) = undefined

instance Show (Par a) where
  show = undefined

instance Type a => Type (Par a)
  where
    typeRep       = T.ParType typeRep
    sizeOf as     = undefined
    toTarget n as = undefined

--------------------------------------------------
-- RealFloat.hs
--------------------------------------------------

-- Make new class, with "Data" in all the types

class (Type a, P.RealFloat a) => RealFloat a where
  atan2 :: Data a -> Data a -> Data a
  atan2 = sugarSym2 Atan2

instance RealFloat Float
instance RealFloat Double

--------------------------------------------------
-- Save.hs
--------------------------------------------------

-- | Tracing execution of Feldspar expressions

-- | An identity function that guarantees that the result will be computed as a
-- sub-result of the whole program. This is useful to prevent certain
-- optimizations.
-- Exception: Currently constant folding does not respect 'save'.
save :: Syntax a => a -> a
save = sugarSym1 Save

-- | Equivalent to 'save'. When applied to a lazy data structure, 'force' (and
-- 'save') has the effect of forcing evaluation of the whole structure.
force :: Syntax a => a -> a
force = save

--------------------------------------------------
-- SizeProp.hs
--------------------------------------------------

-- | The functions in this module can be used to help size inference (which, in
-- turn, helps deriving upper bounds of array sizes and helps optimization).

-- | An identity function affecting the abstract size information used during
-- optimization. The application of a 'SizeCap' is a /guarantee/ (by the caller)
-- that the argument is within a certain size (determined by the creator of the
-- 'SizeCap', e.g. 'sizeProp').
-- /Warning: If the guarantee is not fulfilled, optimizations become unsound!/
-- In general, the size of the resulting value is the intersection of the cap
-- size and the size obtained by ordinary size inference. That is, a 'SizeCap'
-- can only make the size more precise, not less precise.
type SizeCap a = Data a -> Data a

-- | @sizeProp prop a b@: A guarantee that @b@ is within the size @(prop sa)@,
-- where @sa@ is the size of @a@.
sizeProp :: (Syntax a, Type b) =>
    (Size (Internal a) -> Size b) -> a -> SizeCap b
sizeProp f = sugarSym2 (PropSize $ EqBox f) -- TODO

-- | A guarantee that the argument is within the given size
cap :: Type a => Size a -> SizeCap a
cap sz = sizeProp (const sz) (Data $ desugar ())

-- | @notAbove a b@: A guarantee that @b <= a@ holds
notAbove :: (Type a, Bounded a, Size a ~ Range a) => Data a -> SizeCap a
notAbove = sizeProp (Range minBound . upperBound)

-- | @notBelow a b@: A guarantee that @b >= a@ holds
notBelow :: (Type a, Bounded a, Size a ~ Range a) => Data a -> SizeCap a
notBelow = sizeProp (flip Range maxBound . lowerBound)

-- | @between l u a@: A guarantee that @l <= a <= u@ holds
between :: (Type a, Bounded a, Size a ~ Range a) =>
    Data a -> Data a -> SizeCap a
between l u = notBelow l . notAbove u

--------------------------------------------------
-- SourceInfo.hs
--------------------------------------------------
{- Omitted
-- | Source-code annotations

data SourceInfo1 a = SourceInfo1 SourceInfo

-- | Annotate an expression with information about its source code
sourceData :: Type a => SourceInfo1 a -> Data a -> Data a
sourceData info = sugarSym1 (Decor info Id)
-}

--------------------------------------------------
-- Switch.hs
--------------------------------------------------

-- | Select between the cases based on the value of the scrutinee.
select :: (Eq a, Syntax b) => Data a -> [(Data a, b)] -> b -> b
select s cs def = foldr (\(c,a) b -> c == s ? a $ b) def cs

{-# DEPRECATED select "select will generate a tree of if-statements. Use switch instead" #-}

-- | Select between the cases based on the value of the scrutinee.
-- If no match is found return the first argument
switch :: (Eq (Internal a), Hashable (Internal a), Syntax a, Syntax b)
       => b -> [(Internal a, b)] -> a -> b
switch def [] _ = def
switch def cs s = let s' = resugar s
                  in sugarSym1 Switch (foldr (\(c,a) b -> value c == s' ? a $ b) def cs)

--------------------------------------------------
-- Tuple.hs
--------------------------------------------------

-- instance TupleSat FeldDomain Type

instance (Syntax a, Syntax b) => Syntactic (a, b) where
  type Internal (a, b) = (Internal a, Internal b)
  type Domain (a, b) = Domain a -- FeldDomain
  sugar e = (sugar $ sugarSym1 Sel1 $ Data e,
             sugar $ sugarSym1 Sel2 $ Data e)
  desugar (x,y) = full $ op2f Tup2 @@ x @@ y

instance ( Syntax a, Syntax b, Syntax c )
      => Syntactic (a, b, c)
  where
    type Internal (a, b, c) =
                  ( Internal a, Internal b, Internal c )
    type Domain (a, b, c) = Domain a -- FeldDomain
    sugar e = ( sugar $ sugarSym1 Sel1  $ Data e
              , sugar $ sugarSym1 Sel2  $ Data e
              , sugar $ sugarSym1 Sel3  $ Data e
              )
    desugar (a, b, c)
          = full $ op2f Tup3 @@ a @@ b @@ c

instance ( Syntax a, Syntax b, Syntax c, Syntax d )
      => Syntactic (a, b, c, d)
  where
    type Internal (a, b, c, d) =
                  ( Internal a, Internal b, Internal c, Internal d )
    type Domain (a, b, c, d) = Domain a -- FeldDomain
    sugar e = ( sugar $ sugarSym1 Sel1  $ Data e
              , sugar $ sugarSym1 Sel2  $ Data e
              , sugar $ sugarSym1 Sel3  $ Data e
              , sugar $ sugarSym1 Sel4  $ Data e
              )
    desugar (a, b, c, d)
          = full $ op2f Tup4 @@ a @@ b @@ c @@ d

instance ( Syntax a, Syntax b, Syntax c, Syntax d
         , Syntax e
         )
      => Syntactic (a, b, c, d, e)
  where
    type Internal (a, b, c, d, e) =
                  ( Internal a, Internal b, Internal c, Internal d
                  , Internal e
                  )
    type Domain (a, b, c, d, e) = Domain a -- FeldDomain
    sugar e = ( sugar $ sugarSym1 Sel1  $ Data e
              , sugar $ sugarSym1 Sel2  $ Data e
              , sugar $ sugarSym1 Sel3  $ Data e
              , sugar $ sugarSym1 Sel4  $ Data e
              , sugar $ sugarSym1 Sel5  $ Data e
              )
    desugar (a, b, c, d, e)
          = full $ op2f Tup5 @@ a @@ b @@ c @@ d @@ e

instance ( Syntax a, Syntax b, Syntax c, Syntax d
         , Syntax e, Syntax f
         )
      => Syntactic (a, b, c, d, e, f)
  where
    type Internal (a, b, c, d, e, f) =
                  ( Internal a, Internal b, Internal c, Internal d
                  , Internal e, Internal f
                  )
    type Domain (a, b, c, d, e, f) = Domain a -- FeldDomain
    sugar e = ( sugar $ sugarSym1 Sel1  $ Data e
              , sugar $ sugarSym1 Sel2  $ Data e
              , sugar $ sugarSym1 Sel3  $ Data e
              , sugar $ sugarSym1 Sel4  $ Data e
              , sugar $ sugarSym1 Sel5  $ Data e
              , sugar $ sugarSym1 Sel6  $ Data e
              )
    desugar (a, b, c, d, e, f)
          = full $ op2f Tup6 @@ a @@ b @@ c @@ d @@ e @@ f

instance ( Syntax a, Syntax b, Syntax c, Syntax d
         , Syntax e, Syntax f, Syntax g
         )
      => Syntactic (a, b, c, d, e, f, g)
  where
    type Internal (a, b, c, d, e, f, g) =
                  ( Internal a, Internal b, Internal c, Internal d
                  , Internal e, Internal f, Internal g
                  )
    type Domain (a, b, c, d, e, f, g) = Domain a -- FeldDomain
    sugar e = ( sugar $ sugarSym1 Sel1  $ Data e
              , sugar $ sugarSym1 Sel2  $ Data e
              , sugar $ sugarSym1 Sel3  $ Data e
              , sugar $ sugarSym1 Sel4  $ Data e
              , sugar $ sugarSym1 Sel5  $ Data e
              , sugar $ sugarSym1 Sel6  $ Data e
              , sugar $ sugarSym1 Sel7  $ Data e
              )
    desugar (a, b, c, d, e, f, g)
          = full $ op2f Tup7 @@ a @@ b @@ c @@ d @@ e @@ f @@ g

instance ( Syntax a, Syntax b, Syntax c, Syntax d
         , Syntax e, Syntax f, Syntax g, Syntax h
         )
      => Syntactic (a, b, c, d, e, f, g, h)
  where
    type Internal (a, b, c, d, e, f, g, h) =
                  ( Internal a, Internal b, Internal c, Internal d
                  , Internal e, Internal f, Internal g, Internal h
                  )
    type Domain (a, b, c, d, e, f, g, h) = Domain a -- FeldDomain
    sugar e = ( sugar $ sugarSym1 Sel1  $ Data e
              , sugar $ sugarSym1 Sel2  $ Data e
              , sugar $ sugarSym1 Sel3  $ Data e
              , sugar $ sugarSym1 Sel4  $ Data e
              , sugar $ sugarSym1 Sel5  $ Data e
              , sugar $ sugarSym1 Sel6  $ Data e
              , sugar $ sugarSym1 Sel7  $ Data e
              , sugar $ sugarSym1 Sel8  $ Data e
              )
    desugar (a, b, c, d, e, f, g, h)
          = full $ op2f Tup8 @@ a @@ b @@ c @@ d @@ e @@ f @@ g @@ h

instance ( Syntax a, Syntax b, Syntax c, Syntax d
         , Syntax e, Syntax f, Syntax g, Syntax h
         , Syntax i
         )
      => Syntactic (a, b, c, d, e, f, g, h, i)
  where
    type Internal (a, b, c, d, e, f, g, h, i) =
                  ( Internal a, Internal b, Internal c, Internal d
                  , Internal e, Internal f, Internal g, Internal h
                  , Internal i
                  )
    type Domain (a, b, c, d, e, f, g, h, i) = Domain a -- FeldDomain
    sugar e = ( sugar $ sugarSym1 Sel1  $ Data e
              , sugar $ sugarSym1 Sel2  $ Data e
              , sugar $ sugarSym1 Sel3  $ Data e
              , sugar $ sugarSym1 Sel4  $ Data e
              , sugar $ sugarSym1 Sel5  $ Data e
              , sugar $ sugarSym1 Sel6  $ Data e
              , sugar $ sugarSym1 Sel7  $ Data e
              , sugar $ sugarSym1 Sel8  $ Data e
              , sugar $ sugarSym1 Sel9  $ Data e
              )
    desugar (a, b, c, d, e, f, g, h, i)
          = full $ op2f Tup9 @@ a @@ b @@ c @@ d @@ e @@ f @@ g @@ h @@ i

instance ( Syntax a, Syntax b, Syntax c, Syntax d
         , Syntax e, Syntax f, Syntax g, Syntax h
         , Syntax i, Syntax j
         )
      => Syntactic (a, b, c, d, e, f, g, h, i, j)
  where
    type Internal (a, b, c, d, e, f, g, h, i, j) =
                  ( Internal a, Internal b, Internal c, Internal d
                  , Internal e, Internal f, Internal g, Internal h
                  , Internal i, Internal j
                  )
    type Domain (a, b, c, d, e, f, g, h, i, j) = Domain a -- FeldDomain
    sugar e = ( sugar $ sugarSym1 Sel1  $ Data e
              , sugar $ sugarSym1 Sel2  $ Data e
              , sugar $ sugarSym1 Sel3  $ Data e
              , sugar $ sugarSym1 Sel4  $ Data e
              , sugar $ sugarSym1 Sel5  $ Data e
              , sugar $ sugarSym1 Sel6  $ Data e
              , sugar $ sugarSym1 Sel7  $ Data e
              , sugar $ sugarSym1 Sel8  $ Data e
              , sugar $ sugarSym1 Sel9  $ Data e
              , sugar $ sugarSym1 Sel10 $ Data e
              )
    desugar (a, b, c, d, e, f, g, h, i, j)
          = full $ op2f Tup10 @@ a @@ b @@ c @@ d @@ e @@ f @@ g @@ h @@ i @@ j

instance ( Syntax a, Syntax b, Syntax c, Syntax d
         , Syntax e, Syntax f, Syntax g, Syntax h
         , Syntax i, Syntax j, Syntax k
         )
      => Syntactic (a, b, c, d, e, f, g, h, i, j, k)
  where
    type Internal (a, b, c, d, e, f, g, h, i, j, k) =
                  ( Internal a, Internal b, Internal c, Internal d
                  , Internal e, Internal f, Internal g, Internal h
                  , Internal i, Internal j, Internal k
                  )
    type Domain (a, b, c, d, e, f, g, h, i, j, k) = Domain a -- FeldDomain
    sugar e = ( sugar $ sugarSym1 Sel1  $ Data e
              , sugar $ sugarSym1 Sel2  $ Data e
              , sugar $ sugarSym1 Sel3  $ Data e
              , sugar $ sugarSym1 Sel4  $ Data e
              , sugar $ sugarSym1 Sel5  $ Data e
              , sugar $ sugarSym1 Sel6  $ Data e
              , sugar $ sugarSym1 Sel7  $ Data e
              , sugar $ sugarSym1 Sel8  $ Data e
              , sugar $ sugarSym1 Sel9  $ Data e
              , sugar $ sugarSym1 Sel10 $ Data e
              , sugar $ sugarSym1 Sel11 $ Data e
              )
    desugar (a, b, c, d, e, f, g, h, i, j, k)
          = full $ op2f Tup11 @@ a @@ b @@ c @@ d @@ e @@ f @@ g @@ h @@ i @@ j
                              @@ k

instance ( Syntax a, Syntax b, Syntax c, Syntax d
         , Syntax e, Syntax f, Syntax g, Syntax h
         , Syntax i, Syntax j, Syntax k, Syntax l
         )
      => Syntactic (a, b, c, d, e, f, g, h, i, j, k, l)
  where
    type Internal (a, b, c, d, e, f, g, h, i, j, k, l) =
                  ( Internal a, Internal b, Internal c, Internal d
                  , Internal e, Internal f, Internal g, Internal h
                  , Internal i, Internal j, Internal k, Internal l
                  )
    type Domain (a, b, c, d, e, f, g, h, i, j, k, l) = Domain a -- FeldDomain
    sugar e = ( sugar $ sugarSym1 Sel1  $ Data e
              , sugar $ sugarSym1 Sel2  $ Data e
              , sugar $ sugarSym1 Sel3  $ Data e
              , sugar $ sugarSym1 Sel4  $ Data e
              , sugar $ sugarSym1 Sel5  $ Data e
              , sugar $ sugarSym1 Sel6  $ Data e
              , sugar $ sugarSym1 Sel7  $ Data e
              , sugar $ sugarSym1 Sel8  $ Data e
              , sugar $ sugarSym1 Sel9  $ Data e
              , sugar $ sugarSym1 Sel10 $ Data e
              , sugar $ sugarSym1 Sel11 $ Data e
              , sugar $ sugarSym1 Sel12 $ Data e
              )
    desugar (a, b, c, d, e, f, g, h, i, j, k, l)
          = full $ op2f Tup12 @@ a @@ b @@ c @@ d @@ e @@ f @@ g @@ h @@ i @@ j
                              @@ k @@ l

instance ( Syntax a, Syntax b, Syntax c, Syntax d
         , Syntax e, Syntax f, Syntax g, Syntax h
         , Syntax i, Syntax j, Syntax k, Syntax l
         , Syntax m
         )
      => Syntactic (a, b, c, d, e, f, g, h, i, j, k, l, m)
  where
    type Internal (a, b, c, d, e, f, g, h, i, j, k, l, m) =
                  ( Internal a, Internal b, Internal c, Internal d
                  , Internal e, Internal f, Internal g, Internal h
                  , Internal i, Internal j, Internal k, Internal l
                  , Internal m
                  )
    type Domain (a, b, c, d, e, f, g, h, i, j, k, l, m) = Domain a -- FeldDomain
    sugar e = ( sugar $ sugarSym1 Sel1  $ Data e
              , sugar $ sugarSym1 Sel2  $ Data e
              , sugar $ sugarSym1 Sel3  $ Data e
              , sugar $ sugarSym1 Sel4  $ Data e
              , sugar $ sugarSym1 Sel5  $ Data e
              , sugar $ sugarSym1 Sel6  $ Data e
              , sugar $ sugarSym1 Sel7  $ Data e
              , sugar $ sugarSym1 Sel8  $ Data e
              , sugar $ sugarSym1 Sel9  $ Data e
              , sugar $ sugarSym1 Sel10 $ Data e
              , sugar $ sugarSym1 Sel11 $ Data e
              , sugar $ sugarSym1 Sel12 $ Data e
              , sugar $ sugarSym1 Sel13 $ Data e
              )
    desugar (a, b, c, d, e, f, g, h, i, j, k, l, m)
          = full $ op2f Tup13 @@ a @@ b @@ c @@ d @@ e @@ f @@ g @@ h @@ i @@ j
                              @@ k @@ l @@ m

instance ( Syntax a, Syntax b, Syntax c, Syntax d
         , Syntax e, Syntax f, Syntax g, Syntax h
         , Syntax i, Syntax j, Syntax k, Syntax l
         , Syntax m, Syntax n
         )
      => Syntactic (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
  where
    type Internal (a, b, c, d, e, f, g, h, i, j, k, l, m, n) =
                  ( Internal a, Internal b, Internal c, Internal d
                  , Internal e, Internal f, Internal g, Internal h
                  , Internal i, Internal j, Internal k, Internal l
                  , Internal m, Internal n
                  )
    type Domain (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = Domain a -- FeldDomain
    sugar e = ( sugar $ sugarSym1 Sel1  $ Data e
              , sugar $ sugarSym1 Sel2  $ Data e
              , sugar $ sugarSym1 Sel3  $ Data e
              , sugar $ sugarSym1 Sel4  $ Data e
              , sugar $ sugarSym1 Sel5  $ Data e
              , sugar $ sugarSym1 Sel6  $ Data e
              , sugar $ sugarSym1 Sel7  $ Data e
              , sugar $ sugarSym1 Sel8  $ Data e
              , sugar $ sugarSym1 Sel9  $ Data e
              , sugar $ sugarSym1 Sel10 $ Data e
              , sugar $ sugarSym1 Sel11 $ Data e
              , sugar $ sugarSym1 Sel12 $ Data e
              , sugar $ sugarSym1 Sel13 $ Data e
              , sugar $ sugarSym1 Sel14 $ Data e
              )
    desugar (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
          = full $ op2f Tup14 @@ a @@ b @@ c @@ d @@ e @@ f @@ g @@ h @@ i @@ j
                              @@ k @@ l @@ m @@ n

instance ( Syntax a, Syntax b, Syntax c, Syntax d
         , Syntax e, Syntax f, Syntax g, Syntax h
         , Syntax i, Syntax j, Syntax k, Syntax l
         , Syntax m, Syntax n, Syntax o
         )
      => Syntactic (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
  where
    type Internal (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) =
                  ( Internal a, Internal b, Internal c, Internal d
                  , Internal e, Internal f, Internal g, Internal h
                  , Internal i, Internal j, Internal k, Internal l
                  , Internal m, Internal n, Internal o
                  )
    type Domain (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = Domain a -- FeldDomain
    sugar e = ( sugar $ sugarSym1 Sel1  $ Data e
              , sugar $ sugarSym1 Sel2  $ Data e
              , sugar $ sugarSym1 Sel3  $ Data e
              , sugar $ sugarSym1 Sel4  $ Data e
              , sugar $ sugarSym1 Sel5  $ Data e
              , sugar $ sugarSym1 Sel6  $ Data e
              , sugar $ sugarSym1 Sel7  $ Data e
              , sugar $ sugarSym1 Sel8  $ Data e
              , sugar $ sugarSym1 Sel9  $ Data e
              , sugar $ sugarSym1 Sel10 $ Data e
              , sugar $ sugarSym1 Sel11 $ Data e
              , sugar $ sugarSym1 Sel12 $ Data e
              , sugar $ sugarSym1 Sel13 $ Data e
              , sugar $ sugarSym1 Sel14 $ Data e
              , sugar $ sugarSym1 Sel15 $ Data e
              )
    desugar (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
          = full $ op2f Tup15 @@ a @@ b @@ c @@ d @@ e @@ f @@ g @@ h @@ i @@ j
                              @@ k @@ l @@ m @@ n @@ o

-------------------------------------------------
-- Functions
-------------------------------------------------

instance (Type (Internal a), TypeF (Internal b), Syntactic a, Syntactic b) => Syntactic (a -> b) where
  type Domain (a -> b) = FeldDomain
  type Internal (a -> b) = Internal a -> Internal b
  sugar e = undefined
  desugar f = ASTF func (i+1)
    where ASTF ce i = desugar $ f (sugar $ ASTF (toCExpr $ toAExpr $ Variable v) 0)
          (m1,e1) = catchBindings [varNum v] ce
          func = (m1, toAExpr $ Lambda v e1)
          v = Var (P.fromIntegral i + hashBase) B.empty

-------------------------------------------------
-- Converting Haskell values to Feldspar
-------------------------------------------------

value :: (Syntax a, Hashable (Internal a)) => Internal a -> a
value v = sugar $ ASTF (flattenCSE $ toCExpr $ toAExpr $ Literal v) 0

-------------------------------------------------
-- Support functions for monads
-------------------------------------------------

-- | One-layer desugaring of monadic actions
desugarMonad
    :: ( Monad m
       , Typeable m
       , Typeable a
       , Type (m a)
       , Type a
       , Size a ~ Size (m a)
       )
    => Mon dom m (ASTF dom a) -> ASTF dom (m a)
desugarMonad = flip runCont (sugarSym1 Return) . unMon

-- | One-layer sugaring of monadic actions
sugarMonad
    :: ( Monad m
       , Typeable m
       , Typeable a
       -- , pVar a
       , Type (m a)
       , Type a
       , Size a ~ Size (m a)
       )
    => ASTF dom (m a) -> Mon dom m (ASTF dom a)
sugarMonad ma = Mon $ cont $ sugarSym2 Bind ma

instance ( Syntactic a
         , Monad m
         , Typeable m
         , Typeable (Internal a)
         -- , pVar (Internal a)
         -- , Domain a ~ FeldDomain
         , dom ~ Domain a
         , Type (Internal a)
         , Type (m (Internal a))
         , Size (Internal a) ~ Size (m (Internal a))
         ) =>
           Syntactic (Mon dom m a)
  where
    type Domain (Mon dom m a)   = dom
    type Internal (Mon dom m a) = m (Internal a)
    desugar = desugarMonad . fmap desugar
    sugar   = fmap sugar   . sugarMonad


-------------------------------------------------
-- Support functions
-------------------------------------------------

-- | Cast the domain (phantom) type parameter
ut :: ASTF d a -> ASTF e a
ut (ASTF e i) = ASTF e i

-- | Convert a CSE map, an Expr and an Int to an ASTF
full :: TypeF b => (CSEExpr (Expr b), Int) -> ASTF dom b
full (ce, i) = ASTF (flattenCSE $ transCSEExpr toAExpr ce) i

infixl 5 @@

-- Construct an application
(@@) :: (Syntactic a, TypeF (Internal a), TypeF b)
     => (CSEExpr (Expr (Internal a -> b)), Int) -> a -> (CSEExpr (Expr b), Int)
(cf,i) @@ e = go $ desugar e
  where go (ASTF ce j) = (applyCSE cf ce, P.max i j)

-- | Convenience wrappers for sugarSym
sugarSym0 op         = unFull $ sugarSym op
sugarSym1 op a       = unFull $ sugarSym op a
sugarSym2 op a b     = unFull $ sugarSym op a b
sugarSym3 op a b c   = unFull $ sugarSym op a b c
sugarSym4 op a b c d = unFull $ sugarSym op a b c d

-- | Convert an 'Op' to a function that builds the corresponding syntax tree
sugarSym :: (SugarF a) => Op (SugarT a) -> a
sugarSym = sugarF . op2f

-- | Mark an application as full rather than partial
newtype FFF a = FFF a

-- | Force the argument to be a full applicaton, resolving the
--   overloading in sugarSym
unFull :: FFF a -> a
unFull (FFF x) = x

op2f :: Op a -> (RCSExpr a, Int)
op2f op = (toCExpr $ Operator op, 0)

type RCSExpr a = CSEExpr (Expr a)

-- | Support for the overloaded sugarSym function
class SugarF a where
  type SugarT a
  sugarF :: (RCSExpr (SugarT a), Int) -> a

instance (Syntactic b, TypeF (Internal b), TypeF (SugarT c), SugarF c) => SugarF (b -> c) where
  type SugarT (b -> c) = Internal b -> SugarT c
  sugarF f = \ e -> sugarF $ f @@ e

instance (Syntactic b, TypeF (Internal b)) => SugarF (FFF b) where
  type SugarT (FFF b) = Internal b
  sugarF (ce,i) = FFF $ sugar $ ASTF (flattenCSE $ transCSEExpr toAExpr ce) i

