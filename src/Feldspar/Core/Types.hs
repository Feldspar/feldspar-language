{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

module Feldspar.Core.Types
       ( module Feldspar.Core.Types
       , tuple, Tuple(), RTuple(..), sel, Skip(..), First(..), (:*), TNil -- From NestedTuples
       ) where



import Data.Array.IO
import Data.Bits
import Data.Complex
import Data.Int
import Data.IORef
import Data.List
import Data.Typeable (Typeable)
import Data.Orphans
import Data.Word
import Data.Default
import Data.Hash
import Test.QuickCheck
import System.Random (Random(..))
import qualified Control.Monad.Par as MonadPar

import Data.Patch
import Data.Proxy

import Control.DeepSeq (NFData(..))
import Foreign.Storable (Storable)

import Feldspar.Lattice
import Feldspar.Range
import Feldspar.Core.NestedTuples


--------------------------------------------------------------------------------
-- * Heterogenous lists
--------------------------------------------------------------------------------

-- | Heterogeneous list
data a :> b = a :> b
  deriving (Eq, Ord, Show)

infixr 5 :>

instance (Lattice a, Lattice b) => Lattice (a :> b)
  where
    bot = bot :> bot
    top = top :> top
    (a1:>a2) \/ (b1:>b2) = (a1 \/ b1) :> (a2 \/ b2)
    (a1:>a2) /\ (b1:>b2) = (a1 /\ b1) :> (a2 /\ b2)



--------------------------------------------------------------------------------
-- * Integers
--------------------------------------------------------------------------------

-- | Target-dependent unsigned integers
newtype WordN = WordN Word32
  deriving
    ( Eq, Ord, Num, Enum, Ix, Real, Integral, Bits, Bounded, Typeable
    , Arbitrary, Random, Storable, NFData, Default
    , FiniteBits, Hashable
    )

type instance UnsignedRep WordN = Word32

-- | Target-dependent signed integers
newtype IntN = IntN Int32
  deriving
    ( Eq, Ord, Num, Enum, Ix, Real, Integral, Bits, Bounded, Typeable
    , Arbitrary, Random, Storable, NFData, Default
    , FiniteBits, Hashable
    )

type instance UnsignedRep IntN = Word32

instance Show WordN
  where
    show (WordN a) = show a

instance Show IntN
  where
    show (IntN a) = show a

-- | Type representation of 8 bits
data N8

-- | Type representation of 16 bits
data N16

-- | Type representation of 32 bits
data N32

-- | Type representation of 64 bits
data N64

-- | Type representation of the native number of bits on the target
data NNative

-- | Witness for 'N8', 'N16', 'N32', 'N64' or 'NNative'
data BitWidth n
  where
    N8      :: BitWidth N8
    N16     :: BitWidth N16
    N32     :: BitWidth N32
    N64     :: BitWidth N64
    NNative :: BitWidth NNative

bitWidth :: BitWidth n -> String
bitWidth N8      = "8"
bitWidth N16     = "16"
bitWidth N32     = "32"
bitWidth N64     = "64"
bitWidth NNative = "N"

-- | Type representation of \"unsigned\"
data U

-- | Type representation of \"signed\"
data S

-- | Witness for 'U' or 'S'
data Signedness s
  where
    U :: Signedness U
    S :: Signedness S

signedness :: Signedness s -> String
signedness U = "Word"
signedness S = "Int"

-- | A generalization of unsigned and signed integers. The first parameter
-- represents the signedness and the sectond parameter the number of bits.
type family GenericInt s n
type instance GenericInt U N8      = Word8
type instance GenericInt S N8      = Int8
type instance GenericInt U N16     = Word16
type instance GenericInt S N16     = Int16
type instance GenericInt U N32     = Word32
type instance GenericInt S N32     = Int32
type instance GenericInt U N64     = Word64
type instance GenericInt S N64     = Int64
type instance GenericInt U NNative = WordN
type instance GenericInt S NNative = IntN

type family WidthOf a
type instance WidthOf Word8  = N8
type instance WidthOf Int8   = N8
type instance WidthOf Word16 = N16
type instance WidthOf Int16  = N16
type instance WidthOf Word32 = N32
type instance WidthOf Int32  = N32
type instance WidthOf Word64 = N64
type instance WidthOf Int64  = N64
type instance WidthOf WordN  = NNative
type instance WidthOf IntN   = NNative

type family SignOf a
type instance SignOf Word8  = U
type instance SignOf Int8   = S
type instance SignOf Word16 = U
type instance SignOf Int16  = S
type instance SignOf Word32 = U
type instance SignOf Int32  = S
type instance SignOf Word64 = U
type instance SignOf Int64  = S
type instance SignOf WordN  = U
type instance SignOf IntN   = S

fromWordN :: BitWidth n -> WordN -> GenericInt U n
fromWordN N8      = fromInteger . toInteger
fromWordN N16     = fromInteger . toInteger
fromWordN N32     = fromInteger . toInteger
fromWordN N64     = fromInteger . toInteger
fromWordN NNative = id
  -- TODO Check that the number fits

fromIntN :: BitWidth n -> IntN -> GenericInt S n
fromIntN N8      = fromInteger . toInteger
fromIntN N16     = fromInteger . toInteger
fromIntN N32     = fromInteger . toInteger
fromIntN N64     = fromInteger . toInteger
fromIntN NNative = id
  -- TODO Check that the number fits

genericLen :: BitWidth n -> [a] -> GenericInt U n
genericLen N8      = genericLength
genericLen N16     = genericLength
genericLen N32     = genericLength
genericLen N64     = genericLength
genericLen NNative = genericLength

type Length = WordN
type Index  = WordN



--------------------------------------------------------------------------------
-- * Arrays
--------------------------------------------------------------------------------

-- | Array whose length is represented by an @n@-bit word
data TargetArr n a = TargetArr (GenericInt U n) [a]


--------------------------------------------------------------------------------
-- * Monadic Types
--------------------------------------------------------------------------------

-- | This class is used to allow constructs to be abstract in the monad
class MonadType m
  where
    voidTypeRep :: TypeRep (m ())
  -- TODO Since the `Mut` monad is already abstract, this class is probably only
  --      needed if we want to be able to use monadic constructs with different
  --      monad types.



--------------------------------------------------------------------------------
-- * Mutable data
--------------------------------------------------------------------------------

-- TODO Make newtypes?

-- | Monad for manipulation of mutable data
type Mut = IO

-- | Mutable references
instance Show (IORef a)
  where
    show _ = "IORef"

-- | Mutable arrays
type MArr a = IOArray Integer a

instance Show (MArr a)
  where
    show _ = "MArr"

instance MonadType Mut
  where
    voidTypeRep = MutType UnitType


--------------------------------------------------------------------------------
-- * Par Monad
--------------------------------------------------------------------------------

-- | Monad for parallel constructs
type Par = MonadPar.Par

deriving instance Typeable Par

-- | Immutable references
type IV = MonadPar.IVar

instance Show (IV a)
  where
    show _ = "IVar"

instance MonadType Par
  where
    voidTypeRep = ParType UnitType

--------------------------------------------------------------------------------
-- * Elements Language
--------------------------------------------------------------------------------

newtype Elements a = Elements { unE :: [(Index, a)] }

deriving instance Typeable Elements

instance Show a => Show (Elements a)
  where
    show (Elements a) = "Elements " ++ show a

instance Eq a => Eq (Elements a)
  where
    _ == _ = False

--------------------------------------------------------------------------------
-- * Future values
--------------------------------------------------------------------------------

newtype FVal a = FVal {unFVal :: a}

deriving instance Typeable IV
deriving instance Typeable FVal

instance Show (FVal a)
  where
    show _ = "future"

instance Eq a => Eq (FVal a)
  where
    (FVal a) == (FVal b) = a == b

--------------------------------------------------------------------------------
-- * Hashing
--------------------------------------------------------------------------------

instance Hashable a => Hashable (Complex a) where
  hash (re :+ im) = hash re `combine` hash im

--------------------------------------------------------------------------------
-- * Type representation
--------------------------------------------------------------------------------

-- | Representation of supported types
data TypeRep a
  where
    UnitType      :: TypeRep ()
    BoolType      :: TypeRep Bool
    IntType       :: ( BoundedInt (GenericInt s n)
                     , Size (GenericInt s n) ~ Range (GenericInt s n)
                     ) =>
                       Signedness s -> BitWidth n -> TypeRep (GenericInt s n)
    FloatType     :: TypeRep Float
    DoubleType    :: TypeRep Double
    ComplexType   :: RealFloat a => TypeRep a -> TypeRep (Complex a)
    ArrayType     :: TypeRep a -> TypeRep [a]
    TargetArrType :: BitWidth n -> TypeRep a -> TypeRep (TargetArr n a)
    Tup2Type      :: TypeRep a -> TypeRep b -> TypeRep (a,b)
    Tup3Type      :: TypeRep a -> TypeRep b -> TypeRep c -> TypeRep (a,b,c)
    Tup4Type      :: TypeRep a -> TypeRep b -> TypeRep c -> TypeRep d -> TypeRep (a,b,c,d)
    Tup5Type      :: TypeRep a -> TypeRep b -> TypeRep c -> TypeRep d -> TypeRep e -> TypeRep (a,b,c,d,e)
    Tup6Type      :: TypeRep a -> TypeRep b -> TypeRep c -> TypeRep d -> TypeRep e -> TypeRep f -> TypeRep (a,b,c,d,e,f)
    Tup7Type      :: TypeRep a -> TypeRep b -> TypeRep c -> TypeRep d -> TypeRep e -> TypeRep f -> TypeRep g -> TypeRep (a,b,c,d,e,f,g)
    Tup8Type      :: TypeRep a -> TypeRep b -> TypeRep c -> TypeRep d -> TypeRep e -> TypeRep f -> TypeRep g -> TypeRep h -> TypeRep (a,b,c,d,e,f,g,h)
    Tup9Type      :: TypeRep a -> TypeRep b -> TypeRep c -> TypeRep d -> TypeRep e -> TypeRep f -> TypeRep g -> TypeRep h -> TypeRep i -> TypeRep (a,b,c,d,e,f,g,h,i)
    Tup10Type      :: TypeRep a -> TypeRep b -> TypeRep c -> TypeRep d -> TypeRep e -> TypeRep f -> TypeRep g -> TypeRep h -> TypeRep i -> TypeRep j -> TypeRep (a,b,c,d,e,f,g,h,i,j)
    Tup11Type      :: TypeRep a -> TypeRep b -> TypeRep c -> TypeRep d -> TypeRep e -> TypeRep f -> TypeRep g -> TypeRep h -> TypeRep i -> TypeRep j -> TypeRep k -> TypeRep (a,b,c,d,e,f,g,h,i,j,k)
    Tup12Type      :: TypeRep a -> TypeRep b -> TypeRep c -> TypeRep d -> TypeRep e -> TypeRep f -> TypeRep g -> TypeRep h -> TypeRep i -> TypeRep j -> TypeRep k -> TypeRep l -> TypeRep (a,b,c,d,e,f,g,h,i,j,k,l)
    Tup13Type      :: TypeRep a -> TypeRep b -> TypeRep c -> TypeRep d -> TypeRep e -> TypeRep f -> TypeRep g -> TypeRep h -> TypeRep i -> TypeRep j -> TypeRep k -> TypeRep l -> TypeRep m -> TypeRep (a,b,c,d,e,f,g,h,i,j,k,l,m)
    Tup14Type      :: TypeRep a -> TypeRep b -> TypeRep c -> TypeRep d -> TypeRep e -> TypeRep f -> TypeRep g -> TypeRep h -> TypeRep i -> TypeRep j -> TypeRep k -> TypeRep l -> TypeRep m -> TypeRep n -> TypeRep (a,b,c,d,e,f,g,h,i,j,k,l,m,n)
    Tup15Type      :: TypeRep a -> TypeRep b -> TypeRep c -> TypeRep d -> TypeRep e -> TypeRep f -> TypeRep g -> TypeRep h -> TypeRep i -> TypeRep j -> TypeRep k -> TypeRep l -> TypeRep m -> TypeRep n -> TypeRep o -> TypeRep (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)
    FunType       :: TypeRep a -> TypeRep b -> TypeRep (a -> b)
    MutType       :: TypeRep a -> TypeRep (Mut a)
    RefType       :: TypeRep a -> TypeRep (IORef a)
    MArrType      :: TypeRep a -> TypeRep (MArr a)
    ParType       :: TypeRep a -> TypeRep (Par a)
    ElementsType  :: TypeRep a -> TypeRep (Elements a)
    ConsType      :: TypeRep a -> TypeRep (RTuple b) -> TypeRep (RTuple (a :* b))
    NilType       :: TypeRep (RTuple TNil)
    TupleType     :: TypeRep (RTuple a) -> TypeRep (Tuple a)
    IVarType      :: TypeRep a -> TypeRep (IV a)
    FValType      :: TypeRep a -> TypeRep (FVal a)
      -- TODO `MArrType` Should have a target-specialized version. Or perhaps
      --      use a single type with a flag to distinguish between immutable and
      --      mutable arrays.

instance Show (TypeRep a)
  where
    show UnitType            = "()"
    show BoolType            = "Bool"
    show (IntType s n)       = signedness s ++ bitWidth n
    show FloatType           = "Float"
    show DoubleType          = "Double"
    show (ComplexType t)     = "(Complex " ++ show t ++ ")"
    show (ArrayType t)       = "[" ++ show t ++ "]"
    show (TargetArrType _ t) = "[" ++ show t ++ "]"
    show (Tup2Type ta tb)                = showTup [show ta, show tb]
    show (Tup3Type ta tb tc)             = showTup [show ta, show tb, show tc]
    show (Tup4Type ta tb tc td)          = showTup [show ta, show tb, show tc, show td]
    show (Tup5Type ta tb tc td te)       = showTup [show ta, show tb, show tc, show td, show te]
    show (Tup6Type ta tb tc td te tf)    = showTup [show ta, show tb, show tc, show td, show te, show tf]
    show (Tup7Type ta tb tc td te tf tg) = showTup [show ta, show tb, show tc, show td, show te, show tf, show tg]
    show (Tup8Type ta tb tc td te tf tg th) = showTup [show ta, show tb, show tc, show td, show te, show tf, show tg, show th]
    show (Tup9Type ta tb tc td te tf tg th ti) = showTup [show ta, show tb, show tc, show td, show te, show tf, show tg, show th, show ti]
    show (Tup10Type ta tb tc td te tf tg th ti tj) = showTup [show ta, show tb, show tc, show td, show te, show tf, show tg, show th, show ti, show tj]
    show (Tup11Type ta tb tc td te tf tg th ti tj tk) = showTup [show ta, show tb, show tc, show td, show te, show tf, show tg, show th, show ti, show tj, show tk]
    show (Tup12Type ta tb tc td te tf tg th ti tj tk tl) = showTup [show ta, show tb, show tc, show td, show te, show tf, show tg, show th, show ti, show tj, show tk, show tl]
    show (Tup13Type ta tb tc td te tf tg th ti tj tk tl tm) = showTup [show ta, show tb, show tc, show td, show te, show tf, show tg, show th, show ti, show tj, show tk, show tl, show tm]
    show (Tup14Type ta tb tc td te tf tg th ti tj tk tl tm tn) = showTup [show ta, show tb, show tc, show td, show te, show tf, show tg, show th, show ti, show tj, show tk, show tl, show tm, show tn]
    show (Tup15Type ta tb tc td te tf tg th ti tj tk tl tm tn to) = showTup [show ta, show tb, show tc, show td, show te, show tf, show tg, show th, show ti, show tj, show tk, show tl, show tm, show tn, show to]
    show (FunType ta tb)                 = show ta ++ " -> " ++ show tb
    show (MutType ta)                    = unwords ["Mut", show ta]
    show (RefType ta)                    = unwords ["Ref", show ta]
    show (MArrType ta)                   = unwords ["MArr", show ta]
    show (ParType ta)                    = unwords ["Par", show ta]
    show (ElementsType ta)               = unwords ["Elements", show ta]
    show (ConsType ta tb)                = unwords [show ta, ":*", show tb]
    show NilType                         = "TNil"
    show (TupleType t)                   = "<" ++ show t ++ ">"
    show (IVarType ta)                   = unwords ["IVar", show ta]
    show (FValType ta)                   = unwords ["FVal", show ta]

argType :: TypeRep (a -> b) -> TypeRep a
argType (FunType ta _) = ta

resType :: TypeRep (a -> b) -> TypeRep b
resType (FunType _ tb) = tb

-- | Type equality witness
data TypeEq a b
  where
    TypeEq :: TypeEq a a

defaultSize :: TypeRep a -> Size a
defaultSize UnitType = universal
defaultSize BoolType = universal
defaultSize (IntType _ _) = universal
defaultSize FloatType = universal
defaultSize DoubleType = universal
defaultSize (ComplexType _) = universal
defaultSize (ArrayType t) = universal :> defaultSize t
--defaultSize (TargetArrType n t) = universal :> defaultSize t -- TODO
defaultSize (Tup2Type ta tb) =  ( defaultSize ta
                                , defaultSize tb
                                )
defaultSize (Tup3Type ta tb tc) = ( defaultSize ta
                                  , defaultSize tb
                                  , defaultSize tc
                                  )
defaultSize (Tup4Type ta tb tc td) = ( defaultSize ta
                                     , defaultSize tb
                                     , defaultSize tc
                                     , defaultSize td
                                     )
defaultSize (Tup5Type ta tb tc td te) = ( defaultSize ta
                                        , defaultSize tb
                                        , defaultSize tc
                                        , defaultSize td
                                        , defaultSize te
                                        )
defaultSize (Tup6Type ta tb tc td te tf) = ( defaultSize ta
                                           , defaultSize tb
                                           , defaultSize tc
                                           , defaultSize td
                                           , defaultSize te
                                           , defaultSize tf
                                           )
defaultSize (Tup7Type ta tb tc td te tf tg) = ( defaultSize ta
                                              , defaultSize tb
                                              , defaultSize tc
                                              , defaultSize td
                                              , defaultSize te
                                              , defaultSize tf
                                              , defaultSize tg
                                              )
defaultSize (Tup8Type ta tb tc td te tf tg th) =
                                              ( defaultSize ta
                                              , defaultSize tb
                                              , defaultSize tc
                                              , defaultSize td
                                              , defaultSize te
                                              , defaultSize tf
                                              , defaultSize tg
                                              , defaultSize th
                                              )
defaultSize (Tup9Type ta tb tc td te tf tg th ti) =
                                              ( defaultSize ta
                                              , defaultSize tb
                                              , defaultSize tc
                                              , defaultSize td
                                              , defaultSize te
                                              , defaultSize tf
                                              , defaultSize tg
                                              , defaultSize th
                                              , defaultSize ti
                                              )
defaultSize (Tup10Type ta tb tc td te tf tg th ti tj) =
                                              ( defaultSize ta
                                              , defaultSize tb
                                              , defaultSize tc
                                              , defaultSize td
                                              , defaultSize te
                                              , defaultSize tf
                                              , defaultSize tg
                                              , defaultSize th
                                              , defaultSize ti
                                              , defaultSize tj
                                              )
defaultSize (Tup11Type ta tb tc td te tf tg th ti tj tk) =
                                              ( defaultSize ta
                                              , defaultSize tb
                                              , defaultSize tc
                                              , defaultSize td
                                              , defaultSize te
                                              , defaultSize tf
                                              , defaultSize tg
                                              , defaultSize th
                                              , defaultSize ti
                                              , defaultSize tj
                                              , defaultSize tk
                                              )
defaultSize (Tup12Type ta tb tc td te tf tg th ti tj tk tl) =
                                              ( defaultSize ta
                                              , defaultSize tb
                                              , defaultSize tc
                                              , defaultSize td
                                              , defaultSize te
                                              , defaultSize tf
                                              , defaultSize tg
                                              , defaultSize th
                                              , defaultSize ti
                                              , defaultSize tj
                                              , defaultSize tk
                                              , defaultSize tl
                                              )
defaultSize (Tup13Type ta tb tc td te tf tg th ti tj tk tl tm) =
                                              ( defaultSize ta
                                              , defaultSize tb
                                              , defaultSize tc
                                              , defaultSize td
                                              , defaultSize te
                                              , defaultSize tf
                                              , defaultSize tg
                                              , defaultSize th
                                              , defaultSize ti
                                              , defaultSize tj
                                              , defaultSize tk
                                              , defaultSize tl
                                              , defaultSize tm
                                              )
defaultSize (Tup14Type ta tb tc td te tf tg th ti tj tk tl tm tn) =
                                              ( defaultSize ta
                                              , defaultSize tb
                                              , defaultSize tc
                                              , defaultSize td
                                              , defaultSize te
                                              , defaultSize tf
                                              , defaultSize tg
                                              , defaultSize th
                                              , defaultSize ti
                                              , defaultSize tj
                                              , defaultSize tk
                                              , defaultSize tl
                                              , defaultSize tm
                                              , defaultSize tn
                                              )
defaultSize (Tup15Type ta tb tc td te tf tg th ti tj tk tl tm tn to) =
                                              ( defaultSize ta
                                              , defaultSize tb
                                              , defaultSize tc
                                              , defaultSize td
                                              , defaultSize te
                                              , defaultSize tf
                                              , defaultSize tg
                                              , defaultSize th
                                              , defaultSize ti
                                              , defaultSize tj
                                              , defaultSize tk
                                              , defaultSize tl
                                              , defaultSize tm
                                              , defaultSize tn
                                              , defaultSize to
                                              )
defaultSize (FunType ta tb) = (defaultSize ta, defaultSize tb)
defaultSize (MutType ta) = defaultSize ta
defaultSize (RefType ta) = defaultSize ta
defaultSize (MArrType ta) = universal :> defaultSize ta
defaultSize (ParType ta) = defaultSize ta
defaultSize (ElementsType ta) = universal :> defaultSize ta
defaultSize (ConsType ta tb) = (defaultSize ta, defaultSize tb)
defaultSize NilType = universal
defaultSize (TupleType t) = defaultSize t
defaultSize (IVarType ta) = defaultSize ta
defaultSize (FValType ta) = defaultSize ta

-- | Type equality on 'Signedness'
signEq :: Signedness s1 -> Signedness s2 -> Maybe (TypeEq s1 s2)
signEq U U = Just TypeEq
signEq S S = Just TypeEq
signEq _ _ = Nothing

-- | Type equality on 'BitWidth'
widthEq :: BitWidth n1 -> BitWidth n2 -> Maybe (TypeEq n1 n2)
widthEq N8      N8      = Just TypeEq
widthEq N16     N16     = Just TypeEq
widthEq N32     N32     = Just TypeEq
widthEq N64     N64     = Just TypeEq
widthEq NNative NNative = Just TypeEq
widthEq _ _ = Nothing

-- | Type equality on 'TypeRep'
typeEq :: TypeRep a -> TypeRep b -> Maybe (TypeEq a b)
typeEq UnitType UnitType = Just TypeEq
typeEq BoolType BoolType = Just TypeEq
typeEq (IntType s1 n1) (IntType s2 n2) = do
    TypeEq <- signEq s1 s2
    TypeEq <- widthEq n1 n2
    return TypeEq
typeEq FloatType FloatType = Just TypeEq
typeEq DoubleType DoubleType = Just TypeEq
typeEq (ComplexType t1) (ComplexType t2) = do
    TypeEq <- typeEq t1 t2
    return TypeEq
typeEq (ArrayType t1) (ArrayType t2) = do
    TypeEq <- typeEq t1 t2
    return TypeEq
typeEq (TargetArrType n1 t1) (TargetArrType n2 t2) = do
    TypeEq <- widthEq n1 n2
    TypeEq <- typeEq t1 t2
    return TypeEq
typeEq (Tup2Type a1 b1) (Tup2Type a2 b2) = do
    TypeEq <- typeEq a1 a2
    TypeEq <- typeEq b1 b2
    return TypeEq
typeEq (Tup3Type a1 b1 c1) (Tup3Type a2 b2 c2) = do
    TypeEq <- typeEq a1 a2
    TypeEq <- typeEq b1 b2
    TypeEq <- typeEq c1 c2
    return TypeEq
typeEq (Tup4Type a1 b1 c1 d1) (Tup4Type a2 b2 c2 d2) = do
    TypeEq <- typeEq a1 a2
    TypeEq <- typeEq b1 b2
    TypeEq <- typeEq c1 c2
    TypeEq <- typeEq d1 d2
    return TypeEq
typeEq (Tup5Type a1 b1 c1 d1 e1) (Tup5Type a2 b2 c2 d2 e2) = do
    TypeEq <- typeEq a1 a2
    TypeEq <- typeEq b1 b2
    TypeEq <- typeEq c1 c2
    TypeEq <- typeEq d1 d2
    TypeEq <- typeEq e1 e2
    return TypeEq
typeEq (Tup6Type a1 b1 c1 d1 e1 f1) (Tup6Type a2 b2 c2 d2 e2 f2) = do
    TypeEq <- typeEq a1 a2
    TypeEq <- typeEq b1 b2
    TypeEq <- typeEq c1 c2
    TypeEq <- typeEq d1 d2
    TypeEq <- typeEq e1 e2
    TypeEq <- typeEq f1 f2
    return TypeEq
typeEq (Tup7Type a1 b1 c1 d1 e1 f1 g1) (Tup7Type a2 b2 c2 d2 e2 f2 g2) = do
    TypeEq <- typeEq a1 a2
    TypeEq <- typeEq b1 b2
    TypeEq <- typeEq c1 c2
    TypeEq <- typeEq d1 d2
    TypeEq <- typeEq e1 e2
    TypeEq <- typeEq f1 f2
    TypeEq <- typeEq g1 g2
    return TypeEq
typeEq (Tup8Type a1 b1 c1 d1 e1 f1 g1 h1) (Tup8Type a2 b2 c2 d2 e2 f2 g2 h2) = do
    TypeEq <- typeEq a1 a2
    TypeEq <- typeEq b1 b2
    TypeEq <- typeEq c1 c2
    TypeEq <- typeEq d1 d2
    TypeEq <- typeEq e1 e2
    TypeEq <- typeEq f1 f2
    TypeEq <- typeEq g1 g2
    TypeEq <- typeEq h1 h2
    return TypeEq
typeEq (Tup9Type a1 b1 c1 d1 e1 f1 g1 h1 i1) (Tup9Type a2 b2 c2 d2 e2 f2 g2 h2 i2) = do
    TypeEq <- typeEq a1 a2
    TypeEq <- typeEq b1 b2
    TypeEq <- typeEq c1 c2
    TypeEq <- typeEq d1 d2
    TypeEq <- typeEq e1 e2
    TypeEq <- typeEq f1 f2
    TypeEq <- typeEq g1 g2
    TypeEq <- typeEq h1 h2
    TypeEq <- typeEq i1 i2
    return TypeEq
typeEq (Tup10Type a1 b1 c1 d1 e1 f1 g1 h1 i1 j1) (Tup10Type a2 b2 c2 d2 e2 f2 g2 h2 i2 j2) = do
    TypeEq <- typeEq a1 a2
    TypeEq <- typeEq b1 b2
    TypeEq <- typeEq c1 c2
    TypeEq <- typeEq d1 d2
    TypeEq <- typeEq e1 e2
    TypeEq <- typeEq f1 f2
    TypeEq <- typeEq g1 g2
    TypeEq <- typeEq h1 h2
    TypeEq <- typeEq i1 i2
    TypeEq <- typeEq j1 j2
    return TypeEq
typeEq (Tup11Type a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1) (Tup11Type a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2) = do
    TypeEq <- typeEq a1 a2
    TypeEq <- typeEq b1 b2
    TypeEq <- typeEq c1 c2
    TypeEq <- typeEq d1 d2
    TypeEq <- typeEq e1 e2
    TypeEq <- typeEq f1 f2
    TypeEq <- typeEq g1 g2
    TypeEq <- typeEq h1 h2
    TypeEq <- typeEq i1 i2
    TypeEq <- typeEq j1 j2
    TypeEq <- typeEq k1 k2
    return TypeEq
typeEq (Tup12Type a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1) (Tup12Type a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2) = do
    TypeEq <- typeEq a1 a2
    TypeEq <- typeEq b1 b2
    TypeEq <- typeEq c1 c2
    TypeEq <- typeEq d1 d2
    TypeEq <- typeEq e1 e2
    TypeEq <- typeEq f1 f2
    TypeEq <- typeEq g1 g2
    TypeEq <- typeEq h1 h2
    TypeEq <- typeEq i1 i2
    TypeEq <- typeEq j1 j2
    TypeEq <- typeEq k1 k2
    TypeEq <- typeEq l1 l2
    return TypeEq
typeEq (Tup13Type a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1) (Tup13Type a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2 m2) = do
    TypeEq <- typeEq a1 a2
    TypeEq <- typeEq b1 b2
    TypeEq <- typeEq c1 c2
    TypeEq <- typeEq d1 d2
    TypeEq <- typeEq e1 e2
    TypeEq <- typeEq f1 f2
    TypeEq <- typeEq g1 g2
    TypeEq <- typeEq h1 h2
    TypeEq <- typeEq i1 i2
    TypeEq <- typeEq j1 j2
    TypeEq <- typeEq k1 k2
    TypeEq <- typeEq l1 l2
    TypeEq <- typeEq m1 m2
    return TypeEq
typeEq (Tup14Type a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1) (Tup14Type a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2 m2 n2) = do
    TypeEq <- typeEq a1 a2
    TypeEq <- typeEq b1 b2
    TypeEq <- typeEq c1 c2
    TypeEq <- typeEq d1 d2
    TypeEq <- typeEq e1 e2
    TypeEq <- typeEq f1 f2
    TypeEq <- typeEq g1 g2
    TypeEq <- typeEq h1 h2
    TypeEq <- typeEq i1 i2
    TypeEq <- typeEq j1 j2
    TypeEq <- typeEq k1 k2
    TypeEq <- typeEq l1 l2
    TypeEq <- typeEq m1 m2
    TypeEq <- typeEq n1 n2
    return TypeEq
typeEq (Tup15Type a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1) (Tup15Type a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2 m2 n2 o2) = do
    TypeEq <- typeEq a1 a2
    TypeEq <- typeEq b1 b2
    TypeEq <- typeEq c1 c2
    TypeEq <- typeEq d1 d2
    TypeEq <- typeEq e1 e2
    TypeEq <- typeEq f1 f2
    TypeEq <- typeEq g1 g2
    TypeEq <- typeEq h1 h2
    TypeEq <- typeEq i1 i2
    TypeEq <- typeEq j1 j2
    TypeEq <- typeEq k1 k2
    TypeEq <- typeEq l1 l2
    TypeEq <- typeEq m1 m2
    TypeEq <- typeEq n1 n2
    TypeEq <- typeEq o1 o2
    return TypeEq
typeEq (FunType a1 b1) (FunType a2 b2) = do
    TypeEq <- typeEq a1 a2
    TypeEq <- typeEq b1 b2
    return TypeEq
typeEq (MutType t1) (MutType t2) = do
    TypeEq <- typeEq t1 t2
    return TypeEq
typeEq (RefType t1) (RefType t2) = do
    TypeEq <- typeEq t1 t2
    return TypeEq
typeEq (MArrType a1) (MArrType a2) = do
    TypeEq <- typeEq a1 a2
    return TypeEq
typeEq (ParType t1) (ParType t2) = do
    TypeEq <- typeEq t1 t2
    return TypeEq
typeEq (ElementsType t1) (ElementsType t2) = do
    TypeEq <- typeEq t1 t2
    return TypeEq
typeEq (ConsType a1 b1) (ConsType a2 b2) = do
    TypeEq <- typeEq a1 a2
    TypeEq <- typeEq b1 b2
    return TypeEq
typeEq NilType NilType = do
    return TypeEq
typeEq (TupleType t1) (TupleType t2) = do
    TypeEq <- typeEq t1 t2
    return TypeEq
typeEq (IVarType t1) (IVarType t2) = do
    TypeEq <- typeEq t1 t2
    return TypeEq
typeEq (FValType t1) (FValType t2) = do
    TypeEq <- typeEq t1 t2
    return TypeEq

typeEq _ _ = Nothing

showTup :: [String] -> String
showTup as =  "(" ++ intercalate "," as ++ ")"

type family TargetType n a
type instance TargetType n ()              = ()
type instance TargetType n Bool            = Bool
type instance TargetType n Word8           = Word8
type instance TargetType n Int8            = Int8
type instance TargetType n Word16          = Word16
type instance TargetType n Int16           = Int16
type instance TargetType n Word32          = Word32
type instance TargetType n Int32           = Int32
type instance TargetType n Word64          = Word64
type instance TargetType n Int64           = Int64
type instance TargetType n WordN           = GenericInt U n
type instance TargetType n IntN            = GenericInt S n
type instance TargetType n Float           = Float
type instance TargetType n Double          = Double
type instance TargetType n (Complex a)     = Complex (TargetType n a)
type instance TargetType n [a]             = TargetArr n (TargetType n a)
type instance TargetType n (a,b)           = (TargetType n a, TargetType n b)
type instance TargetType n (a,b,c)         = (TargetType n a, TargetType n b, TargetType n c)
type instance TargetType n (a,b,c,d)       = (TargetType n a, TargetType n b, TargetType n c, TargetType n d)
type instance TargetType n (a,b,c,d,e)     = (TargetType n a, TargetType n b, TargetType n c, TargetType n d, TargetType n e)
type instance TargetType n (a,b,c,d,e,f)   = (TargetType n a, TargetType n b, TargetType n c, TargetType n d, TargetType n e, TargetType n f)
type instance TargetType n (a,b,c,d,e,f,g) = (TargetType n a, TargetType n b, TargetType n c, TargetType n d, TargetType n e, TargetType n f, TargetType n g)
type instance TargetType n (a,b,c,d,e,f,g,h) = (TargetType n a, TargetType n b, TargetType n c, TargetType n d, TargetType n e, TargetType n f, TargetType n g, TargetType n h)
type instance TargetType n (a,b,c,d,e,f,g,h,i) = (TargetType n a, TargetType n b, TargetType n c, TargetType n d, TargetType n e, TargetType n f, TargetType n g, TargetType n h, TargetType n i)
type instance TargetType n (a,b,c,d,e,f,g,h,i,j) = (TargetType n a, TargetType n b, TargetType n c, TargetType n d, TargetType n e, TargetType n f, TargetType n g, TargetType n h, TargetType n i, TargetType n j)
type instance TargetType n (a,b,c,d,e,f,g,h,i,j,k) = (TargetType n a, TargetType n b, TargetType n c, TargetType n d, TargetType n e, TargetType n f, TargetType n g, TargetType n h, TargetType n i, TargetType n j, TargetType n k)
type instance TargetType n (a,b,c,d,e,f,g,h,i,j,k,l) = (TargetType n a, TargetType n b, TargetType n c, TargetType n d, TargetType n e, TargetType n f, TargetType n g, TargetType n h, TargetType n i, TargetType n j, TargetType n k, TargetType n l)
type instance TargetType n (a,b,c,d,e,f,g,h,i,j,k,l,m) = (TargetType n a, TargetType n b, TargetType n c, TargetType n d, TargetType n e, TargetType n f, TargetType n g, TargetType n h, TargetType n i, TargetType n j, TargetType n k, TargetType n l, TargetType n m)
type instance TargetType n (a,b,c,d,e,f,g,h,i,j,k,l,m,n') = (TargetType n a, TargetType n b, TargetType n c, TargetType n d, TargetType n e, TargetType n f, TargetType n g, TargetType n h, TargetType n i, TargetType n j, TargetType n k, TargetType n l, TargetType n m, TargetType n n')
type instance TargetType n (a,b,c,d,e,f,g,h,i,j,k,l,m,n',o) = (TargetType n a, TargetType n b, TargetType n c, TargetType n d, TargetType n e, TargetType n f, TargetType n g, TargetType n h, TargetType n i, TargetType n j, TargetType n k, TargetType n l, TargetType n m, TargetType n n', TargetType n o)
type instance TargetType n (IORef a)       = IORef (TargetType n a)
type instance TargetType n (MArr a)        = MArr (TargetType n a)
type instance TargetType n (Elements a)    = Elements (TargetType n a)
type instance TargetType n (RTuple (a:*b)) = RTuple (TargetType n a :* TargetType n b)
type instance TargetType n (RTuple TNil)   = Tuple TNil
type instance TargetType n (Tuple a)       = Tuple (TargetType n a)
type instance TargetType n (IV a)          = IV (TargetType n a)
type instance TargetType n (FVal a)        = FVal (TargetType n a)

-- | The set of supported types
class (Eq a, Show a, Typeable a, Show (Size a), Lattice (Size a), TypeF a) => Type a
  where
    -- | Gives the type representation a value.
    typeRep  :: TypeRep a
    sizeOf   :: a -> Size a
    toTarget :: BitWidth n -> a -> TargetType n a

instance Type ()      where typeRep = UnitType;          sizeOf _ = AnySize;      toTarget _ = id
instance Type Bool    where typeRep = BoolType;          sizeOf _ = AnySize;      toTarget _ = id
instance Type Word8   where typeRep = IntType U N8;      sizeOf = singletonRange; toTarget _ = id
instance Type Int8    where typeRep = IntType S N8;      sizeOf = singletonRange; toTarget _ = id
instance Type Word16  where typeRep = IntType U N16;     sizeOf = singletonRange; toTarget _ = id
instance Type Int16   where typeRep = IntType S N16;     sizeOf = singletonRange; toTarget _ = id
instance Type Word32  where typeRep = IntType U N32;     sizeOf = singletonRange; toTarget _ = id
instance Type Int32   where typeRep = IntType S N32;     sizeOf = singletonRange; toTarget _ = id
instance Type Word64  where typeRep = IntType U N64;     sizeOf = singletonRange; toTarget _ = id
instance Type Int64   where typeRep = IntType S N64;     sizeOf = singletonRange; toTarget _ = id
instance Type WordN   where typeRep = IntType U NNative; sizeOf = singletonRange; toTarget = fromWordN
instance Type IntN    where typeRep = IntType S NNative; sizeOf = singletonRange; toTarget = fromIntN
instance Type Float   where typeRep = FloatType;         sizeOf _ = AnySize;      toTarget _ = id
instance Type Double  where typeRep = DoubleType;        sizeOf _ = AnySize;      toTarget _ = id

instance (Type a, RealFloat a) => Type (Complex a)
  where
    typeRep             = ComplexType typeRep
    sizeOf _            = AnySize
    toTarget _ (_ :+ _) = error "TODO" -- toTarget n r :+ toTarget n i

instance Type a => Type [a]
  where
    typeRep       = ArrayType typeRep
    sizeOf as     = singletonRange (genericLength as) :> unions (map sizeOf as)
    toTarget n as = TargetArr (genericLen n as) $ map (toTarget n) as

instance (Type a, Type b) => Type (a,b)
  where
    typeRep = Tup2Type typeRep typeRep

    sizeOf (a,b) =
        ( sizeOf a
        , sizeOf b
        )

    toTarget n (a,b) =
        ( toTarget n a
        , toTarget n b
        )

instance (Type a, Type b, Type c) => Type (a,b,c)
  where
    typeRep = Tup3Type typeRep typeRep typeRep

    sizeOf (a,b,c) =
        ( sizeOf a
        , sizeOf b
        , sizeOf c
        )

    toTarget n (a,b,c) =
        ( toTarget n a
        , toTarget n b
        , toTarget n c
        )

instance (Type a, Type b, Type c, Type d) => Type (a,b,c,d)
  where
    typeRep = Tup4Type typeRep typeRep typeRep typeRep

    sizeOf (a,b,c,d) =
        ( sizeOf a
        , sizeOf b
        , sizeOf c
        , sizeOf d
        )

    toTarget n (a,b,c,d) =
        ( toTarget n a
        , toTarget n b
        , toTarget n c
        , toTarget n d
        )

instance (Type a, Type b, Type c, Type d, Type e) => Type (a,b,c,d,e)
  where
    typeRep = Tup5Type typeRep typeRep typeRep typeRep typeRep

    sizeOf (a,b,c,d,e) =
        ( sizeOf a
        , sizeOf b
        , sizeOf c
        , sizeOf d
        , sizeOf e
        )

    toTarget n (a,b,c,d,e) =
        ( toTarget n a
        , toTarget n b
        , toTarget n c
        , toTarget n d
        , toTarget n e
        )

instance (Type a, Type b, Type c, Type d, Type e, Type f) => Type (a,b,c,d,e,f)
  where
    typeRep = Tup6Type typeRep typeRep typeRep typeRep typeRep typeRep

    sizeOf (a,b,c,d,e,f) =
        ( sizeOf a
        , sizeOf b
        , sizeOf c
        , sizeOf d
        , sizeOf e
        , sizeOf f
        )

    toTarget n (a,b,c,d,e,f) =
        ( toTarget n a
        , toTarget n b
        , toTarget n c
        , toTarget n d
        , toTarget n e
        , toTarget n f
        )

instance (Type a, Type b, Type c, Type d, Type e, Type f, Type g) => Type (a,b,c,d,e,f,g)
  where
    typeRep = Tup7Type typeRep typeRep typeRep typeRep typeRep typeRep typeRep

    sizeOf (a,b,c,d,e,f,g) =
        ( sizeOf a
        , sizeOf b
        , sizeOf c
        , sizeOf d
        , sizeOf e
        , sizeOf f
        , sizeOf g
        )

    toTarget n (a,b,c,d,e,f,g) =
        ( toTarget n a
        , toTarget n b
        , toTarget n c
        , toTarget n d
        , toTarget n e
        , toTarget n f
        , toTarget n g
        )


instance (Type a, Type b, Type c, Type d, Type e, Type f, Type g, Type h) => Type (a,b,c,d,e,f,g,h)
  where
    typeRep = Tup8Type typeRep typeRep typeRep typeRep typeRep typeRep typeRep typeRep

    sizeOf (a,b,c,d,e,f,g,h) =
        ( sizeOf a
        , sizeOf b
        , sizeOf c
        , sizeOf d
        , sizeOf e
        , sizeOf f
        , sizeOf g
        , sizeOf h
        )

    toTarget n (a,b,c,d,e,f,g,h) =
        ( toTarget n a
        , toTarget n b
        , toTarget n c
        , toTarget n d
        , toTarget n e
        , toTarget n f
        , toTarget n g
        , toTarget n h
        )

instance (Type a, Type b, Type c, Type d, Type e, Type f, Type g, Type h, Type i) => Type (a,b,c,d,e,f,g,h,i)
  where
    typeRep = Tup9Type typeRep typeRep typeRep typeRep typeRep typeRep typeRep typeRep typeRep

    sizeOf (a,b,c,d,e,f,g,h,i) =
        ( sizeOf a
        , sizeOf b
        , sizeOf c
        , sizeOf d
        , sizeOf e
        , sizeOf f
        , sizeOf g
        , sizeOf h
        , sizeOf i
        )

    toTarget n (a,b,c,d,e,f,g,h,i) =
        ( toTarget n a
        , toTarget n b
        , toTarget n c
        , toTarget n d
        , toTarget n e
        , toTarget n f
        , toTarget n g
        , toTarget n h
        , toTarget n i
        )

instance (Type a, Type b, Type c, Type d, Type e, Type f, Type g, Type h, Type i, Type j) => Type (a,b,c,d,e,f,g,h,i,j)
  where
    typeRep = Tup10Type typeRep typeRep typeRep typeRep typeRep typeRep typeRep typeRep typeRep typeRep

    sizeOf (a,b,c,d,e,f,g,h,i,j) =
        ( sizeOf a
        , sizeOf b
        , sizeOf c
        , sizeOf d
        , sizeOf e
        , sizeOf f
        , sizeOf g
        , sizeOf h
        , sizeOf i
        , sizeOf j
        )

    toTarget n (a,b,c,d,e,f,g,h,i,j) =
        ( toTarget n a
        , toTarget n b
        , toTarget n c
        , toTarget n d
        , toTarget n e
        , toTarget n f
        , toTarget n g
        , toTarget n h
        , toTarget n i
        , toTarget n j
        )

instance (Type a, Type b, Type c, Type d, Type e, Type f, Type g, Type h, Type i, Type j, Type k) => Type (a,b,c,d,e,f,g,h,i,j,k)
  where
    typeRep = Tup11Type typeRep typeRep typeRep typeRep typeRep typeRep typeRep typeRep typeRep typeRep typeRep

    sizeOf (a,b,c,d,e,f,g,h,i,j,k) =
        ( sizeOf a
        , sizeOf b
        , sizeOf c
        , sizeOf d
        , sizeOf e
        , sizeOf f
        , sizeOf g
        , sizeOf h
        , sizeOf i
        , sizeOf j
        , sizeOf k
        )

    toTarget n (a,b,c,d,e,f,g,h,i,j,k) =
        ( toTarget n a
        , toTarget n b
        , toTarget n c
        , toTarget n d
        , toTarget n e
        , toTarget n f
        , toTarget n g
        , toTarget n h
        , toTarget n i
        , toTarget n j
        , toTarget n k
        )

instance (Type a, Type b, Type c, Type d, Type e, Type f, Type g, Type h, Type i, Type j, Type k, Type l) => Type (a,b,c,d,e,f,g,h,i,j,k,l)
  where
    typeRep = Tup12Type typeRep typeRep typeRep typeRep typeRep typeRep typeRep typeRep typeRep typeRep typeRep typeRep

    sizeOf (a,b,c,d,e,f,g,h,i,j,k,l) =
        ( sizeOf a
        , sizeOf b
        , sizeOf c
        , sizeOf d
        , sizeOf e
        , sizeOf f
        , sizeOf g
        , sizeOf h
        , sizeOf i
        , sizeOf j
        , sizeOf k
        , sizeOf l
        )

    toTarget n (a,b,c,d,e,f,g,h,i,j,k,l) =
        ( toTarget n a
        , toTarget n b
        , toTarget n c
        , toTarget n d
        , toTarget n e
        , toTarget n f
        , toTarget n g
        , toTarget n h
        , toTarget n i
        , toTarget n j
        , toTarget n k
        , toTarget n l
        )

instance (Type a, Type b, Type c, Type d, Type e, Type f, Type g, Type h, Type i, Type j, Type k, Type l, Type m) => Type (a,b,c,d,e,f,g,h,i,j,k,l,m)
  where
    typeRep = Tup13Type typeRep typeRep typeRep typeRep typeRep typeRep typeRep typeRep typeRep typeRep typeRep typeRep typeRep

    sizeOf (a,b,c,d,e,f,g,h,i,j,k,l,m) =
        ( sizeOf a
        , sizeOf b
        , sizeOf c
        , sizeOf d
        , sizeOf e
        , sizeOf f
        , sizeOf g
        , sizeOf h
        , sizeOf i
        , sizeOf j
        , sizeOf k
        , sizeOf l
        , sizeOf m
        )

    toTarget n (a,b,c,d,e,f,g,h,i,j,k,l,m) =
        ( toTarget n a
        , toTarget n b
        , toTarget n c
        , toTarget n d
        , toTarget n e
        , toTarget n f
        , toTarget n g
        , toTarget n h
        , toTarget n i
        , toTarget n j
        , toTarget n k
        , toTarget n l
        , toTarget n m
        )

instance (Type a, Type b, Type c, Type d, Type e, Type f, Type g, Type h, Type i, Type j, Type k, Type l, Type m,Type n') => Type (a,b,c,d,e,f,g,h,i,j,k,l,m,n')
  where
    typeRep = Tup14Type typeRep typeRep typeRep typeRep typeRep typeRep typeRep typeRep typeRep typeRep typeRep typeRep typeRep typeRep

    sizeOf (a,b,c,d,e,f,g,h,i,j,k,l,m,n') =
        ( sizeOf a
        , sizeOf b
        , sizeOf c
        , sizeOf d
        , sizeOf e
        , sizeOf f
        , sizeOf g
        , sizeOf h
        , sizeOf i
        , sizeOf j
        , sizeOf k
        , sizeOf l
        , sizeOf m
        , sizeOf n'
        )

    toTarget n (a,b,c,d,e,f,g,h,i,j,k,l,m,n') =
        ( toTarget n a
        , toTarget n b
        , toTarget n c
        , toTarget n d
        , toTarget n e
        , toTarget n f
        , toTarget n g
        , toTarget n h
        , toTarget n i
        , toTarget n j
        , toTarget n k
        , toTarget n l
        , toTarget n m
        , toTarget n n'
        )

instance (Type a, Type b, Type c, Type d, Type e, Type f, Type g, Type h, Type i, Type j, Type k, Type l, Type m, Type n', Type o) => Type (a,b,c,d,e,f,g,h,i,j,k,l,m,n',o)
  where
    typeRep = Tup15Type typeRep typeRep typeRep typeRep typeRep typeRep typeRep typeRep typeRep typeRep typeRep typeRep typeRep typeRep typeRep

    sizeOf (a,b,c,d,e,f,g,h,i,j,k,l,m,n',o) =
        ( sizeOf a
        , sizeOf b
        , sizeOf c
        , sizeOf d
        , sizeOf e
        , sizeOf f
        , sizeOf g
        , sizeOf h
        , sizeOf i
        , sizeOf j
        , sizeOf k
        , sizeOf l
        , sizeOf m
        , sizeOf n'
        , sizeOf o
        )

    toTarget n (a,b,c,d,e,f,g,h,i,j,k,l,m,n',o) =
        ( toTarget n a
        , toTarget n b
        , toTarget n c
        , toTarget n d
        , toTarget n e
        , toTarget n f
        , toTarget n g
        , toTarget n h
        , toTarget n i
        , toTarget n j
        , toTarget n k
        , toTarget n l
        , toTarget n m
        , toTarget n n'
        , toTarget n o
        )

instance (Type a, Show (IORef a)) => Type (IORef a)
  where
    typeRep = RefType typeRep

    sizeOf _ = universal

    toTarget = error "toTarget: IORef"  -- TODO Requires IO

instance Type a => Type (MArr a)
  where
    typeRep = MArrType typeRep

    sizeOf _ = universal

    toTarget = error "toTarget: MArr"  -- TODO Requires IO

instance Type a => Type (Elements a)
  where
    typeRep = ElementsType typeRep

    sizeOf _ = universal

    toTarget = error "toTarget: Elements"  -- TODO Requires IO

instance (Typeable a, TypeF (RTuple a), Eq (Tuple a), Show (Tuple a), Size (Tuple a) ~ Size (RTuple a))
  => Type (Tuple a)
  where
    typeRep = TupleType typeRepF

    sizeOf (Tuple xs) = sizeOfF xs

    toTarget = error "toTarget: Tuple"

instance Type a => Type (IV a)
  where
    typeRep = IVarType typeRep

    sizeOf _ = universal

    toTarget = error "toTarget: IVar" -- TODO Requires IO

instance Type a => Type (FVal a)
  where
    typeRep = FValType typeRep

    sizeOf _ = universal

    toTarget = error "toTarget: FVal" -- TODO



typeRepByProxy :: Type a => Proxy a -> TypeRep a
typeRepByProxy _ = typeRep

-- | Extend the class Type to higher order types
class (Typeable a, Show (Size a), Lattice (Size a)) => TypeF a where
  typeRepF :: TypeRep a
  sizeOfF :: a -> Size a

instance {-# OVERLAPPING #-} (TypeF a, TypeF b) => TypeF (a -> b) where
  typeRepF = FunType typeRepF typeRepF
  sizeOfF f = universal

instance {-# OVERLAPPING #-} (Type a, Typeable b, TypeF (RTuple b)) => TypeF (RTuple (a :* b))
  where
    typeRepF = ConsType typeRep typeRepF
    sizeOfF (x :* xs) = (sizeOf x, sizeOfF xs)

instance {-# OVERLAPPING #-} TypeF (RTuple TNil)
  where
    typeRepF = NilType
    sizeOfF _ = universal

instance {-# OVERLAPPABLE #-} (Typeable a, Type a) => TypeF a where
  typeRepF = typeRep
  sizeOfF = sizeOf



--------------------------------------------------------------------------------
-- * Sized types
--------------------------------------------------------------------------------

data AnySize = AnySize
    deriving (Eq, Show, Ord)

anySizeFun :: AnySize -> AnySize
anySizeFun AnySize = AnySize

anySizeFun2 :: AnySize -> AnySize -> AnySize
anySizeFun2 AnySize AnySize = AnySize

instance Num AnySize
  where
    fromInteger _ = AnySize
    abs           = anySizeFun
    signum        = anySizeFun
    (+)           = anySizeFun2
    (-)           = anySizeFun2
    (*)           = anySizeFun2

instance Lattice AnySize
  where
    bot  = AnySize
    top  = AnySize
    (\/) = anySizeFun2
    (/\) = anySizeFun2

type family Size a

type instance Size ()              = AnySize
type instance Size Bool            = AnySize
type instance Size Word8           = Range Word8
type instance Size Int8            = Range Int8
type instance Size Word16          = Range Word16
type instance Size Int16           = Range Int16
type instance Size Word32          = Range Word32
type instance Size Int32           = Range Int32
type instance Size Word64          = Range Word64
type instance Size Int64           = Range Int64
type instance Size WordN           = Range WordN
type instance Size IntN            = Range IntN
type instance Size Float           = AnySize
type instance Size Double          = AnySize
type instance Size (Complex a)     = AnySize
type instance Size [a]             = Range Length :> Size a
type instance Size (TargetArr n a) = Range (GenericInt U n) :> Size a
type instance Size (a,b)           = (Size a, Size b)
type instance Size (a,b,c)         = (Size a, Size b, Size c)
type instance Size (a,b,c,d)       = (Size a, Size b, Size c, Size d)
type instance Size (a,b,c,d,e)     = (Size a, Size b, Size c, Size d, Size e)
type instance Size (a,b,c,d,e,f)   = (Size a, Size b, Size c, Size d, Size e, Size f)
type instance Size (a,b,c,d,e,f,g) = (Size a, Size b, Size c, Size d, Size e, Size f, Size g)
type instance Size (a,b,c,d,e,f,g,h) = (Size a, Size b, Size c, Size d, Size e, Size f, Size g, Size h)
type instance Size (a,b,c,d,e,f,g,h,i) = (Size a, Size b, Size c, Size d, Size e, Size f, Size g, Size h, Size i)
type instance Size (a,b,c,d,e,f,g,h,i,j) = (Size a, Size b, Size c, Size d, Size e, Size f, Size g, Size h, Size i, Size j)
type instance Size (a,b,c,d,e,f,g,h,i,j,k) = (Size a, Size b, Size c, Size d, Size e, Size f, Size g, Size h, Size i, Size j, Size k)
type instance Size (a,b,c,d,e,f,g,h,i,j,k,l) = (Size a, Size b, Size c, Size d, Size e, Size f, Size g, Size h, Size i, Size j, Size k, Size l)
type instance Size (a,b,c,d,e,f,g,h,i,j,k,l,m) = (Size a, Size b, Size c, Size d, Size e, Size f, Size g, Size h, Size i, Size j, Size k, Size l, Size m)
type instance Size (a,b,c,d,e,f,g,h,i,j,k,l,m,n) = (Size a, Size b, Size c, Size d, Size e, Size f, Size g, Size h, Size i, Size j, Size k, Size l, Size m, Size n)
type instance Size (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) = (Size a, Size b, Size c, Size d, Size e, Size f, Size g, Size h, Size i, Size j, Size k, Size l, Size m, Size n, Size o)
type instance Size (a -> b)        = (Size a, Size b)
type instance Size (Mut a)         = Size a
type instance Size (IORef a)       = Size a
type instance Size (MArr a)        = Range Length :> Size a
type instance Size (Par a)         = Size a
type instance Size (Elements a)    = Range Length :> Size a
type instance Size (RTuple (a :* b)) = (Size a, Size (RTuple b))
type instance Size (RTuple TNil)   = Size ()
type instance Size (Tuple a)       = Size (RTuple a)
type instance Size (IV a)          = Size a
type instance Size (FVal a)        = Size a


-- | A generalization of 'Range' that serves two purposes: (1) Adding an extra
-- 'Universal' constructor to support unbounded types ('Range' can only
-- represent bounded ranges), and (2) pack a 'BoundedInt' constraint with the
-- 'RangeSet' constructor. This is what allows 'sizeToRange' to be defined as a
-- total function with 'Type' as the only constraint.
data RangeSet a
  where
    RangeSet  :: BoundedInt a => Range a -> RangeSet a
    Universal :: RangeSet a

-- | Cast a 'Size' to a 'RangeSet'
sizeToRange :: forall a . Type a => Size a -> RangeSet a
sizeToRange sz = case typeRep :: TypeRep a of
    IntType _ _ -> RangeSet sz
    _           -> Universal


tIntN :: Patch IntN IntN
tIntN = id

tWordN :: Patch WordN WordN
tWordN = id

tIndex :: Patch Index Index
tIndex  = id

tLength :: Patch Length Length
tLength = id

tArr :: Patch a a -> Patch [a] [a]
tArr _ = id

