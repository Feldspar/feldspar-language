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

module Feldspar.Core.Types where



import Data.Array.IO
import Data.Bits
import Data.Complex
import Data.Int
import Data.IORef
import Data.List
import Data.Typeable (Typeable, Typeable1)
import Data.Word
import Test.QuickCheck
import qualified Control.Monad.Par as MonadPar

import Data.Patch
import Data.Proxy

import Feldspar.Lattice
import Feldspar.Range



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
    , Arbitrary )

type instance UnsignedRep WordN = Word32

-- | Target-dependent signed integers
newtype IntN = IntN Int32
  deriving
    ( Eq, Ord, Num, Enum, Ix, Real, Integral, Bits, Bounded, Typeable
    , Arbitrary )

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
type MArr a = IOArray Index a

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

deriving instance Typeable1 Par

-- | Immutable references
type IV = MonadPar.IVar

deriving instance Typeable1 IV

instance Show (IV a)
  where
    show _ = "IVar"

instance Eq (IV a)
  where
    _ == _ = False

instance MonadType Par
  where
    voidTypeRep = ParType UnitType

--------------------------------------------------------------------------------
-- * Future values
--------------------------------------------------------------------------------

newtype FVal a = FVal {unFVal :: a}

deriving instance Typeable1 FVal

instance Show (FVal a)
  where
    show _ = "future"

instance Eq a => Eq (FVal a)
  where
    (FVal a) == (FVal b) = a == b


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
    ComplexType   :: RealFloat a => TypeRep a -> TypeRep (Complex a)
    ArrayType     :: TypeRep a -> TypeRep [a]
    TargetArrType :: BitWidth n -> TypeRep a -> TypeRep (TargetArr n a)
    Tup2Type      :: TypeRep a -> TypeRep b -> TypeRep (a,b)
    Tup3Type      :: TypeRep a -> TypeRep b -> TypeRep c -> TypeRep (a,b,c)
    Tup4Type      :: TypeRep a -> TypeRep b -> TypeRep c -> TypeRep d -> TypeRep (a,b,c,d)
    Tup5Type      :: TypeRep a -> TypeRep b -> TypeRep c -> TypeRep d -> TypeRep e -> TypeRep (a,b,c,d,e)
    Tup6Type      :: TypeRep a -> TypeRep b -> TypeRep c -> TypeRep d -> TypeRep e -> TypeRep f -> TypeRep (a,b,c,d,e,f)
    Tup7Type      :: TypeRep a -> TypeRep b -> TypeRep c -> TypeRep d -> TypeRep e -> TypeRep f -> TypeRep g -> TypeRep (a,b,c,d,e,f,g)
    FunType       :: TypeRep a -> TypeRep b -> TypeRep (a -> b)
    MutType       :: TypeRep a -> TypeRep (Mut a)
    RefType       :: TypeRep a -> TypeRep (IORef a)
    MArrType      :: TypeRep a -> TypeRep (MArr a)
    ParType       :: TypeRep a -> TypeRep (Par a)
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
    show (ComplexType t)     = "(Complex " ++ show t ++ ")"
    show (ArrayType t)       = "[" ++ show t ++ "]"
    show (TargetArrType _ t) = "[" ++ show t ++ "]"
    show (Tup2Type ta tb)                = showTup [show ta, show tb]
    show (Tup3Type ta tb tc)             = showTup [show ta, show tb, show tc]
    show (Tup4Type ta tb tc td)          = showTup [show ta, show tb, show tc, show td]
    show (Tup5Type ta tb tc td te)       = showTup [show ta, show tb, show tc, show td, show te]
    show (Tup6Type ta tb tc td te tf)    = showTup [show ta, show tb, show tc, show td, show te, show tf]
    show (Tup7Type ta tb tc td te tf tg) = showTup [show ta, show tb, show tc, show td, show te, show tf, show tg]
    show (FunType ta tb)                 = show ta ++ " -> " ++ show tb
    show (MutType ta)                    = unwords ["Mut", show ta]
    show (RefType ta)                    = unwords ["Ref", show ta]
    show (MArrType ta)                   = unwords ["MArr", show ta]
    show (ParType ta)                    = unwords ["Par", show ta]
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
defaultSize (FunType ta tb) = (defaultSize ta, defaultSize tb)
defaultSize (MutType ta) = defaultSize ta
defaultSize (RefType ta) = defaultSize ta
defaultSize (MArrType ta) = universal :> defaultSize ta
defaultSize (ParType ta) = defaultSize ta
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
type instance TargetType n (Complex a)     = Complex (TargetType n a)
type instance TargetType n [a]             = TargetArr n (TargetType n a)
type instance TargetType n (a,b)           = (TargetType n a, TargetType n b)
type instance TargetType n (a,b,c)         = (TargetType n a, TargetType n b, TargetType n c)
type instance TargetType n (a,b,c,d)       = (TargetType n a, TargetType n b, TargetType n c, TargetType n d)
type instance TargetType n (a,b,c,d,e)     = (TargetType n a, TargetType n b, TargetType n c, TargetType n d, TargetType n e)
type instance TargetType n (a,b,c,d,e,f)   = (TargetType n a, TargetType n b, TargetType n c, TargetType n d, TargetType n e, TargetType n f)
type instance TargetType n (a,b,c,d,e,f,g) = (TargetType n a, TargetType n b, TargetType n c, TargetType n d, TargetType n e, TargetType n f, TargetType n g)
type instance TargetType n (IORef a)       = IORef (TargetType n a)
type instance TargetType n (MArr a)        = MArr (TargetType n a)
type instance TargetType n (IV a)          = IV (TargetType n a)
type instance TargetType n (FVal a)        = FVal (TargetType n a)

-- | The set of supported types
class (Eq a, Show a, Typeable a, Show (Size a), Lattice (Size a)) => Type a
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
type instance Size (Complex a)     = AnySize
type instance Size [a]             = Range Length :> Size a
type instance Size (TargetArr n a) = Range (GenericInt U n) :> Size a
type instance Size (a,b)           = (Size a, Size b)
type instance Size (a,b,c)         = (Size a, Size b, Size c)
type instance Size (a,b,c,d)       = (Size a, Size b, Size c, Size d)
type instance Size (a,b,c,d,e)     = (Size a, Size b, Size c, Size d, Size e)
type instance Size (a,b,c,d,e,f)   = (Size a, Size b, Size c, Size d, Size e, Size f)
type instance Size (a,b,c,d,e,f,g) = (Size a, Size b, Size c, Size d, Size e, Size f, Size g)
type instance Size (a -> b)        = (Size a, Size b)
type instance Size (Mut a)         = Size a
type instance Size (IORef a)       = Size a
type instance Size (MArr a)        = Range Length :> Size a
type instance Size (Par a)         = Size a
type instance Size (IV a)          = Size a
type instance Size (FVal a)        = Size a

-- Note: The instance
--
--     Size (a -> b) = Size b
--
-- might seem strange. In general, the size of a function result depends on the
-- size of the argument, so it might be more natural to have
--
--     Size (a -> b) = Size a -> Size b
--
-- However, this doesn't really work with the `optimize` function, since
-- optimization is done simultaneously with size inference, and the two
-- influence each other. The result of optimization is an optimized expression
-- decorated with a size. If the expression is of function type, the size of the
-- argument has to be provided before optimizing the function body. Yet, the
-- result will be decorated by a value of type `Size a -> Size b`, which
-- suggests that we do not yet know the size of the argument.
--
-- So instead, we represent the size of a function as the size of its result,
-- which means that the size of a function is only valid in a given context.



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

