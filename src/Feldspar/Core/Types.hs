{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall #-}
-- Show and Eq instances gives warnings.
{-# OPTIONS_GHC -Wno-orphans #-}

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
       , Tuple(..), build, tuple -- From NestedTuples
       ) where


import Data.Array.IO (IOArray)
import Data.Bits
import Data.Complex
import Data.Int
import Data.IORef
import Data.List
import Data.Typeable (Typeable)
import Data.Word
import qualified Control.Monad.Par as MonadPar
import GHC.TypeLits

import Feldspar.Core.Tuple (Tuply(..))
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

-- | Convenience alias for bounded integers
type BoundedInt a = (Ord a, Bounded a, Integral a, FiniteBits a,
                     Integral (UnsignedRep a), FiniteBits (UnsignedRep a))

-- | Witness for a type level literal. Native is encoded as 0.
data BitWidth (n :: Nat) where
    N8      :: BitWidth 8
    N16     :: BitWidth 16
    N32     :: BitWidth 32
    N64     :: BitWidth 64

-- | Witness for 'U' or 'S'
data Signedness (s :: Symbol) where
    U :: Signedness "U"
    S :: Signedness "S"

signedness :: Signedness s -> String
signedness U = "Word"
signedness S = "Int"

-- | A generalization of unsigned and signed integers. The first parameter
-- represents the signedness and the second parameter the number of bits.
type family GenericInt s n where
  GenericInt "U" 8      = Word8
  GenericInt "S" 8      = Int8
  GenericInt "U" 16     = Word16
  GenericInt "S" 16     = Int16
  GenericInt "U" 32     = Word32
  GenericInt "S" 32     = Int32
  GenericInt "U" 64     = Word64
  GenericInt "S" 64     = Int64

-- FIXME: IntN/WordN are hardcoded, not based on Options for architecture.

-- | Signed "natural" representation on an architecture
type IntN = Int32

-- | Unsigned "natural" representation on an architecture
type WordN = Word32

type Length = WordN
type Index  = WordN

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
    IntType       :: ( KnownNat n, KnownSymbol s, BoundedInt (GenericInt s n)
                     , Size (GenericInt s n) ~ Range (GenericInt s n)
                     ) =>
                       Signedness s -> BitWidth n -> TypeRep (GenericInt s n)
    FloatType     :: TypeRep Float
    DoubleType    :: TypeRep Double
    ComplexType   :: RealFloat a => TypeRep a -> TypeRep (Complex a)
    ArrayType     :: TypeRep a -> TypeRep [a]
    Tup2Type      :: TypeRep (Tuple '[a, b]) -> TypeRep (a,b)
    Tup3Type      :: TypeRep (Tuple '[a, b, c]) -> TypeRep (a,b,c)
    Tup4Type      :: TypeRep (Tuple '[a, b, c, d]) -> TypeRep (a, b, c, d)
    Tup5Type      :: TypeRep (Tuple '[a, b, c, d, e]) -> TypeRep (a, b, c, d, e)
    Tup6Type      :: TypeRep (Tuple '[a, b, c, d, e, f])
                  -> TypeRep (a, b, c, d, e, f)
    Tup7Type      :: TypeRep (Tuple '[a, b, c, d, e, f, g])
                  -> TypeRep (a, b, c, d, e, f, g)
    Tup8Type      :: TypeRep (Tuple '[a, b, c, d, e, f, g, h])
                  -> TypeRep (a, b, c, d, e, f, g, h)
    Tup9Type      :: TypeRep (Tuple '[a, b, c, d, e, f, g, h, i])
                  -> TypeRep (a, b, c, d, e, f, g, h, i)
    Tup10Type     :: TypeRep (Tuple '[a, b, c, d, e, f, g, h, i, j])
                  -> TypeRep (a, b, c, d, e, f, g, h, i, j)
    Tup11Type     :: TypeRep (Tuple '[a, b, c, d, e, f, g, h, i, j, k])
                  -> TypeRep (a, b, c, d, e, f, g, h, i, j, k)
    Tup12Type     :: TypeRep (Tuple '[a, b, c, d, e, f, g, h, i, j, k, l])
                  -> TypeRep (a, b, c, d, e, f, g, h, i, j, k, l)
    Tup13Type     :: TypeRep (Tuple '[a, b, c, d, e, f, g, h, i, j, k, l, m])
                  -> TypeRep (a, b, c, d, e, f, g, h, i, j, k, l, m)
    Tup14Type     :: TypeRep (Tuple '[a, b, c, d, e, f, g, h, i, j, k, l, m, n])
                  -> TypeRep (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
    Tup15Type     :: TypeRep (Tuple '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o])
                  -> TypeRep (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
    FunType       :: TypeRep a -> TypeRep b -> TypeRep (a -> b)
    MutType       :: TypeRep a -> TypeRep (Mut a)
    RefType       :: TypeRep a -> TypeRep (IORef a)
    MArrType      :: TypeRep a -> TypeRep (MArr a)
    ParType       :: TypeRep a -> TypeRep (Par a)
    ElementsType  :: TypeRep a -> TypeRep (Elements a)
    ConsType      :: TypeRep a -> TypeRep (Tuple b) -> TypeRep (Tuple (a ': b))
    NilType       :: TypeRep (Tuple '[])
    IVarType      :: TypeRep a -> TypeRep (IV a)
    FValType      :: TypeRep a -> TypeRep (FVal a)
      -- TODO `MArrType` Should have a target-specialized version. Or perhaps
      --      use a single type with a flag to distinguish between immutable and
      --      mutable arrays.

instance Show (TypeRep a)
  where
    show UnitType            = "()"
    show BoolType            = "Bool"
    show (IntType s n)       = signedness s ++ show (natVal n)
    show FloatType           = "Float"
    show DoubleType          = "Double"
    show (ComplexType t)     = "(Complex " ++ show t ++ ")"
    show (ArrayType t)       = "[" ++ show t ++ "]"
    show (Tup2Type t)        = show t
    show (Tup3Type t)        = show t
    show (Tup4Type t)        = show t
    show (Tup5Type t)        = show t
    show (Tup6Type t)        = show t
    show (Tup7Type t)        = show t
    show (Tup8Type t)        = show t
    show (Tup9Type t)        = show t
    show (Tup10Type t)       = show t
    show (Tup11Type t)       = show t
    show (Tup12Type t)       = show t
    show (Tup13Type t)       = show t
    show (Tup14Type t)       = show t
    show (Tup15Type t)       = show t
    show (FunType ta tb)                 = show ta ++ " -> " ++ show tb
    show (MutType ta)                    = unwords ["Mut", show ta]
    show (RefType ta)                    = unwords ["Ref", show ta]
    show (MArrType ta)                   = unwords ["MArr", show ta]
    show (ParType ta)                    = unwords ["Par", show ta]
    show (ElementsType ta)               = unwords ["Elements", show ta]
    show (ConsType ta tb)                = unwords [show ta, ":*", show tb]
    show NilType                         = "TNil"
    show (IVarType ta)                   = unwords ["IVar", show ta]
    show (FValType ta)                   = unwords ["FVal", show ta]

-- | The set of supported types
class (Eq a, Show a, Typeable a, Show (Size a), Lattice (Size a), TypeF a) => Type a
  where
    -- | Gives the type representation a value.
    typeRep  :: TypeRep a
    sizeOf   :: a -> Size a

instance Type ()      where typeRep = UnitType;          sizeOf _ = AnySize
instance Type Bool    where typeRep = BoolType;          sizeOf = singletonRange
instance Type Word8   where typeRep = IntType U N8;      sizeOf = singletonRange
instance Type Int8    where typeRep = IntType S N8;      sizeOf = singletonRange
instance Type Word16  where typeRep = IntType U N16;     sizeOf = singletonRange
instance Type Int16   where typeRep = IntType S N16;     sizeOf = singletonRange
instance Type Word32  where typeRep = IntType U N32;     sizeOf = singletonRange
instance Type Int32   where typeRep = IntType S N32;     sizeOf = singletonRange
instance Type Word64  where typeRep = IntType U N64;     sizeOf = singletonRange
instance Type Int64   where typeRep = IntType S N64;     sizeOf = singletonRange
instance Type Float   where typeRep = FloatType;         sizeOf _ = AnySize
instance Type Double  where typeRep = DoubleType;        sizeOf _ = AnySize

instance (Type a, RealFloat a) => Type (Complex a)
  where
    typeRep             = ComplexType typeRep
    sizeOf _            = AnySize

instance Type a => Type [a]
  where
    typeRep       = ArrayType typeRep
    sizeOf as     = singletonRange (genericLength as) :> unions (map sizeOf as)

instance (Type a, Type b) => Type (a, b) where
  typeRep = Tup2Type typeRep
  sizeOf = sizeOf . unpack

instance (Type a, Type b, Type c) => Type (a, b, c) where
  typeRep = Tup3Type typeRep
  sizeOf = sizeOf . unpack

instance (Type a, Type b, Type c, Type d) => Type (a, b, c, d) where
  typeRep = Tup4Type typeRep
  sizeOf = sizeOf . unpack

instance (Type a, Type b, Type c, Type d, Type e) => Type (a, b, c, d, e) where
  typeRep = Tup5Type typeRep
  sizeOf = sizeOf . unpack

instance (Type a, Type b, Type c, Type d, Type e, Type f)
         => Type (a, b, c, d, e, f) where
  typeRep = Tup6Type typeRep
  sizeOf = sizeOf . unpack

instance (Type a, Type b, Type c, Type d, Type e, Type f, Type g)
         => Type (a, b, c, d, e, f, g) where
  typeRep = Tup7Type typeRep
  sizeOf = sizeOf . unpack

instance (Type a, Type b, Type c, Type d, Type e, Type f, Type g, Type h)
         => Type (a, b, c, d, e, f, g, h) where
  typeRep = Tup8Type typeRep
  sizeOf = sizeOf . unpack

instance (Type a, Type b, Type c, Type d, Type e, Type f, Type g, Type h,
          Type i)
         => Type (a, b, c, d, e, f, g, h, i) where
  typeRep = Tup9Type typeRep
  sizeOf = sizeOf . unpack

instance (Type a, Type b, Type c, Type d, Type e, Type f, Type g, Type h,
          Type i, Type j)
         => Type (a, b, c, d, e, f, g, h, i, j) where
  typeRep = Tup10Type typeRep
  sizeOf = sizeOf . unpack

instance (Type a, Type b, Type c, Type d, Type e, Type f, Type g, Type h,
          Type i, Type j, Type k)
         => Type (a, b, c, d, e, f, g, h, i, j, k) where
  typeRep = Tup11Type typeRep
  sizeOf = sizeOf . unpack

instance (Type a, Type b, Type c, Type d, Type e, Type f, Type g, Type h,
          Type i, Type j, Type k, Type l)
         => Type (a, b, c, d, e, f, g, h, i, j, k, l) where
  typeRep = Tup12Type typeRep
  sizeOf = sizeOf . unpack

instance (Type a, Type b, Type c, Type d, Type e, Type f, Type g, Type h,
          Type i, Type j, Type k, Type l, Type m)
         => Type (a, b, c, d, e, f, g, h, i, j, k, l, m) where
  typeRep = Tup13Type typeRep
  sizeOf = sizeOf . unpack

instance (Type a, Type b, Type c, Type d, Type e, Type f, Type g, Type h,
          Type i, Type j, Type k, Type l, Type m, Type n')
         => Type (a, b, c, d, e, f, g, h, i, j, k, l, m, n') where
  typeRep = Tup14Type typeRep
  sizeOf = sizeOf . unpack

instance (Type a, Type b, Type c, Type d, Type e, Type f, Type g, Type h,
          Type i, Type j, Type k, Type l, Type m, Type n', Type o)
         => Type (a, b, c, d, e, f, g, h, i, j, k, l, m, n', o) where
  typeRep = Tup15Type typeRep
  sizeOf = sizeOf . unpack

instance Type a => Type (IORef a)
  where
    typeRep = RefType typeRep

    sizeOf _ = universal

instance Type a => Type (MArr a)
  where
    typeRep = MArrType typeRep

    sizeOf _ = universal

instance Type a => Type (Elements a)
  where
    typeRep = ElementsType typeRep

    sizeOf _ = universal

instance (Type a, Typeable b, Type (Tuple b)) => Type (Tuple (a ': b)) where
    typeRep = ConsType typeRep typeRep
    sizeOf (x :* xs) = (sizeOf x, sizeOf xs)

instance Type (Tuple '[]) where
    typeRep = NilType
    sizeOf _ = universal

instance Type a => Type (IV a)
  where
    typeRep = IVarType typeRep

    sizeOf _ = universal

instance Type a => Type (FVal a)
  where
    typeRep = FValType typeRep

    sizeOf _ = universal

-- | Extend the class Type to higher order types
class (Typeable a, Show (Size a), Lattice (Size a)) => TypeF a where
  typeRepF :: TypeRep a
  sizeOfF :: a -> Size a

instance {-# OVERLAPPING #-} (TypeF a, TypeF b) => TypeF (a -> b) where
  typeRepF = FunType typeRepF typeRepF
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

type family Size a where
  Size ()              = AnySize
  Size Bool            = Range Bool
  Size Word8           = Range Word8
  Size Int8            = Range Int8
  Size Word16          = Range Word16
  Size Int16           = Range Int16
  Size Word32          = Range Word32
  Size Int32           = Range Int32
  Size Word64          = Range Word64
  Size Int64           = Range Int64
  Size Float           = AnySize
  Size Double          = AnySize
  Size (Complex a)     = AnySize
  Size [a]             = Range Length :> Size a
  Size (a, b)          = Size (Tuple '[a, b])
  Size (a, b, c)       = Size (Tuple '[a, b, c])
  Size (a, b, c, d)    = Size (Tuple '[a, b, c, d])
  Size (a, b, c, d, e) = Size (Tuple '[a, b, c, d, e])
  Size (a, b, c, d, e, f) = Size (Tuple '[a, b, c, d, e, f])
  Size (a, b, c, d, e, f, g) = Size (Tuple '[a, b, c, d, e, f, g])
  Size (a, b, c, d, e, f, g, h) = Size (Tuple '[a, b, c, d, e, f, g, h])
  Size (a, b, c, d, e, f, g, h, i) = Size (Tuple '[a, b, c, d, e, f, g, h, i])
  Size (a, b, c, d, e, f, g, h, i, j)
    = Size (Tuple '[a, b, c, d, e, f, g, h, i, j])
  Size (a, b, c, d, e, f, g, h, i, j, k)
    = Size (Tuple '[a, b, c, d, e, f, g, h, i, j, k])
  Size (a, b, c, d, e, f, g, h, i, j, k, l)
    = Size (Tuple '[a, b, c, d, e, f, g, h, i, j, k, l])
  Size (a, b, c, d, e, f, g, h, i, j, k, l, m)
    = Size (Tuple '[a, b, c, d, e, f, g, h, i, j, k, l, m])
  Size (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
    = Size (Tuple '[a, b, c, d, e, f, g, h, i, j, k, l, m, n])
  Size (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
    = Size (Tuple '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o])
  Size (a -> b)        = (Size a, Size b)
  Size (Mut a)         = Size a
  Size (IORef a)       = Size a
  Size (MArr a)        = Range Length :> Size a
  Size (Par a)         = Size a
  Size (Elements a)    = Range Length :> Size a
  Size (Tuple (a ': b)) = (Size a, Size (Tuple b))
  Size (Tuple '[])     = Size ()
  Size (IV a)          = Size a
  Size (FVal a)        = Size a
