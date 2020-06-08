{-# LANGUAGE GADTs #-}
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
       , IntN(..), WordN(..)
       , Tuple(..), (:*), TNil -- From NestedTuples
       ) where


import Data.Array.IO (IOArray)
import Data.Complex
import Data.Int
import Data.IORef
import Data.List
import Data.Typeable (Typeable)
import Data.Word
import qualified Control.Monad.Par as MonadPar

import Data.Patch

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
-- represents the signedness and the second parameter the number of bits.
type family GenericInt s n where
  GenericInt U N8      = Word8
  GenericInt S N8      = Int8
  GenericInt U N16     = Word16
  GenericInt S N16     = Int16
  GenericInt U N32     = Word32
  GenericInt S N32     = Int32
  GenericInt U N64     = Word64
  GenericInt S N64     = Int64
  GenericInt U NNative = WordN
  GenericInt S NNative = IntN

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
    IntType       :: ( BoundedInt (GenericInt s n)
                     , Size (GenericInt s n) ~ Range (GenericInt s n)
                     ) =>
                       Signedness s -> BitWidth n -> TypeRep (GenericInt s n)
    FloatType     :: TypeRep Float
    DoubleType    :: TypeRep Double
    ComplexType   :: RealFloat a => TypeRep a -> TypeRep (Complex a)
    ArrayType     :: TypeRep a -> TypeRep [a]
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
    ConsType      :: TypeRep a -> TypeRep (Tuple b) -> TypeRep (Tuple (a :* b))
    NilType       :: TypeRep (Tuple TNil)
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
    show (IVarType ta)                   = unwords ["IVar", show ta]
    show (FValType ta)                   = unwords ["FVal", show ta]

defaultSize :: TypeRep a -> Size a
defaultSize UnitType = universal
defaultSize BoolType = universal
defaultSize (IntType _ _) = universal
defaultSize FloatType = universal
defaultSize DoubleType = universal
defaultSize (ComplexType _) = universal
defaultSize (ArrayType t) = universal :> defaultSize t
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
defaultSize (IVarType ta) = defaultSize ta
defaultSize (FValType ta) = defaultSize ta

showTup :: [String] -> String
showTup as =  "(" ++ intercalate "," as ++ ")"

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
instance Type WordN   where typeRep = IntType U NNative; sizeOf = singletonRange
instance Type IntN    where typeRep = IntType S NNative; sizeOf = singletonRange
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

instance (Type a, Type b) => Type (a,b)
  where
    typeRep = Tup2Type typeRep typeRep

    sizeOf (a,b) =
        ( sizeOf a
        , sizeOf b
        )

instance (Type a, Type b, Type c) => Type (a,b,c)
  where
    typeRep = Tup3Type typeRep typeRep typeRep

    sizeOf (a,b,c) =
        ( sizeOf a
        , sizeOf b
        , sizeOf c
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

instance (Type a, Typeable b, Type (Tuple b)) => Type (Tuple (a :* b))
  where
    typeRep = ConsType typeRep typeRep
    sizeOf (x :* xs) = (sizeOf x, sizeOf xs)

instance Type (Tuple TNil)
  where
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
  Size WordN           = Range WordN
  Size IntN            = Range IntN
  Size Float           = AnySize
  Size Double          = AnySize
  Size (Complex a)     = AnySize
  Size [a]             = Range Length :> Size a
  Size (a,b)           = (Size a, Size b)
  Size (a,b,c)         = (Size a, Size b, Size c)
  Size (a,b,c,d)       = (Size a, Size b, Size c, Size d)
  Size (a,b,c,d,e)     = (Size a, Size b, Size c, Size d, Size e)
  Size (a,b,c,d,e,f)   = (Size a, Size b, Size c, Size d, Size e, Size f)
  Size (a,b,c,d,e,f,g) = (Size a, Size b, Size c, Size d, Size e, Size f, Size g)
  Size (a,b,c,d,e,f,g,h) = (Size a, Size b, Size c, Size d, Size e, Size f, Size g, Size h)
  Size (a,b,c,d,e,f,g,h,i) = (Size a, Size b, Size c, Size d, Size e, Size f, Size g, Size h, Size i)
  Size (a,b,c,d,e,f,g,h,i,j) = (Size a, Size b, Size c, Size d, Size e, Size f, Size g, Size h, Size i, Size j)
  Size (a,b,c,d,e,f,g,h,i,j,k) = (Size a, Size b, Size c, Size d, Size e, Size f, Size g, Size h, Size i, Size j, Size k)
  Size (a,b,c,d,e,f,g,h,i,j,k,l) = (Size a, Size b, Size c, Size d, Size e, Size f, Size g, Size h, Size i, Size j, Size k, Size l)
  Size (a,b,c,d,e,f,g,h,i,j,k,l,m) = (Size a, Size b, Size c, Size d, Size e, Size f, Size g, Size h, Size i, Size j, Size k, Size l, Size m)
  Size (a,b,c,d,e,f,g,h,i,j,k,l,m,n) = (Size a, Size b, Size c, Size d, Size e, Size f, Size g, Size h, Size i, Size j, Size k, Size l, Size m, Size n)
  Size (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) = (Size a, Size b, Size c, Size d, Size e, Size f, Size g, Size h, Size i, Size j, Size k, Size l, Size m, Size n, Size o)
  Size (a -> b)        = (Size a, Size b)
  Size (Mut a)         = Size a
  Size (IORef a)       = Size a
  Size (MArr a)        = Range Length :> Size a
  Size (Par a)         = Size a
  Size (Elements a)    = Range Length :> Size a
  Size (Tuple (a :* b)) = (Size a, Size (Tuple b))
  Size (Tuple TNil)   = Size ()
  Size (IV a)          = Size a
  Size (FVal a)        = Size a


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
