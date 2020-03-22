{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

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

module Feldspar.Core.Representation
  ( Var(..)
  , VarId(..)
  , TypeF(..)
  , AExpr(..)
  , Info(..)
  , Expr(..)
  , exprType
  , toAExpr
  , exprSize
  , (:->)
  , EqBox(..)
  , Op(..)
  , fvi
  , fviR
  , CBind(..)
  , bvId
  , fviB
  , showRhs
  , BindEnv(..)
  , lookupBE
  , extendBE
  , mkLets
  , sharable
  , legalToShare
  , goodToShare
  ) where

import Feldspar.Core.Interpretation (VarId (..))
import Feldspar.Core.Types (Type(typeRep,sizeOf), TypeF(..), TypeRep(..), Length, Index, IntN,
                            Size(..), Elements, FVal, Mut, AnySize, MArr, Par, IV, Tuple(..), RTuple, (:*), TNil)
import Feldspar.Range
import Feldspar.Lattice (Lattice(..))

import qualified Data.ByteString.Char8 as B
import Data.Typeable (Typeable, (:~:)(Refl), eqT)
import Data.Hash (Hashable)
import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Data.Bits (Bits)
import Data.Complex (Complex)
import Data.IORef (IORef)

import Feldspar.Core.Tuple

infixr :->
infixl 5 :@
infix 1 :&

data Var a where
  Var :: Typeable a => { varNum :: VarId, varName :: B.ByteString} -> Var a

instance Eq (Var a) where
  v1 == v2 = varNum v1 == varNum v2

instance Show (Var a) where
  show (Var n _) = "v" ++ show n

-- | Full expressions always have types of the form 'Expr (Full t)' for some 't' to
--   distinguish them from partial applications of operators.
type Full a = a

-- We currently do not use the type constructor :->
type a :-> b = a -> b

-- | The type of information, for instance range information. Currently only size info.
data Info a where
  Info :: (Show (Size a), Lattice (Size a)) => {infoSize :: Size a} -> Info a

instance Eq (Info a) where
  Info x == Info y = x == y

instance Show (Info a) where
  show (Info x) = show x

-- | Adding default info to an Expr
toAExpr :: (Show (Size a), Lattice (Size a), TypeF a) => Expr (Full a) -> AExpr a
toAExpr e = Info top :& e

-- | Getting the size (value info) of an expression from its annotation
exprSize :: AExpr a -> Size a
exprSize = infoSize . aeInfo

-- | Annotated expression, that is, an expression together with extra information,
--   for instance from a program analysis.
data AExpr a where
  (:&) :: TypeF a => {aeInfo :: Info a, aeExpr :: Expr (Full a)} -> AExpr a

-- | Equality for AExps
instance Eq (AExpr a) where
  (:&) il el == (:&) ir er = il == ir && el == er

instance Show (AExpr a) where
  show e = showAExpr 0 e ""

showAExpr :: Int -> AExpr a -> String -> String
showAExpr n (i :& (e :: Expr (Full a))) r = '{':inf ++ "} " ++ showExpr n e r
  where inf = show (infoSize i) ++ " : " ++ show (typeRepF :: TypeRep a)

type LiteralType a = (Hashable a, Type a)

{- | The main expression type.
     Applications always have an operator at the left end, and are never annotated.

     Note that an operator itself is not a full expression unless its type is of
     the form 'Full t' for some 't'.
-}
data Expr a where
  Literal  :: LiteralType a          => a -> Expr (Full a)
  Operator ::                           Op a -> Expr a
  Variable ::                           Var a -> Expr (Full a)
  (:@)     :: TypeF a                => Expr (a -> b) -> AExpr a -> Expr b
  Lambda   :: TypeF a                => Var a -> AExpr b -> Expr (Full (a -> b))

exprType :: TypeF a => Expr a -> TypeRep a
exprType _ = typeRepF

instance Show (Expr a) where
  show e = showExpr 0 e ""

showExpr :: Int -> Expr a -> String -> String
showExpr _ (Literal l)    r = show l ++ r
showExpr _ (Operator op)  r = show op ++ r
showExpr _ (Variable v)   r = "v" ++ show (varNum v) ++ r
showExpr n (f :@ e)       r = showExpr n f
                            $ "\n" ++ replicate (n+2) ' ' ++
                              showAExpr (n+2) e r
showExpr n (Lambda v e) r = "\\ " ++ show v ++ " ->\n" ++ replicate (n+2) ' ' ++ showAExpr (n+2) e r

instance Typeable a => Eq (Expr a) where
  Literal l == Literal r = l == r
  Operator op1 == Operator op2 = op1 == op2
  Variable v1 == Variable v2 = v1 == v2
  ((f1 :: Expr (a1 -> b1)) :@ e1) == ((f2 :: Expr (a2 -> b2)) :@ e2)
        = case eqT :: Maybe ((a1,b1) :~: (a2,b2)) of
            Nothing -> False
            Just Refl -> f1 == f2 && e1 == e2
  Lambda v1 e1 == Lambda v2 e2 = v1 == v2 && e1 == e2
  _ == _ = False

-- | A box which makes its contents equal to everything else with the same type
newtype EqBox a = EqBox {unEqBox :: a}

instance Eq (EqBox a) where
  x == y = True

instance Show (EqBox a) where
  show x = "Box"

type SelCtx w uw sw usw = (Tuply w, Unpack w ~ uw, Size w ~ sw, Tuply sw, Unpack sw ~ usw)

-- | The main data type for built-in operators as well as let and conditional
--   constructs.
data Op a where
    -- | Array
    Parallel   :: Type a => Op (Length :-> (Index -> a) :-> Full [a])
    Sequential :: (Type a, Type st) =>
                  Op (Length :-> st :-> (Index -> st -> (a,st)) :-> Full [a])
    Append     :: Type a => Op ([a] :-> [a] :-> Full [a])
    GetIx      :: Type a => Op ([a] :-> Index :-> Full a)
    SetIx      :: Type a => Op ([a] :-> Index :-> a :-> Full [a])
    GetLength  :: Type a => Op ([a] :-> Full Length)
    SetLength  :: Type a => Op (Length :-> [a] :-> Full [a])

    -- | Binding
    Let :: Op (a :-> (a -> b) :-> Full b)

    -- | Bits
    BAnd          :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => Op (a :-> a :-> Full a)
    BOr           :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => Op (a :-> a :-> Full a)
    BXor          :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => Op (a :-> a :-> Full a)
    Complement    :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => Op (a :->       Full a)

    Bit           :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => Op (Index :->       Full a)
    SetBit        :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => Op (a :-> Index :-> Full a)
    ClearBit      :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => Op (a :-> Index :-> Full a)
    ComplementBit :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => Op (a :-> Index :-> Full a)
    TestBit       :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => Op (a :-> Index :-> Full Bool)

    ShiftLU       :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => Op (a :-> Index :-> Full a)
    ShiftRU       :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => Op (a :-> Index :-> Full a)
    ShiftL        :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => Op (a :-> IntN  :-> Full a)
    ShiftR        :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => Op (a :-> IntN  :-> Full a)
    RotateLU      :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => Op (a :-> Index :-> Full a)
    RotateRU      :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => Op (a :-> Index :-> Full a)
    RotateL       :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => Op (a :-> IntN  :-> Full a)
    RotateR       :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => Op (a :-> IntN  :-> Full a)
    ReverseBits   :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => Op (a :->           Full a)

    BitScan       :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => Op (a :-> Full Index)
    BitCount      :: (Type a, Bits a, BoundedInt a, Size a ~ Range a) => Op (a :-> Full Index)

    -- | Complex
    MkComplex :: (Type a, RealFloat a) => Op (a :-> a :-> Full (Complex a))
    RealPart  :: (Type a, RealFloat a) => Op (Complex a :-> Full a)
    ImagPart  :: (Type a, RealFloat a) => Op (Complex a :-> Full a)
    Conjugate :: (Type a, RealFloat a) => Op (Complex a :-> Full (Complex a))
    MkPolar   :: (Type a, RealFloat a) => Op (a :-> a :-> Full (Complex a))
    Magnitude :: (Type a, RealFloat a) => Op (Complex a :-> Full a)
    Phase     :: (Type a, RealFloat a) => Op (Complex a :-> Full a)
    Cis       :: (Type a, RealFloat a) => Op (a :-> Full (Complex a))

    -- | Condition
    Condition  ::                      Op (Bool :-> a :-> a :-> Full a)

    -- | Conversion
    F2I     :: (Type a, Integral a, RealFloat b)                     => Op (b :-> Full a)
    I2N     :: (Type a, Type b, Integral a, Num b, Size a ~ Range a) => Op (a :-> Full b)
    B2I     :: (Type a, Integral a)                                  => Op (Bool  :-> Full a)
    Round   :: (Type a, Integral a, RealFloat b)                     => Op (b :-> Full a)
    Ceiling :: (Type a, Integral a, RealFloat b)                     => Op (b :-> Full a)
    Floor   :: (Type a, Integral a, RealFloat b)                     => Op (b :-> Full a)

    -- | Elements
    EMaterialize :: Type a => Op (Length :-> Elements a :-> Full [a])
    EWrite       :: Type a => Op (Index :-> a :-> Full (Elements a))
    ESkip        :: Type a => Op (Full (Elements a))
    EPar         :: Type a => Op (Elements a :-> Elements a :-> Full (Elements a))
    EparFor      :: Type a => Op (Length :-> (Index -> Elements a) :-> Full (Elements a))

    -- | Eq
    Equal    :: (Type a, Eq a) => Op (a :-> a :-> Full Bool)
    NotEqual :: (Type a, Eq a) => Op (a :-> a :-> Full Bool)

    -- | Error
    Undefined :: Type a => Op (Full a)
    Assert    :: Type a => String -> Op (Bool :-> a :-> Full a)

    -- FFI
    -- ForeignImport :: (Type (DenResult a))
    --              => String -> Denotation a -> Op a

    -- | Floating
    Pi      :: (Type a, Floating a) => Op (Full a)
    Exp     :: (Type a, Floating a) => Op (a :-> Full a)
    Sqrt    :: (Type a, Floating a) => Op (a :-> Full a)
    Log     :: (Type a, Floating a) => Op (a :-> Full a)
    Pow     :: (Type a, Floating a) => Op (a :-> a :-> Full a)
    LogBase :: (Type a, Floating a) => Op (a :-> a :-> Full a)
    Sin     :: (Type a, Floating a) => Op (a :-> Full a)
    Tan     :: (Type a, Floating a) => Op (a :-> Full a)
    Cos     :: (Type a, Floating a) => Op (a :-> Full a)
    Asin    :: (Type a, Floating a) => Op (a :-> Full a)
    Atan    :: (Type a, Floating a) => Op (a :-> Full a)
    Acos    :: (Type a, Floating a) => Op (a :-> Full a)
    Sinh    :: (Type a, Floating a) => Op (a :-> Full a)
    Tanh    :: (Type a, Floating a) => Op (a :-> Full a)
    Cosh    :: (Type a, Floating a) => Op (a :-> Full a)
    Asinh   :: (Type a, Floating a) => Op (a :-> Full a)
    Atanh   :: (Type a, Floating a) => Op (a :-> Full a)
    Acosh   :: (Type a, Floating a) => Op (a :-> Full a)

    -- | Fractional
    DivFrac :: (Type a, Fractional a) => Op (a :-> a :-> Full a)

    -- | Future
    MkFuture :: Type a => Op (a :-> Full (FVal a))
    Await    :: Type a => Op (FVal a :-> Full a)

    -- | Integral
    Quot :: (Type a, BoundedInt a, Size a ~ Range a) => Op (a :-> a :-> Full a)
    Rem  :: (Type a, BoundedInt a, Size a ~ Range a) => Op (a :-> a :-> Full a)
    Div  :: (Type a, BoundedInt a, Size a ~ Range a) => Op (a :-> a :-> Full a)
    Mod  :: (Type a, BoundedInt a, Size a ~ Range a) => Op (a :-> a :-> Full a)
    IExp :: (Type a, BoundedInt a, Size a ~ Range a) => Op (a :-> a :-> Full a)

    -- | Logic
    And :: Op (Bool :-> Bool :-> Full Bool)
    Or  :: Op (Bool :-> Bool :-> Full Bool)
    Not :: Op (Bool :->          Full Bool)

    -- | Loop
    ForLoop   :: Type a => Op (Length :-> a :-> (Index -> a -> a) :-> Full a)
    WhileLoop :: Type a => Op (a :-> (a -> Bool) :-> (a -> a) :-> Full a)

    -- | Mutable
    Run :: Type a => Op (Mut a :-> Full a)

    -- | MutableArray
    NewArr    :: Type a => Op (Length :-> a :-> Full (Mut (MArr a)))
    NewArr_   :: Type a => Op (Length :-> Full (Mut (MArr a)))
    GetArr    :: Type a => Op (MArr a :-> Index :-> Full (Mut a))
    SetArr    :: Op (MArr a :-> Index :-> a :-> Full (Mut ()))
    ArrLength :: Op (MArr a :-> Full (Mut Length))

    -- | MutableToPure
    RunMutableArray :: Type a => Op (Mut (MArr a) :-> Full [a])
    WithArray       :: (Type a, Type b) => Op (MArr a :-> ([a] -> Mut b) :-> Full (Mut b))

    -- | MutableReference
    NewRef :: Type a => Op (a :-> Full (Mut (IORef a)))
    GetRef :: Type a => Op (IORef a :-> Full (Mut a))
    SetRef :: Type a => Op (IORef a :-> a :-> Full (Mut ()))
    ModRef :: Type a => Op (IORef a :-> (a -> a) :-> Full (Mut ()))

    -- | Nested tuples
    Cons  :: Type a => Op (a :-> RTuple b :-> Full (RTuple (a :* b)))
    Nil   ::           Op (Full (RTuple TNil))
    Car   :: Type a => Op (RTuple (a :* b) :-> Full a)
    Cdr   ::           Op (RTuple (a :* b) :-> Full (RTuple b))
    Tup   ::           Op (RTuple a :-> Full (Tuple a))
    UnTup ::           Op (Tuple a :-> Full (RTuple a))

    -- | NoInline
    NoInline :: Type a => Op (a :-> Full a)

    -- | Num
    Abs  :: (Type a, Num a, Num (Size a)) => Op (a :-> Full a)
    Sign :: (Type a, Num a, Num (Size a)) => Op (a :-> Full a)
    Add  :: (Type a, Num a, Num (Size a)) => Op (a :-> a :-> Full a)
    Sub  :: (Type a, Num a, Num (Size a)) => Op (a :-> a :-> Full a)
    Mul  :: (Type a, Num a, Num (Size a)) => Op (a :-> a :-> Full a)

    -- | Ord
    LTH :: (Type a, Ord a, Ord (Size a)) => Op (a :-> a :-> Full Bool)
    GTH :: (Type a, Ord a, Ord (Size a)) => Op (a :-> a :-> Full Bool)
    LTE :: (Type a, Ord a, Ord (Size a)) => Op (a :-> a :-> Full Bool)
    GTE :: (Type a, Ord a, Ord (Size a)) => Op (a :-> a :-> Full Bool)
    Min :: (Type a, Ord a, Ord (Size a)) => Op (a :-> a :-> Full a)
    Max :: (Type a, Ord a, Ord (Size a)) => Op (a :-> a :-> Full a)

    -- | Par
    ParRun    :: Type a => Op (Par a :-> Full a)
    ParNew    :: Type a => Op (Full (Par (IV a)))
    ParGet    :: Type a => Op (IV a :-> Full (Par a))
    ParPut    :: Type a => Op (IV a :-> a :-> Full (Par ()))
    ParFork   ::           Op (Par () :-> Full (Par ()))
    ParYield  ::           Op (Full (Par ()))

    -- | RealFloat
    Atan2   :: (Type a, RealFloat a) => Op (a :-> a :-> Full a)

    -- | Save
    Save :: Type a => Op (a :-> Full a)

    -- When are two size prop operators equal?
    -- | SizeProp
    PropSize :: (Type a, Type b) =>
        EqBox (Size a -> Size b) -> Op (a :-> b :-> Full b)

    -- | Switch
    Switch :: Type b => Op (b :-> Full b)

    -- Tuple
    Tup0  :: Op (Full ())
    Tup2  :: Op (a :-> b :-> Full (a,b))
    Tup3  :: Op (a :-> b :-> c :-> Full (a,b,c))
    Tup4  :: Op (a :-> b :-> c :-> d :-> Full (a,b,c,d))
    Tup5  :: Op (a :-> b :-> c :-> d :-> e :-> Full (a,b,c,d,e))
    Tup6  :: Op (a :-> b :-> c :-> d :-> e :-> f :-> Full (a,b,c,d,e,f))
    Tup7  :: Op (a :-> b :-> c :-> d :-> e :-> f :-> g :->
                 Full (a,b,c,d,e,f,g))
    Tup8  :: Op (a :-> b :-> c :-> d :-> e :-> f :-> g :-> h :->
                 Full (a,b,c,d,e,f,g,h))
    Tup9  :: Op (a :-> b :-> c :-> d :-> e :-> f :-> g :-> h :-> i :->
                 Full (a,b,c,d,e,f,g,h,i))
    Tup10 :: Op (a :-> b :-> c :-> d :-> e :-> f :-> g :-> h :-> i :-> j :->
                 Full (a,b,c,d,e,f,g,h,i,j))
    Tup11 :: Op (a :-> b :-> c :-> d :-> e :-> f :-> g :-> h :-> i :-> j :->
                 k :->
                 Full (a,b,c,d,e,f,g,h,i,j,k))
    Tup12 :: Op (a :-> b :-> c :-> d :-> e :-> f :-> g :-> h :-> i :-> j :->
                 k :-> l :->
                 Full (a,b,c,d,e,f,g,h,i,j,k,l))
    Tup13 :: Op (a :-> b :-> c :-> d :-> e :-> f :-> g :-> h :-> i :-> j :->
                 k :-> l :-> m :->
                 Full (a,b,c,d,e,f,g,h,i,j,k,l,m))
    Tup14 :: Op (a :-> b :-> c :-> d :-> e :-> f :-> g :-> h :-> i :-> j :->
                 k :-> l :-> m :-> n :->
                 Full (a,b,c,d,e,f,g,h,i,j,k,l,m,n))
    Tup15 :: Op (a :-> b :-> c :-> d :-> e :-> f :-> g :-> h :-> i :-> j :->
                 k :-> l :-> m :-> n :-> o :->
                 Full (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o))

    Sel1  :: (SelCtx w uw sw usw, Sel1C  uw, Sel1C  usw, Sel1T  usw ~ Size (Sel1T uw))  => Op (w :-> Full (Sel1T  uw))
    Sel2  :: (SelCtx w uw sw usw, Sel2C  uw, Sel2C  usw, Sel2T  usw ~ Size (Sel2T uw))  => Op (w :-> Full (Sel2T  uw))
    Sel3  :: (SelCtx w uw sw usw, Sel3C  uw, Sel3C  usw, Sel3T  usw ~ Size (Sel3T uw))  => Op (w :-> Full (Sel3T  uw))
    Sel4  :: (SelCtx w uw sw usw, Sel4C  uw, Sel4C  usw, Sel4T  usw ~ Size (Sel4T uw))  => Op (w :-> Full (Sel4T  uw))
    Sel5  :: (SelCtx w uw sw usw, Sel5C  uw, Sel5C  usw, Sel5T  usw ~ Size (Sel5T uw))  => Op (w :-> Full (Sel5T  uw))
    Sel6  :: (SelCtx w uw sw usw, Sel6C  uw, Sel6C  usw, Sel6T  usw ~ Size (Sel6T uw))  => Op (w :-> Full (Sel6T  uw))
    Sel7  :: (SelCtx w uw sw usw, Sel7C  uw, Sel7C  usw, Sel7T  usw ~ Size (Sel7T uw))  => Op (w :-> Full (Sel7T  uw))
    Sel8  :: (SelCtx w uw sw usw, Sel8C  uw, Sel8C  usw, Sel8T  usw ~ Size (Sel8T uw))  => Op (w :-> Full (Sel8T  uw))
    Sel9  :: (SelCtx w uw sw usw, Sel9C  uw, Sel9C  usw, Sel9T  usw ~ Size (Sel9T uw))  => Op (w :-> Full (Sel9T  uw))
    Sel10 :: (SelCtx w uw sw usw, Sel10C uw, Sel10C usw, Sel10T usw ~ Size (Sel10T uw)) => Op (w :-> Full (Sel10T uw))
    Sel11 :: (SelCtx w uw sw usw, Sel11C uw, Sel11C usw, Sel11T usw ~ Size (Sel11T uw)) => Op (w :-> Full (Sel11T uw))
    Sel12 :: (SelCtx w uw sw usw, Sel12C uw, Sel12C usw, Sel12T usw ~ Size (Sel12T uw)) => Op (w :-> Full (Sel12T uw))
    Sel13 :: (SelCtx w uw sw usw, Sel13C uw, Sel13C usw, Sel13T usw ~ Size (Sel13T uw)) => Op (w :-> Full (Sel13T uw))
    Sel14 :: (SelCtx w uw sw usw, Sel14C uw, Sel14C usw, Sel14T usw ~ Size (Sel14T uw)) => Op (w :-> Full (Sel14T uw))
    Sel15 :: (SelCtx w uw sw usw, Sel15C uw, Sel15C usw, Sel15T usw ~ Size (Sel15T uw)) => Op (w :-> Full (Sel15T uw))

    -- | ConditionM
    ConditionM :: (Monad m, Type a) => Op (Bool :-> m a :-> m a :-> Full (m a))

    -- | LoopM
    While :: (Monad m, Size (m ()) ~ AnySize) => Op (m Bool :-> m a :-> Full (m ()))
    For   :: (Monad m, Size (m ()) ~ AnySize) => Op (Length :-> (Index -> m a) :-> Full (m ()))

    -- | Mutable
    Return :: (Monad m, Size (m a) ~ Size a)         => Op (a    :-> Full (m a))
    Bind   :: (Monad m, Size (m a) ~ Size a, Type a) => Op (m a  :-> (a -> m b) :-> Full (m b))
    Then   :: Monad m                                => Op (m a  :-> m b        :-> Full (m b))
    When   :: Monad m                                => Op (Bool :-> m ()       :-> Full (m ()))

deriving instance Eq (Op a)
deriving instance Show (Op a)

isSelOp :: Op a -> Bool
isSelOp Sel1  = True
isSelOp Sel2  = True
isSelOp Sel3  = True
isSelOp Sel4  = True
isSelOp Sel5  = True
isSelOp Sel6  = True
isSelOp Sel7  = True
isSelOp Sel8  = True
isSelOp Sel9  = True
isSelOp Sel10 = True
isSelOp Sel11 = True
isSelOp Sel12 = True
isSelOp Sel13 = True
isSelOp Sel14 = True
isSelOp Sel15 = True
isSelOp _     = False

-- | Utility functions

fvi :: AExpr a -> S.Set VarId
fvi (_ :& e) = fviR e

fviR :: Expr a -> S.Set VarId
fviR (Variable v) = viSet v
fviR (f :@ e) = fviR f `S.union` fvi e
fviR (Lambda v e) = fvi e S.\\ viSet v
fviR _ = S.empty

viSet :: Var a -> S.Set VarId
viSet v = S.singleton $ varNum v


data CBind where
  CBind :: Var a -> AExpr a -> CBind

instance Eq CBind where
  CBind (v1@(Var n1 _) :: Var a) e1 == CBind (v2@(Var n2 _) :: Var b) e2
      = case eqT :: Maybe (a :~: b) of
          Nothing -> False
          Just Refl -> n1 == n2 && e1 == e2

instance Show CBind where
  show (CBind v e) = show v ++ " = " ++ show e

bvId :: CBind -> VarId
bvId (CBind v _) = varNum v

fviB :: CBind -> S.Set VarId
fviB (CBind _ e) = fvi e

showRhs (CBind _ e) = show e

mkLets :: ([CBind], AExpr a) -> AExpr a
mkLets (CBind v e1@(i :& _) : bs, e@(i2 :& _)) = i2 :& Operator Let :@ e1 :@ bs'
  where bs' = Info (infoSize i, infoSize i2) :& Lambda v (mkLets (bs,e))
mkLets ([], e) = e

-- | Functions for bind environments
type BindEnv = M.Map VarId CBind

lookupBE :: String -> BindEnv -> Var a -> AExpr a
lookupBE msg bm (v@(Var n _) :: Var a)
               = case M.lookup n bm of
                      Nothing -> error $ msg ++ ": lookupBE does not find variable " ++ show v
                      Just (CBind (u@Var{} :: Var b) e)
                           -> case eqT :: Maybe (a :~: b) of
                                   Nothing -> error $ msg ++ ": lookupBE finds conflicing types for " ++ show v
                                   Just Refl -> e

extendBE :: BindEnv -> CBind -> BindEnv
extendBE bm b = M.insert (bvId b) b bm

-- | Expressions that can and should be shared
sharable :: AExpr a -> Bool
sharable e = legalToShare e && goodToShare e

-- | Expressions that can be shared without breaking fromCore
legalToShare :: AExpr a -> Bool
legalToShare (_ :& Operator op) = shOp op
legalToShare (_ :& f :@ _)      = shApp f
legalToShare (_ :& Lambda _ _)  = False
legalToShare _                  = True

shApp :: Expr a -> Bool
shApp (f :@ _) = shApp f
shApp (Operator op) = shOp op

shOp :: Op a -> Bool
-- Elements
shOp ESkip     = False
shOp EWrite    = False
shOp EPar      = False
shOp EparFor   = False
-- Monads
shOp Return    = False
shOp Bind      = False
shOp Then      = False
shOp When      = False
-- Monadic arrays
shOp NewArr    = False
shOp NewArr_   = False
shOp GetArr    = False
shOp SetArr    = False
shOp ArrLength = False
-- Monadic loops
shOp For       = False
shOp While     = False
-- MonadRef
shOp NewRef    = False
shOp GetRef    = False
shOp SetRef    = False
shOp ModRef    = False
-- Nested tuples
shOp Cons      = False
shOp Nil       = False
shOp Cdr       = False
shOp UnTup     = False
-- Everything else
shOp _ = True

-- | Expressions that are expensive enough to be worth sharing
goodToShare :: AExpr a -> Bool
goodToShare (_ :& Literal (l :: a)) = largeLit (typeRep :: TypeRep a) l
-- The case below avoids constructing a let-binding for an array stored
-- in a tuple. This is beneficial because the select operator is order
-- of magnitudes cheaper than the array copy generated for the let-binding.
-- With a better compilation of array assignments, the need for this
-- special case goes away.
goodToShare (_ :& Operator op :@ e :: AExpr a)
  | isArrayT (typeRepF :: TypeRep a) && isSelOp op && not (goodToShare e)
  = False
goodToShare (_ :& _ :@ _) = True
goodToShare _                   = False

isArrayT :: TypeRep a -> Bool
isArrayT (ArrayType _) = True
isArrayT _             = False

largeLit :: TypeRep a -> a -> Bool
largeLit UnitType l = False
largeLit BoolType l = False
largeLit (IntType _ _) l = False
largeLit FloatType l = False
largeLit DoubleType l = False
largeLit (ArrayType t) l = not $ null l
largeLit (ElementsType t) l = False
largeLit _ _ = True
