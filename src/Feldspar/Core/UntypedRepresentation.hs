{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Feldspar.Core.UntypedRepresentation (
    Term(..)
  , UntypedFeld(..)
  , UntypedFeldF(..)
  , PrimOp0(..)
  , PrimOp1(..)
  , PrimOp2(..)
  , PrimOp3(..)
  , Type(..)
  , Lit(..)
  , Var(..)
  , Size(..)
  , Signedness(..)
  , HasType(..)
  , fv
  , collectLetBinders
  , mkLets
  )
  where

import Data.List (nub)

import Feldspar.Range (Range(..), singletonRange, fullRange)
import Feldspar.Core.Types (Length)

-- This file contains the UntypedFeld format and associated
-- helper-formats and -functions that work on those formats, for
-- example fv and typeof.
--
-- THe format resembles the structure of the typed Syntactic format,
-- but it does not reflect into the host language type system.

type UntypedFeld = Term UntypedFeldF

data Term f = In (f (Term f))

deriving instance (Eq (f (Term f))) => Eq (Term f)
instance (Show (f (Term f))) => Show (Term f) where
  show (In f) = show f

data Size = S8 | S16 | S32 | S40 | S64
    deriving (Eq,Show)

data Signedness = Signed | Unsigned
    deriving (Eq,Show)

data Type =
     UnitType
   | BoolType
   | BitType
   | IntType Signedness Size
   | FloatType
   | DoubleType
   | ComplexType Type
   | Tup2Type Type Type
   | Tup3Type Type Type Type
   | Tup4Type Type Type Type Type
   | Tup5Type Type Type Type Type Type
   | Tup6Type Type Type Type Type Type Type
   | Tup7Type Type Type Type Type Type Type Type
   | MutType Type
   | RefType Type
   | ArrayType (Range Length) Type
   | MArrType (Range Length) Type
   | ParType Type
   | ElementsType Type
   | IVarType Type
   | FunType Type Type
   | FValType Type
   deriving (Eq,Show)

data Var = Var { varNum :: Integer
               , varType :: Type
               }

-- Variables are equal if they have the same varNum.
instance Eq Var where
  v1 == v2 = varNum v1 == varNum v2

instance Show Var where
  show (Var n _t) = "v" ++ show n

data Lit =
     LUnit
   | LBool Bool
   | LInt Signedness Size Integer
   | LFloat Float
   | LDouble Double
   | LComplex Lit Lit
   | LArray Type [Lit] -- Type necessary for empty array literals.
   | LTup2 Lit Lit
   | LTup3 Lit Lit Lit
   | LTup4 Lit Lit Lit Lit
   | LTup5 Lit Lit Lit Lit Lit
   | LTup6 Lit Lit Lit Lit Lit Lit
   | LTup7 Lit Lit Lit Lit Lit Lit Lit
   deriving (Eq,Show)

-- 0-ary application heads.
data PrimOp0 =
   -- Elements
     ESkip
   -- Error
   | Undefined
   -- Floating
   | Pi
   -- Par
   | ParNew
   | ParYield
   deriving (Eq,Show)

-- 1-ary application heads.
data PrimOp1 =
   -- Array
     GetLength
   -- Bits
   | Bit
   | Complement
   | ReverseBits
   | BitScan
   | BitCount
   -- Complex
   | RealPart
   | ImagPart
   | Conjugate
   | Magnitude
   | Phase
   | Cis
   -- Conversion
   | F2I
   | I2N
   | B2I
   | Round
   | Ceiling
   | Floor
   -- Floating
   | Exp
   | Sqrt
   | Log
   | Sin
   | Tan
   | Cos
   | Asin
   | Atan
   | Acos
   | Sinh
   | Tanh
   | Cosh
   | Asinh
   | Atanh
   | Acosh
   -- Future
   | MkFuture
   | Await
   -- Logic
   | Not
   -- Mutable
   | Run
   | Return
   -- MutableArray
   | NewArr_
   | ArrLength
   -- MutableToPure
   | RunMutableArray
   -- MutableReference
   | NewRef
   | GetRef
   -- Noinline
   | NoInline
   -- Num
   | Abs
   | Sign
   -- Par
   | ParRun
   | ParGet
   | ParFork
   -- Save
   | Save
   -- SizeProp
   | PropSize
   -- SourceInfo
   | SourceInfo String
   -- Switch
   | Switch
   -- Tuples
   | Sel1
   | Sel2
   | Sel3
   | Sel4
   | Sel5
   | Sel6
   | Sel7
   deriving (Eq, Show)

-- 2-ary application heads.
data PrimOp2 =
   -- Array
     Parallel
   | Append
   | GetIx
   | SetLength
   -- Bits
   | BAnd
   | BOr
   | BXor
   | SetBit
   | ClearBit
   | ComplementBit
   | TestBit
   | ShiftLU
   | ShiftRU
   | ShiftL
   | ShiftR
   | RotateLU
   | RotateRU
   | RotateL
   | RotateR
   -- Complex
   | MkComplex
   | MkPolar
   -- Elements
   | EMaterialize
   | EWrite
   | EPar
   | EparFor
   -- Eq
   | Equal
   | NotEqual
   -- Error
   | Assert String
   -- Floating
   | Pow
   | LogBase
   -- Fractional
   | DivFrac
   -- Integral
   | Quot
   | Rem
   | Div
   | Mod
   | IExp
   -- Logic
   | And
   | Or
   -- LoopM
   | While
   | For
   -- Mutable
   | Bind
   | Then
   | When
   -- MutableArray
   | NewArr
   | GetArr
   -- MutableToPure
   | WithArray
   -- MutableReference
   | SetRef
   | ModRef
   -- Num
   | Add
   | Sub
   | Mul
   -- Par
   | ParPut
   -- Ord
   | LTH
   | GTH
   | LTE
   | GTE
   | Min
   | Max
   -- RealFloat
   | Atan2
   deriving (Eq, Show)

-- 3-ary application heads.
data PrimOp3 =
   -- Array
     Sequential
   | SetIx
   -- Condition
   | Condition
   | ConditionM
   -- Loop
   | ForLoop
   | WhileLoop
   -- MutableArray
   | SetArr
   deriving (Eq, Show)

-- The main type: Applications, Bindings and other leftovers that are not 0-3-ary.
data UntypedFeldF e =
   -- Binding
     Variable Var
   | Lambda Var e
   | Let e e
   -- FFI
   | ForeignImport String Type [e] -- The type is the return type of the function.
   -- Literal
   | Literal Lit
   -- Tuple
   -- Keep all tuples in the same place, although Tup2/Tup3 could live in PrimOp2/3.
   | Tup2 e e
   | Tup3 e e e
   | Tup4 e e e e
   | Tup5 e e e e e
   | Tup6 e e e e e e
   | Tup7 e e e e e e e
   -- Common nodes
   | PrimApp0 PrimOp0 Type
   | PrimApp1 PrimOp1 Type e
   | PrimApp2 PrimOp2 Type e e
   | PrimApp3 PrimOp3 Type e e e
   deriving (Eq, Show)

class HasType a where
    type TypeOf a
    typeof :: a -> TypeOf a

instance HasType Var where
    type TypeOf Var = Type
    typeof Var{..}  = varType

instance HasType Lit where
    type TypeOf Lit      = Type
    typeof (LInt s n _)  = IntType s n
    typeof LDouble{}     = DoubleType
    typeof LFloat{}      = FloatType
    typeof LBool{}       = BoolType
    typeof (LArray t es) = ArrayType (singletonRange $ fromIntegral $ length es) t
    typeof (LComplex r _) = ComplexType $ typeof r
    typeof (LTup2 l1 l2) = Tup2Type (typeof l1) (typeof l2)
    typeof (LTup3 l1 l2 l3) = Tup3Type (typeof l1) (typeof l2) (typeof l3)
    typeof (LTup4 l1 l2 l3 l4) = Tup4Type (typeof l1) (typeof l2) (typeof l3)
                                          (typeof l4)
    typeof (LTup5 l1 l2 l3 l4 l5) = Tup5Type (typeof l1) (typeof l2) (typeof l3)
                                             (typeof l4) (typeof l5)
    typeof (LTup6 l1 l2 l3 l4 l5 l6) = Tup6Type (typeof l1) (typeof l2) (typeof l3)
                                                (typeof l4) (typeof l5) (typeof l6)
    typeof (LTup7 l1 l2 l3 l4 l5 l6 l7) = Tup7Type (typeof l1) (typeof l2) (typeof l3)
                                                   (typeof l4) (typeof l5) (typeof l6)
                                                   (typeof l7)

instance HasType UntypedFeld where
    type TypeOf UntypedFeld                = Type
   -- Binding
    typeof (In (Variable v))               = typeof v
    typeof (In (Lambda v e))               = FunType (typeof v) (typeof e)
    typeof (In (Let _ (In (Lambda _ e))))  = typeof e
   -- FFI
    typeof (In (ForeignImport _ t _))      = t
   -- Literal
    typeof (In (Literal l))                = typeof l
   -- Tuple
    typeof (In (Tup2 e1 e2))               = Tup2Type (typeof e1) (typeof e2)
    typeof (In (Tup3 e1 e2 e3))            = Tup3Type (typeof e1) (typeof e2)
                                                      (typeof e3)
    typeof (In (Tup4 e1 e2 e3 e4))         = Tup4Type (typeof e1) (typeof e2)
                                                      (typeof e3) (typeof e4)
    typeof (In (Tup5 e1 e2 e3 e4 e5))      = Tup5Type (typeof e1) (typeof e2)
                                                      (typeof e3) (typeof e4)
                                                      (typeof e5)
    typeof (In (Tup6 e1 e2 e3 e4 e5 e6))   = Tup6Type (typeof e1) (typeof e2)
                                                      (typeof e3) (typeof e4)
                                                      (typeof e5) (typeof e6)
    typeof (In (Tup7 e1 e2 e3 e4 e5 e6 e7)) = Tup7Type (typeof e1) (typeof e2)
                                                       (typeof e3) (typeof e4)
                                                       (typeof e5) (typeof e6)
                                                       (typeof e7)
    typeof (In (PrimApp0 _ t))              = t
    typeof (In (PrimApp1 _ t _))            = t
    typeof (In (PrimApp2 _ t _ _))          = t
    typeof (In (PrimApp3 _ t _ _ _))        = t
    typeof e = error ("UntypedRepresentation: Missing match of: " ++ show e)


fv :: UntypedFeld -> [Var]
fv = nub . fvU' []

fvU' :: [Var] -> UntypedFeld -> [Var]
   -- Binding
fvU' vs (In (Variable v)) | v `elem` vs  = []
                          | otherwise    = [v]
fvU' vs (In (Lambda v e))                = fvU' (v:vs) e
fvU' vs (In (Let e1 e2))                 = fvU' vs e1 ++ fvU' vs e2
   -- FFI
fvU' vs (In (ForeignImport _ _ es))      = concatMap (fvU' vs) es
   -- Literal
fvU' vs (In (Literal l))                 = []
   -- Tuple
fvU' vs (In (Tup2 e1 e2))                = fvU' vs e1 ++ fvU' vs e2
fvU' vs (In (Tup3 e1 e2 e3))             = fvU' vs e1 ++ fvU' vs e2 ++ fvU' vs e3
fvU' vs (In (Tup4 e1 e2 e3 e4))          = fvU' vs e1 ++ fvU' vs e2 ++ fvU' vs e3
                                           ++ fvU' vs e4
fvU' vs (In (Tup5 e1 e2 e3 e4 e5))       = fvU' vs e1 ++ fvU' vs e2 ++ fvU' vs e3
                                           ++ fvU' vs e4 ++ fvU' vs e5
fvU' vs (In (Tup6 e1 e2 e3 e4 e5 e6))    = fvU' vs e1 ++ fvU' vs e2 ++ fvU' vs e3
                                           ++ fvU' vs e4 ++ fvU' vs e5 ++ fvU' vs e6
fvU' vs (In (Tup7 e1 e2 e3 e4 e5 e6 e7)) = fvU' vs e1 ++ fvU' vs e2 ++ fvU' vs e3
                                           ++ fvU' vs e4 ++ fvU' vs e5 ++ fvU' vs e6
                                           ++ fvU' vs e7
-- Common nodes.
fvU' vs (In PrimApp0{})                  = []
fvU' vs (In (PrimApp1 _ _ e))            = fvU' vs e
fvU' vs (In (PrimApp2 _ _ e1 e2))        = fvU' vs e1 ++ fvU' vs e2
fvU' vs (In (PrimApp3 _ _ e1 e2 e3))     = fvU' vs e1 ++ fvU' vs e2 ++ fvU' vs e3

-- | Collect nested let binders into the binders and the body.
collectLetBinders :: UntypedFeld -> ([(Var, UntypedFeld)], UntypedFeld)
collectLetBinders e = go e []
  where go (In (Let e (In (Lambda v b)))) acc = go b ((v, e):acc)
        go e                              acc = (reverse acc, e)

-- | Inverse of collectLetBinders, put the term back together.
mkLets :: ([(Var, UntypedFeld)], UntypedFeld) -> UntypedFeld
mkLets ([], body)        = body
mkLets ((v, e):t, body) = In (Let e (In (Lambda v (mkLets (t, body)))))
