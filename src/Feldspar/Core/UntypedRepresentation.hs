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
  )
  where

import Data.List (nub)

import Feldspar.Range (Range(..), singletonRange, fullRange)
import Feldspar.Core.Types (Length)

type UntypedFeld = Term UntypedFeldF

data Term f = In (f (Term f))

deriving instance (Eq (f (Term f))) => Eq (Term f)
deriving instance (Show (f (Term f))) => Show (Term f)

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
   deriving (Show)

instance Eq Var where
  v1 == v2 = varNum v1 == varNum v2

data Lit =
     LUnit
   | LBool Bool
   | LInt Signedness Size Integer
   | LFloat Float
   | LDouble Double
   | LComplex Lit Lit
   | LArray Type [Lit]
   | LTup2 Lit Lit
   | LTup3 Lit Lit Lit
   | LTup4 Lit Lit Lit Lit
   | LTup5 Lit Lit Lit Lit Lit
   | LTup6 Lit Lit Lit Lit Lit Lit
   | LTup7 Lit Lit Lit Lit Lit Lit Lit
   deriving (Eq,Show)

data PrimOp0 =
   -- Error
     Undefined
   -- Floating
   | Pi
   deriving (Eq,Show)

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
   -- Eq
   | Equal
   | NotEqual
   -- Error
   | Assert String
   -- Floating
   | Pow
   | LogBase
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
   -- Ord
   | LTH
   | GTH
   | LTE
   | GTE
   | Min
   | Max
   -- RealFloat
   | Atan2
   -- Trace
   | Trace
   deriving (Eq, Show)

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

data UntypedFeldF e =
   -- Binding
     Variable Var
   | Lambda Var e
   | Let e e
   -- Elements
   | EMaterialize e e
   | EWrite e e
   | ESkip
   | EPar e e
   | EparFor e e
   -- FFI
   | ForeignImport String [e]
   -- Fractional
   | DivFrac e e
   -- Literal
   | Literal Lit
   -- Par
   | ParRun e
   | ParNew
   | ParGet e
   | ParPut e e
   | ParFork e
   | ParYield
   -- Tuple
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
    type TypeOf UntypedFeld          = Type
   -- Binding
    typeof (In (Variable v))              = typeof v
    typeof (In (Lambda v e))              = FunType (typeof v) (typeof e)
    typeof (In (Let _ (In (Lambda _ e)))) = typeof e
   -- Elements
    typeof (In (EMaterialize _ e))        = ElementsType (typeof e)
    typeof (In (EWrite _ e))              = ElementsType (typeof e)
    typeof (In ESkip)                     = ElementsType UnitType
    typeof (In (EPar e _))                = typeof e
    typeof (In (EparFor _ (In (Lambda _ e)))) = typeof e
   -- FFI
    typeof (In (ForeignImport _ e))       = error "typeof FFI"
   -- Fractional
    typeof (In (DivFrac e _))             = typeof e
   -- Literal
    typeof (In (Literal l))               = typeof l
   -- Par
    typeof (In (ParRun e))                = t
      where (ParType t) = typeof e
    typeof (In ParNew)                    = ParType (IVarType UnitType) -- XXX
    typeof (In (ParGet e))                = ParType t
      where (IVarType t) = typeof e
    typeof (In ParPut{})                  = ParType UnitType
    typeof (In ParFork{})                 = ParType UnitType
    typeof (In ParYield)                  = ParType UnitType
   -- Tuple
    typeof (In (Tup2 e1 e2))              = Tup2Type (typeof e1) (typeof e2)
    typeof (In (Tup3 e1 e2 e3))           = Tup3Type (typeof e1) (typeof e2)
                                                     (typeof e3)
    typeof (In (Tup4 e1 e2 e3 e4))        = Tup4Type (typeof e1) (typeof e2)
                                                     (typeof e3) (typeof e4)
    typeof (In (Tup5 e1 e2 e3 e4 e5))     = Tup5Type (typeof e1) (typeof e2)
                                                     (typeof e3) (typeof e4)
                                                     (typeof e5)
    typeof (In (Tup6 e1 e2 e3 e4 e5 e6))  = Tup6Type (typeof e1) (typeof e2)
                                                     (typeof e3) (typeof e4)
                                                     (typeof e5) (typeof e6)
    typeof (In (Tup7 e1 e2 e3 e4 e5 e6 e7)) = Tup7Type (typeof e1) (typeof e2)
                                                       (typeof e3) (typeof e4)
                                                       (typeof e5) (typeof e6)
                                                       (typeof e7)
    typeof (In (PrimApp0 _ t))             = t
    typeof (In (PrimApp1 _ t _))           = t
    typeof (In (PrimApp2 _ t _ _))         = t
    typeof (In (PrimApp3 _ t _ _ _))       = t
    typeof e = error ("UntypedRepresentation: Missing match of: " ++ show e)


fv :: UntypedFeld -> [Var]
fv = nub . fvU' []

fvU' :: [Var] -> UntypedFeld -> [Var]
   -- Binding
fvU' vs (In (Variable v)) | v `elem` vs = []
                          | otherwise = [v]
fvU' vs (In (Lambda v e))  = fvU' (v:vs) e
fvU' vs (In (Let e1 e2)) = fvU' vs e1 ++ fvU' vs e2
   -- Elements
fvU' vs (In (EMaterialize e1 e2)) = fvU' vs e1 ++ fvU' vs e2
fvU' vs (In (EWrite e1 e2)) = fvU' vs e1 ++ fvU' vs e2
fvU' vs (In ESkip) = []
fvU' vs (In (EPar e1 e2)) = fvU' vs e1 ++ fvU' vs e2
fvU' vs (In (EparFor e1 e2)) = fvU' vs e1 ++ fvU' vs e2
   -- FFI
fvU' vs (In (ForeignImport _ es)) = concatMap (fvU' vs) es
   -- Fractional
fvU' vs (In (DivFrac e1 e2)) = fvU' vs e1 ++ fvU' vs e2
   -- Literal
fvU' vs (In (Literal l)) = []
   -- Par
fvU' vs (In (ParRun e)) = fvU' vs e
fvU' vs (In (ParNew)) = []
fvU' vs (In (ParGet e)) = fvU' vs e
fvU' vs (In (ParPut e1 e2)) = fvU' vs e1 ++ fvU' vs e2
fvU' vs (In (ParFork e)) = fvU' vs e
fvU' vs (In (ParYield)) = []
   -- Tuple
fvU' vs (In (Tup2 e1 e2)) = fvU' vs e1 ++ fvU' vs e2
fvU' vs (In (Tup3 e1 e2 e3)) = fvU' vs e1 ++ fvU' vs e2 ++ fvU' vs e3
fvU' vs (In (Tup4 e1 e2 e3 e4)) = fvU' vs e1 ++ fvU' vs e2 ++ fvU' vs e3 ++ fvU' vs e4
fvU' vs (In (Tup5 e1 e2 e3 e4 e5)) = fvU' vs e1 ++ fvU' vs e2 ++ fvU' vs e3 ++ fvU' vs e4 ++ fvU' vs e5
fvU' vs (In (Tup6 e1 e2 e3 e4 e5 e6)) = fvU' vs e1 ++ fvU' vs e2 ++ fvU' vs e3 ++ fvU' vs e4 ++ fvU' vs e5 ++ fvU' vs e6
fvU' vs (In (Tup7 e1 e2 e3 e4 e5 e6 e7)) = fvU' vs e1 ++ fvU' vs e2 ++ fvU' vs e3 ++ fvU' vs e4 ++ fvU' vs e5 ++ fvU' vs e6 ++ fvU' vs e7
-- Common nodes.
fvU' vs (In PrimApp0{})              = []
fvU' vs (In (PrimApp1 _ _ e))        = fvU' vs e
fvU' vs (In (PrimApp2 _ _ e1 e2))    = fvU' vs e1 ++ fvU' vs e2
fvU' vs (In (PrimApp3 _ _ e1 e2 e3)) = fvU' vs e1 ++ fvU' vs e2 ++ fvU' vs e3
