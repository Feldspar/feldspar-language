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
   -- Floating
     Pi
   deriving (Eq,Show)

data PrimOp1 =
   -- Bits
     Bit
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
   -- Logic
   | Not
   -- Num
   | Abs
   | Sign
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
   -- Bits
     BAnd
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
   -- Floating
   | Pow
   | LogBase
   -- Logic
   | And
   | Or
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
   deriving (Eq, Show)

data UntypedFeldF e =
   -- Array
     Parallel e e
   | Sequential e e e
   | Append e e
   | GetIx e e
   | SetIx e e e
   | GetLength e
   | SetLength e e
   -- Binding
   | Variable Var
   | Lambda Var e
   | Let e e
   -- Condition
   | Condition e e e
   | ConditionM e e e
   -- Elements
   | EMaterialize e e
   | EWrite e e
   | ESkip
   | EPar e e
   | EparFor e e
   -- Eq
   | Equal e e
   | NotEqual e e
   -- Error
   | Undefined
   | Assert e e
   -- FFI
   | ForeignImport String [e]
   -- Fractional
   | DivFrac e e
   -- Future
   | MkFuture e
   | Await e
   -- Integral
   | Quot e e
   | Rem e e
   | Div e e
   | Mod e e
   | IExp e e
   -- Literal
   | Literal Lit
   -- Loop
   | ForLoop e e e
   | WhileLoop e e e
   -- LoopM
   | While e e
   | For e e
   -- Mutable
   | Run e
   | Return e
   | Bind e e
   | Then e e
   | When e e
   -- MutableReference
   | NewRef e
   | GetRef e
   | SetRef e e
   | ModRef e e
   -- MutableArray
   | NewArr e e
   | NewArr_ e
   | GetArr e e
   | SetArr e e e
   | ArrLength e
   -- MutableToPure
   | RunMutableArray e
   | WithArray e e
   -- Noinline
   | NoInline e
   -- Par
   | ParRun e
   | ParNew
   | ParGet e
   | ParPut e e
   | ParFork e
   | ParYield
   -- RealFloat
   | Atan2 e e
   -- Save
   | Save e
   -- SizeProp
   | PropSize e
   -- SourceInfo
   | SourceInfo String e
   -- Switch
   | Switch e
   -- Trace
   | Trace e e
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
   -- Array
    typeof (In (Parallel _ e))            = ArrayType fullRange (typeof e)
    typeof (In (Sequential _ _ e))        = ArrayType fullRange (typeof e)
    typeof (In (Append e _))              = typeof e
    typeof (In (GetIx e _))               = t
      where (ArrayType _ t) = typeof e
    typeof (In (SetIx e _ _))             = typeof e
    typeof (In GetLength{})               = IntType Unsigned S32
    typeof (In (SetLength _ e))           = typeof e
   -- Binding
    typeof (In (Variable v))              = typeof v
    typeof (In (Lambda v e))              = FunType (typeof v) (typeof e)
    typeof (In (Let _ (In (Lambda _ e)))) = typeof e
   -- Condition
    typeof (In (Condition _ e _))         = typeof e
    typeof (In (ConditionM _ e _))        = typeof e
   -- Elements
    typeof (In (EMaterialize _ e))        = ElementsType (typeof e)
    typeof (In (EWrite _ e))              = ElementsType (typeof e)
    typeof (In ESkip)                     = ElementsType UnitType
    typeof (In (EPar e _))                = typeof e
    typeof (In (EparFor _ (In (Lambda _ e)))) = typeof e
   -- Eq
    typeof (In Equal{})                   = BoolType
    typeof (In NotEqual{})                = BoolType
   -- Error
    typeof (In Undefined)                 = error "Typeof undefined"
    typeof (In (Assert _ e))              = typeof e
   -- FFI
    typeof (In (ForeignImport _ e))       = error "typeof FFI"
   -- Fractional
    typeof (In (DivFrac e _))             = typeof e
    -- Future
    typeof (In (MkFuture e))              = FValType (typeof e)
    typeof (In (Await e))                 = t
      where (FValType t) = typeof e
   -- Integral
    typeof (In (Quot e _))                = typeof e
    typeof (In (Rem e _))                 = typeof e
    typeof (In (Div e _))                 = typeof e
    typeof (In (Mod e _))                 = typeof e
    typeof (In (IExp e _))                = typeof e
   -- Literal
    typeof (In (Literal l))               = typeof l
   -- Loop
    typeof (In (ForLoop _ e _))           = typeof e
    typeof (In (WhileLoop e _ _))         = typeof e
   -- LoopM
    typeof (In (While e _))               = t
      where t | MutType _ <- typeof e = MutType UnitType
              | ParType _ <- typeof e = ParType UnitType
    typeof (In (For _ (In (Lambda _ e)))) = t
      where t | MutType _ <- typeof e = MutType UnitType
              | ParType _ <- typeof e = ParType UnitType
   -- Mutable
    typeof (In (Run e))                   = t
      where t | MutType a <- typeof e = a
    typeof (In (Return e))                = MutType (typeof e)
    typeof (In (Bind _ (In (Lambda _ e))))= typeof e
    typeof (In (Then _ e))                = typeof e
    typeof (In (When _ e))                = typeof e
   -- MutableReference
    typeof (In (NewRef e))                = MutType (RefType (typeof e))
    typeof (In (GetRef e))                = t
     where (RefType t) = typeof e
    typeof (In SetRef{})                  = MutType UnitType
    typeof (In ModRef{})                  = MutType UnitType
   -- MutableArray
    typeof (In (NewArr _ e))              = MutType (MArrType fullRange (typeof e))
    typeof (In (NewArr_ e))               = error "typeof: newArr_"
    typeof (In (GetArr e _))              = MutType t
     where (MArrType _ t) = typeof e
    typeof (In SetArr{})                  = MutType UnitType
    typeof (In ArrLength{})               = MutType (IntType Unsigned S32)
   -- MutableToPure
    typeof (In (RunMutableArray e))       = ArrayType rs a
     where (MutType (MArrType rs a)) = typeof e
    typeof (In (WithArray _ (In (Lambda _ e)))) = typeof e
   -- Noinline
    typeof (In (NoInline e))              = typeof e
   -- Par
    typeof (In (ParRun e))                = t
      where (ParType t) = typeof e
    typeof (In ParNew)                    = ParType (IVarType UnitType) -- XXX
    typeof (In (ParGet e))                = ParType t
      where (IVarType t) = typeof e
    typeof (In ParPut{})                  = ParType UnitType
    typeof (In ParFork{})                 = ParType UnitType
    typeof (In ParYield)                  = ParType UnitType
   -- RealFloat
    typeof (In (Atan2 e _))               = typeof e
   -- Save
    typeof (In (Save e))                  = typeof e
   -- SizeProp
    typeof (In (PropSize e))              = typeof e
   -- SourceInfo
    typeof (In (SourceInfo _ e))          = typeof e
   -- Switch
    typeof (In (Switch e))                = typeof e
   -- Trace
    typeof (In (Trace _ e))               = typeof e
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
    typeof e = error ("UntypedRepresentation: Missing match of: " ++ show e)


fv :: UntypedFeld -> [Var]
fv = nub . fvU' []

fvU' :: [Var] -> UntypedFeld -> [Var]
-- Array
fvU' vs (In (Parallel len ixf)) = fvU' vs len ++ fvU' vs ixf
fvU' vs (In (Sequential len init e)) = fvU' vs len ++ fvU' vs init ++ fvU' vs e
fvU' vs (In (Append e1 e2)) = fvU' vs e1 ++ fvU' vs e2
fvU' vs (In (GetIx e1 e2)) = fvU' vs e1 ++ fvU' vs e2
fvU' vs (In (SetIx e1 e2 e3)) = fvU' vs e1 ++ fvU' vs e2 ++ fvU' vs e3
fvU' vs (In (GetLength e)) = fvU' vs e
fvU' vs (In (SetLength e1 e2)) = fvU' vs e1 ++ fvU' vs e2
   -- Binding
fvU' vs (In (Variable v)) | v `elem` vs = []
                          | otherwise = [v]
fvU' vs (In (Lambda v e))  = fvU' (v:vs) e
fvU' vs (In (Let e1 e2)) = fvU' vs e1 ++ fvU' vs e2
   -- Condition
fvU' vs (In (Condition c t f)) = fvU' vs c ++ fvU' vs t ++ fvU' vs f
fvU' vs (In (ConditionM c t f)) = fvU' vs c ++ fvU' vs t ++ fvU' vs f
   -- Elements
fvU' vs (In (EMaterialize e1 e2)) = fvU' vs e1 ++ fvU' vs e2
fvU' vs (In (EWrite e1 e2)) = fvU' vs e1 ++ fvU' vs e2
fvU' vs (In ESkip) = []
fvU' vs (In (EPar e1 e2)) = fvU' vs e1 ++ fvU' vs e2
fvU' vs (In (EparFor e1 e2)) = fvU' vs e1 ++ fvU' vs e2
   -- Eq
fvU' vs (In (Equal e1 e2)) = fvU' vs e1 ++ fvU' vs e2
fvU' vs (In (NotEqual e1 e2)) = fvU' vs e1 ++ fvU' vs e2
   -- Error
fvU' vs (In Undefined) = []
fvU' vs (In (Assert e1 e2)) = fvU' vs e1 ++ fvU' vs e2
   -- FFI
fvU' vs (In (ForeignImport _ es)) = concatMap (fvU' vs) es
   -- Fractional
fvU' vs (In (DivFrac e1 e2)) = fvU' vs e1 ++ fvU' vs e2
   -- Future
fvU' vs (In (MkFuture e)) = fvU' vs e
fvU' vs (In (Await e)) = fvU' vs e
   -- Integral
fvU' vs (In (Quot e1 e2)) = fvU' vs e1 ++ fvU' vs e2
fvU' vs (In (Rem e1 e2)) = fvU' vs e1 ++ fvU' vs e2
fvU' vs (In (Div e1 e2)) = fvU' vs e1 ++ fvU' vs e2
fvU' vs (In (Mod e1 e2)) = fvU' vs e1 ++ fvU' vs e2
fvU' vs (In (IExp e1 e2)) = fvU' vs e1 ++ fvU' vs e2
   -- Literal
fvU' vs (In (Literal l)) = []
   -- Loop
fvU' vs (In (ForLoop e1 e2 e3)) = fvU' vs e1 ++ fvU' vs e2 ++ fvU' vs e3
fvU' vs (In (WhileLoop e1 e2 e3)) = fvU' vs e1 ++ fvU' vs e2 ++ fvU' vs e3
   -- LoopM
fvU' vs (In (While e1 e2)) = fvU' vs e1 ++ fvU' vs e2
fvU' vs (In (For e1 e2)) = fvU' vs e1 ++ fvU' vs e2
   -- Mutable
fvU' vs (In (Run e)) = fvU' vs e
fvU' vs (In (Return e)) = fvU' vs e
fvU' vs (In (Bind e1 e2)) = fvU' vs e1 ++ fvU' vs e2
fvU' vs (In (Then e1 e2)) = fvU' vs e1 ++ fvU' vs e2
fvU' vs (In (When e1 e2)) = fvU' vs e1 ++ fvU' vs e2
   -- MutableReference
fvU' vs (In (NewRef e)) = fvU' vs e
fvU' vs (In (GetRef e)) = fvU' vs e
fvU' vs (In (SetRef e1 e2)) = fvU' vs e1 ++ fvU' vs e2
fvU' vs (In (ModRef e1 e2)) = fvU' vs e1 ++ fvU' vs e2
   -- MutableArray
fvU' vs (In (NewArr e1 e2)) = fvU' vs e1 ++ fvU' vs e2
fvU' vs (In (NewArr_ e)) = fvU' vs e
fvU' vs (In (GetArr e1 e2)) = fvU' vs e1 ++ fvU' vs e2
fvU' vs (In (SetArr e1 e2 e3)) = fvU' vs e1 ++ fvU' vs e2 ++ fvU' vs e3
fvU' vs (In (ArrLength e)) = fvU' vs e
   -- MutableToPure
fvU' vs (In (RunMutableArray e)) = fvU' vs e
fvU' vs (In (WithArray e1 e2)) = fvU' vs e1 ++ fvU' vs e2
   -- Noinline
fvU' vs (In (NoInline e)) = fvU' vs e
   -- Par
fvU' vs (In (ParRun e)) = fvU' vs e
fvU' vs (In (ParNew)) = []
fvU' vs (In (ParGet e)) = fvU' vs e
fvU' vs (In (ParPut e1 e2)) = fvU' vs e1 ++ fvU' vs e2
fvU' vs (In (ParFork e)) = fvU' vs e
fvU' vs (In (ParYield)) = []
   -- RealFloat
fvU' vs (In (Atan2 e1 e2)) = fvU' vs e1 ++ fvU' vs e2
   -- Save
fvU' vs (In (Save e)) = fvU' vs e
   -- SizeProp
fvU' vs (In (PropSize e)) = fvU' vs e
   -- SourceInfo
fvU' vs (In (SourceInfo _ e)) = fvU' vs e
   -- Switch
fvU' vs (In (Switch e)) = fvU' vs e
   -- Trace
fvU' vs (In (Trace e1 e2)) = fvU' vs e1 ++ fvU' vs e2
   -- Tuple
fvU' vs (In (Tup2 e1 e2)) = fvU' vs e1 ++ fvU' vs e2
fvU' vs (In (Tup3 e1 e2 e3)) = fvU' vs e1 ++ fvU' vs e2 ++ fvU' vs e3
fvU' vs (In (Tup4 e1 e2 e3 e4)) = fvU' vs e1 ++ fvU' vs e2 ++ fvU' vs e3 ++ fvU' vs e4
fvU' vs (In (Tup5 e1 e2 e3 e4 e5)) = fvU' vs e1 ++ fvU' vs e2 ++ fvU' vs e3 ++ fvU' vs e4 ++ fvU' vs e5
fvU' vs (In (Tup6 e1 e2 e3 e4 e5 e6)) = fvU' vs e1 ++ fvU' vs e2 ++ fvU' vs e3 ++ fvU' vs e4 ++ fvU' vs e5 ++ fvU' vs e6
fvU' vs (In (Tup7 e1 e2 e3 e4 e5 e6 e7)) = fvU' vs e1 ++ fvU' vs e2 ++ fvU' vs e3 ++ fvU' vs e4 ++ fvU' vs e5 ++ fvU' vs e6 ++ fvU' vs e7
-- Common nodes.
fvU' vs (In PrimApp0{})           = []
fvU' vs (In (PrimApp1 _ _ e))     = fvU' vs e
fvU' vs (In (PrimApp2 _ _ e1 e2)) = fvU' vs e1 ++ fvU' vs e2
