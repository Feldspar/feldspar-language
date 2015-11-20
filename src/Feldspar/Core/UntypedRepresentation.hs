{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Feldspar.Core.UntypedRepresentation (
    VarId (..)
  , Term(..)
  , UntypedFeld
  , UntypedFeldF(..)
  , Op(..)
  , Type(..)
  , Lit(..)
  , Var(..)
  , Size(..)
  , Signedness(..)
  , Fork(..)
  , HasType(..)
  , fv
  , allVars
  , collectLetBinders
  , collectBinders
  , mkLets
  , mkLam
  , mkApp
  , subst
  , stringTree
  )
  where

import Data.List (nub, intercalate)
import Data.Tree

import Language.Syntactic.Constructs.Binding (VarId (..))

import Feldspar.Range (Range(..), singletonRange)
import Feldspar.Core.Types (Length)

-- This file contains the UntypedFeld format and associated
-- helper-formats and -functions that work on those formats, for
-- example fv and typeof.
--
-- The format resembles the structure of the typed Syntactic format,
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

data Fork = None | Future | Par | Loop
    deriving (Eq,Show)

data Type =
     BoolType
   | BitType
   | IntType Signedness Size
   | FloatType
   | DoubleType
   | ComplexType Type
   | TupType [Type]
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

data Var = Var { varNum :: VarId
               , varType :: Type
               }

-- Variables are equal if they have the same varNum.
instance Eq Var where
  v1 == v2 = varNum v1 == varNum v2

instance Show Var where
  show (Var n _t) = "v" ++ show n

data Lit =
     LBool Bool
   | LInt Signedness Size Integer
   | LFloat Float
   | LDouble Double
   | LComplex Lit Lit
   | LArray Type [Lit] -- Type necessary for empty array literals.
   | LTup [Lit]
   deriving (Eq)

-- | Human readable show instance.
instance Show Lit where
   show (LBool b)                    = show b
   show (LInt _ _ i)                 = show i
   show (LFloat f)                   = show f
   show (LDouble d)                  = show d
   show (LComplex r c)               = "(" ++ show r ++ ", " ++ show c ++ "i)"
   show (LArray _ ls)                = "[" ++ sls ++ "]"
     where sls = intercalate "," $ map show ls
   show (LTup ls)                    = "(" ++ intercalate ", " (map show ls) ++ ")"

-- | Application heads.
data Op =
   -- Array
     GetLength
   | Parallel
   | Append
   | GetIx
   | SetLength
   | Sequential
   | SetIx
   -- Binding
   | Let
   -- Bits
   | Bit
   | Complement
   | ReverseBits
   | BitScan
   | BitCount
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
   | RealPart
   | ImagPart
   | Conjugate
   | Magnitude
   | Phase
   | Cis
   | MkComplex
   | MkPolar
   -- Condition
   | Condition
   | ConditionM
   -- Conversion
   | F2I
   | I2N
   | B2I
   | Round
   | Ceiling
   | Floor
   -- Elements
   | ESkip
   | EMaterialize
   | EWrite
   | EPar
   | EparFor
   -- Eq
   | Equal
   | NotEqual
   -- Error
   | Undefined
   | Assert String
   -- FFI
   | ForeignImport String
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
   | Pow
   | LogBase
   -- Floating
   | Pi
   -- Fractional
   | DivFrac
   -- Future
   | MkFuture
   | Await
   -- Integral
   | Quot
   | Rem
   | Div
   | Mod
   | IExp
   -- Logic
   | Not
   -- Logic
   | And
   | Or
   -- Loop
   | ForLoop
   | WhileLoop
   -- LoopM
   | While
   | For
   -- Mutable
   | Run
   | Return
   | Bind
   | Then
   | When
   -- MutableArray
   | NewArr_
   | ArrLength
   | NewArr
   | GetArr
   | SetArr
   -- MutableToPure
   | RunMutableArray
   | WithArray
   -- MutableReference
   | NewRef
   | GetRef
   | SetRef
   | ModRef
   -- Noinline
   | NoInline
   -- Num
   | Abs
   | Sign
   | Add
   | Sub
   | Mul
   -- Par
   | ParRun
   | ParGet
   | ParFork
   | ParNew
   | ParYield
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
   -- Save is an artificial node for the frontend, but we're beyond that now.
   -- SizeProp
   | PropSize
   -- SourceInfo
   | SourceInfo String
   -- Switch
   | Switch
   -- Tuples
   | Tup
   | Sel1
   | Sel2
   | Sel3
   | Sel4
   | Sel5
   | Sel6
   | Sel7
   | Sel8
   | Sel9
   | Sel10
   | Sel11
   | Sel12
   | Sel13
   | Sel14
   | Sel15
   -- Common nodes
   | Call Fork String
   deriving (Eq, Show)

-- | The main type: Variables, Bindings, Literals and Applications.
data UntypedFeldF e =
   -- Binding
     Variable Var
   | Lambda Var e
   | LetFun (String, Fork, e) e -- Note [Function bindings]
   -- Literal
   | Literal Lit
   -- Common nodes
   | App Op Type [e]
   deriving (Eq)

{-

Function bindings
-----------------

The LetFun constructor is different from the ordinary let-bindings,
and therefore has its own node type. In an ordinary language the
constructor would be called LetRec, but we do not have any
recursion. Functions are created by the createTasks pass, and they can
be run sequentially or concurrently depending on the "Fork".

-}

instance (Show e) => Show (UntypedFeldF e) where
   show (Variable v)                = show v
   show (Lambda v e)                = "(\\" ++ show v ++ " -> " ++ show e ++ ")"
   show (LetFun (s, k, e1) e2)      = "letFun " ++ show k ++ " " ++ s ++" = "++ show e1 ++ " in " ++ show e2
   show (Literal l) = show l
   show (App p@RunMutableArray _ [e]) = show p ++ " (" ++ show e ++ ")"
   show (App GetIx _ [e1,e2])       = "(" ++ show e1 ++ " ! " ++ show e2 ++ ")"
   show (App Add _ [e1,e2])         = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
   show (App Sub _ [e1,e2])         = "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
   show (App Mul _ [e1,e2])         = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
   show (App Div _ [e1,e2])         = "(" ++ show e1 ++ " / " ++ show e2 ++ ")"
   show (App p@Then _ [e1, e2])     = show p ++ " (" ++ show e1 ++ ") (" ++
                                      show e2 ++ ")"
   show (App p _ [e1, e2])
    | p `elem` [Bind, Let, EPar]    = show p ++ " (" ++ show e1 ++ ") " ++ show e2
   show (App (ForeignImport s) _ es)= s ++ " " ++ unwords (map show es)
   show (App Tup _ es)              = "("   ++ intercalate ", " (map show es) ++ ")"
   show (App p@Parallel _ [e1,e2]) = show p ++ " (" ++ show e1 ++ ") " ++ show e2
   show (App p@Sequential _ [e1,e2,e3]) = show p ++ " (" ++ show e1 ++ ") (" ++ show e2 ++ ") " ++ show e3
   show (App p _ es)                = show p ++ " " ++ unwords (map show es)

-- | Convert an untyped syntax tree into a @Tree@ of @String@s
stringTree :: UntypedFeld -> Tree String
stringTree = unfoldTree go
  where
    go (In (Variable v))         = (show v, [])
    go (In (Lambda v e))         = ("Lambda "++show v, [e])
    go (In (LetFun (s,k,e1) e2)) = (unwords ["LetFun", show k, s], [e1,e2])
    go (In (Literal l))          = (show l, [])
    go (In (App p _ es))         = (show p,es)

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
    typeof (LTup ls)     = TupType (map typeof ls)

instance HasType UntypedFeld where
    type TypeOf UntypedFeld                = Type
   -- Binding
    typeof (In (Variable v))               = typeof v
    typeof (In (Lambda v e))               = FunType (typeof v) (typeof e)
    typeof (In (LetFun _ e))               = typeof e
   -- Literal
    typeof (In (Literal l))                = typeof l
    typeof (In (App _ t _))                = t

fv :: UntypedFeld -> [Var]
fv = nub . fvU' []

fvU' :: [Var] -> UntypedFeld -> [Var]
   -- Binding
fvU' vs (In (Variable v)) | v `elem` vs  = []
                          | otherwise    = [v]
fvU' vs (In (Lambda v e))                = fvU' (v:vs) e
fvU' vs (In (LetFun (_, _, e1) e2))      = fvU' vs e1 ++ fvU' vs e2
   -- Literal
fvU' _  (In (Literal{}))                 = []
-- Common nodes.
fvU' vs (In (App _ _ es))                = concatMap (fvU' vs) es

-- | List all variables (free, bound and introduced by lambdas) in an expression
allVars :: UntypedFeld -> [Var]
allVars = nub . go
  where
    go (In (Variable v))           = [v]
    go (In (Lambda v e))           = v : go e
    go (In (LetFun (_, _, e1) e2)) = go e1 ++ go e2
    go (In (Literal{}))            = []
    go (In (App _ _ es))           = concatMap go es

-- | Collect nested let binders into the binders and the body.
collectLetBinders :: UntypedFeld -> ([(Var, UntypedFeld)], UntypedFeld)
collectLetBinders = go []
  where go acc (In (App Let _ [e, In (Lambda v b)])) = go ((v, e):acc) b
        go acc e                                     = (reverse acc, e)

-- | Collect binders from nested lambda expressions.
collectBinders :: UntypedFeld -> ([Var], UntypedFeld)
collectBinders = go []
  where go acc (In (Lambda v e)) = go (v:acc) e
        go acc e                 = (reverse acc, e)

-- | Inverse of collectLetBinders, put the term back together.
mkLets :: ([(Var, UntypedFeld)], UntypedFeld) -> UntypedFeld
mkLets ([], body)       = body
mkLets ((v, e):t, body) = In (App Let t' [e, body'])
  where body' = In (Lambda v (mkLets (t, body)))
        t'    = typeof body'

-- | Inverse of collectBinders, make a lambda abstraction.
mkLam :: [Var] -> UntypedFeld -> UntypedFeld
mkLam []    e = e
mkLam (h:t) e = In (Lambda h (mkLam t e))

-- | Make an application.
mkApp :: Type -> Op -> [UntypedFeld] -> UntypedFeld
mkApp t p es = In (App p t es)

-- | Substitute new for dst in e. Assumes no shadowing.
subst :: UntypedFeld -> Var -> UntypedFeld -> UntypedFeld
subst new dst = go
  where go v@(In (Variable v')) | dst == v' = new -- Replace.
                                | otherwise = v -- Stop.
        go l@(In (Lambda v e')) | v == dst  = l -- Stop.
                                | otherwise = In (Lambda v (go e'))
        go (In (LetFun (s, k, e1) e2)) = In (LetFun (s, k, go e1) (go e2)) -- Recurse.
        go l@(In Literal{})  = l -- Stop.
        go (In (App p t es)) = In (App p t (map go es)) -- Recurse.
