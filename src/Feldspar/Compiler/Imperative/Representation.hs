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

{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

-- | Abstract syntax representation for imperative programs
module Feldspar.Compiler.Imperative.Representation (
    -- * Representation of imperative programs
    Module(..)
  , Entity(..)
  , Declaration(..)
  , Block(..)
  , Program(..)
  , Expression(..)
  , ActualParameter(..)
  , Function(..)
  , Variable(..)
  , StructMember(..)
  , Pattern(..)
  , ParType(..)
    -- * Types
  , ScalarType(..)
  , Type(..)
  , renderScalarType
  , renderType
  , extend
  , Constant(..)
  , module Feldspar.Core.UntypedRepresentation
  , fv
  )
  where

import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Semigroup (Semigroup(..))
import Language.Haskell.TH.Syntax (Lift(..))

import Feldspar.Range (Range)
import Feldspar.Compiler.Options (ErrorClass(..), handleError)
import Feldspar.Core.Types (Length)
import Feldspar.Core.UntypedRepresentation
        (Signedness(..), Size(..), HasType(..))

--------------------------------------------------------------------------------
-- * Representation of imperative programs
--------------------------------------------------------------------------------

data Module t = Module
    { entities                      :: [Entity t]
    }
    deriving (Eq, Show)

data Entity t
    = StructDef
        { structName                :: String
        , structMembers             :: [StructMember t]
        }
    | TypeDef
        { actualType                :: Type
        , typeName                  :: String
        }
    | Proc
        { procName                  :: String
        , loopBody                  :: Bool
        -- Is this a loopbody in disguise?
        , inParams                  :: [Variable t]
        , procType                  :: Type
        , procBody                  :: Maybe (Block t)
        }
    | ValueDef
        { valVar                    :: Variable t
        , valValue                  :: Constant t
        }
    deriving (Eq, Show)

data StructMember t = StructMember
    { structMemberName              :: String
    , structMemberType              :: Type
    }
    deriving (Eq, Show)

data Block t = Block
    { locals                        :: [Declaration t]
    , blockBody                     :: Program t
    }
    deriving (Eq, Show)

data Program t
    = Empty
        {
        }
    | Comment
        { isBlockComment            :: Bool
        , commentValue              :: String
        }
    | Assign
        { lhs                       :: Expression t
        , rhs                       :: Expression t
        }
    | ProcedureCall
        { procCallName              :: String
        , procCallParams            :: [ActualParameter t]
        }
    | Sequence
        { sequenceProgs             :: [Program t]
        }
    | Switch
        { scrutinee                 :: Expression t
        , alts                      :: [(Pattern t, Block t)]
        }
    | SeqLoop
        { sLoopCond                 :: Expression t
        , sLoopCondCalc             :: Block t
        , sLoopBlock                :: Block t
        }
    | ParLoop
        { pParallelType             :: ParType
        , pLoopCounter              :: Variable t
        , pLoopStart                :: Expression t
        , pLoopEnd                  :: Expression t
        , pLoopStep                 :: Expression t
        , pLoopBlock                :: Block t
        }
    | BlockProgram
        { blockProgram              :: Block t
        }
    deriving (Eq, Show)

data Pattern t
   = PatDefault
   | Pat (Expression t)
     deriving (Eq, Show)

data ParType
   = Sequential
   | Parallel
   | TaskParallel
   | WorkParallel -- Generated from ExternalProgram.
     deriving (Eq, Show)

data ActualParameter t
    = ValueParameter
        { valueParam                :: Expression t
        }
    | TypeParameter
        { typeParam                 :: Type
        }
    | FunParameter
        { funParamName              :: String
        }
    deriving (Eq, Show)

data Declaration t = Declaration
    { declVar                       :: Variable t
    , initVal                       :: Maybe (Expression t)
    }
    deriving (Eq, Show)

data Expression t
    = VarExpr
        { varExpr                   :: Variable t
        }
    | ArrayElem
        { array                     :: Expression t
        , arrayIndex                :: [Expression t]
        }
    | StructField
        { struct                    :: Expression t
        , fieldName                 :: String
        }
    | ConstExpr
        { constExpr                 :: Constant t
        }
    | FunctionCall
        { function                  :: Function
        , funCallParams             :: [Expression t]
        }
    | Cast
        { castType                  :: Type
        , castExpr                  :: Expression t
        }
    | AddrOf
        { addrExpr                  :: Expression t
        }
    | SizeOf
        { sizeOf                    :: Type
        }
    | Deref
        { ptrExpr                   :: Expression t
        }
    deriving (Eq, Show)

data Function
    = Function
        { funName                   :: String
        , returnType                :: Type
        }
    deriving (Eq, Show)

data Constant t
    = IntConst
        { intValue                  :: Integer
        , intType                   :: ScalarType
        }
    | DoubleConst
        { doubleValue               :: Double
        }
    | FloatConst
        { floatValue                :: Float
        }
    | BoolConst
        { boolValue                 :: Bool
        }
    | ComplexConst
        { realPartComplexValue      :: Constant t
        , imagPartComplexValue      :: Constant t
        }
    | StringConst
        { stringValue               :: String -- String value including quotes if required.
        }
    | ArrayConst
        { arrayValues               :: [Constant t]
        , arrayType                 :: Type
        }
    -- The maybe allows us to represent both x = { .field = 0 } and x = { 0 }.
    | StructConst
        { memberValues              :: [(Maybe String, Constant t)]
        , structType                :: Type
        }
    deriving (Eq, Lift, Show)

data Variable t
    = Variable
        { varType                   :: Type
        , varName                   :: String
        }
    deriving (Eq, Show)

instance Semigroup (Program t) where
  Empty         <> p              = p
  p             <> Empty          = p
  (Sequence pa) <> (Sequence pb)  = Sequence (pa <> pb)
  pa            <> pb             = Sequence [pa <> pb]

instance Semigroup (Block t) where
  (Block da pa) <> (Block db pb)  = Block (da <> db) (pa <> pb)

instance Monoid (Block t) where
  mempty                          = Block [] Empty
  mappend                         = (<>)

--------------------------------------------------------------------------------
-- * Types
--------------------------------------------------------------------------------

data ScalarType =
      BoolType
    | BitType
    | FloatType
    | DoubleType
    | NumType Signedness Size
    | ComplexType Type
    | Pointer Type -- Used for scatter/gather.
    deriving (Eq, Lift, Show)

data Type =
      VoidType
    | Length :# ScalarType -- Machine SIMD vectors; xmm registers in x86.
    | StringType
    | ArrayType (Range Length) Type
    | NativeArray (Maybe Length) Type
    | StructType String [(String, Type)]
    | IVarType Type
    deriving (Lift, Show)

-- | Type equality is just structural equality, except for arrays
-- where size info is ignored and struct types where the tag is ignored.
instance Eq Type where
   VoidType              == VoidType              = True
   (l1 :# t1)            == (l2 :# t2)            = l1 == l2 && t1 == t2
   (ArrayType _ t1)      == (ArrayType _ t2)      = t1 == t2
   (NativeArray l1 t1)   == (NativeArray l2 t2)   = l1 == l2 && t1 == t2
   (StructType _ l1)     == (StructType _ l2)     = l1 == l2
   (IVarType t1)         == (IVarType t2)         = t1 == t2
   StringType            == StringType            = True
   _                     == _                     = False

-- | Render a C representation of a scalar type
renderScalarType :: ScalarType -> String
renderScalarType BoolType = "bool"
renderScalarType BitType = error "renderScalarType: No support for BitType"
renderScalarType FloatType = "float"
renderScalarType DoubleType = "double"
renderScalarType (NumType sg sz) = toInt sg ++ toSize sz ++ "_t"
  where toInt Signed = "int"
        toInt Unsigned = "uint"
        toSize S8   = "8"
        toSize S16  = "16"
        toSize S32  = "32"
        toSize S40  = "40"
        toSize S64  = "64"
        toSize S128 = "128"
renderScalarType (ComplexType t) = renderType t ++ " complex"
renderScalarType (Pointer t) = renderType t ++ " *"

-- | Render a C representation of a type
renderType :: Type -> String
renderType VoidType = "void"
renderType (1 :# t) = renderScalarType t
renderType (_ :# _) = error "renderType: No support for SIMD vector output"
renderType StringType = "char *"
renderType (ArrayType _ t) = renderType t ++ " *"
renderType (NativeArray _ t) = renderType t
renderType (StructType n _) = "struct " ++ n
renderType IVarType{} = "struct ivar"

-- | Extend a helper function for the platform
extend :: String -> Type -> String
extend s t = s ++ "_fun_" ++ renderType t

----------------------
-- * Type inference
----------------------

instance HasType (Variable t) where
    type TypeOf (Variable t) = Type
    typeof Variable{..}      = varType

instance HasType (Constant t) where
    type TypeOf (Constant t) = Type
    typeof IntConst{..}      = 1 :# intType
    typeof DoubleConst{}     = 1 :# DoubleType
    typeof FloatConst{}      = 1 :# FloatType
    typeof BoolConst{}       = 1 :# BoolType
    typeof StringConst{}     = StringType
    typeof ArrayConst{..}    = NativeArray (Just (fromIntegral $ length arrayValues)) arrayType
    typeof StructConst{..}   = structType
    typeof ComplexConst{..}  = 1 :# ComplexType (typeof realPartComplexValue)

instance HasType (Expression t) where
    type TypeOf (Expression t) = Type
    typeof VarExpr{..}   = typeof varExpr
    typeof ArrayElem{..} = decrArrayDepth $ typeof array
      where
        decrArrayDepth :: Type -> Type
        decrArrayDepth (ArrayType _ t)               = t
        decrArrayDepth (NativeArray _ t)             = t
        -- Allow indexing of int* with [].
        decrArrayDepth (1 :# Pointer t)              = t
        decrArrayDepth t                             = reprError InternalError $ "Non-array variable is indexed! " ++ show array ++ " :: " ++ show t
    typeof StructField{..} = getStructFieldType fieldName $ typeof struct
      where
        getStructFieldType :: String -> Type -> Type
        getStructFieldType f (StructType _ l) = fromMaybe (structFieldNotFound f) $ lookup f l
        getStructFieldType f t                = reprError InternalError $
            "Trying to get a struct field from not a struct typed expression\n" ++ "Field: " ++ f ++ "\nType:  " ++ show t
        structFieldNotFound f = reprError InternalError $ "Not found struct field with this name: " ++ f
    typeof ConstExpr{..}    = typeof constExpr
    typeof FunctionCall{..} = returnType function
    typeof Cast{..}         = castType
    typeof AddrOf{..}       = 1 :# Pointer (typeof addrExpr)
    -- FIXME: Non-portable size_t assumption for SizeOf.
    typeof SizeOf{}         = 1 :# NumType Signed S32
    typeof Deref{..}        = case typeof ptrExpr of
                                1 :# (Pointer btype) -> btype
                                wtype         -> reprError InternalError $ "Type of dereferenced expression " ++ show ptrExpr ++ " has type " ++ show wtype

instance HasType (ActualParameter t) where
    type TypeOf (ActualParameter t) = Type
    typeof ValueParameter{..}= typeof valueParam
    typeof TypeParameter{..} = typeParam
    typeof FunParameter{}    = VoidType


reprError :: ErrorClass -> String -> a
reprError = handleError "Feldspar.Compiler.Imperative.Representation"

-- | Free variables of an expression.
fv :: Expression t -> [Variable t]
fv = nub . fv'

fv' :: Expression t -> [Variable t]
fv' (VarExpr v)         = [v]
fv' (ArrayElem e i)     = fv' e ++ concatMap fv' i
fv' (StructField e _)   = fv' e
fv' (FunctionCall _ ps) = concatMap fv' ps
fv' (Cast _ e)          = fv' e
fv' (AddrOf e)          = fv' e
fv' (Deref e)           = fv' e
fv' _                   = []
