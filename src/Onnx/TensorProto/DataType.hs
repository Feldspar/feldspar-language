{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Onnx.TensorProto.DataType (DataType(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data DataType = UNDEFINED
              | FLOAT
              | UINT8
              | INT8
              | UINT16
              | INT16
              | INT32
              | INT64
              | STRING
              | BOOL
              | FLOAT16
              | DOUBLE
              | UINT32
              | UINT64
              | COMPLEX64
              | COMPLEX128
              | BFLOAT16
                deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data,
                          Prelude'.Generic)

instance P'.Mergeable DataType

instance Prelude'.Bounded DataType where
  minBound = UNDEFINED
  maxBound = BFLOAT16

instance P'.Default DataType where
  defaultValue = UNDEFINED

toMaybe'Enum :: Prelude'.Int -> P'.Maybe DataType
toMaybe'Enum 0 = Prelude'.Just UNDEFINED
toMaybe'Enum 1 = Prelude'.Just FLOAT
toMaybe'Enum 2 = Prelude'.Just UINT8
toMaybe'Enum 3 = Prelude'.Just INT8
toMaybe'Enum 4 = Prelude'.Just UINT16
toMaybe'Enum 5 = Prelude'.Just INT16
toMaybe'Enum 6 = Prelude'.Just INT32
toMaybe'Enum 7 = Prelude'.Just INT64
toMaybe'Enum 8 = Prelude'.Just STRING
toMaybe'Enum 9 = Prelude'.Just BOOL
toMaybe'Enum 10 = Prelude'.Just FLOAT16
toMaybe'Enum 11 = Prelude'.Just DOUBLE
toMaybe'Enum 12 = Prelude'.Just UINT32
toMaybe'Enum 13 = Prelude'.Just UINT64
toMaybe'Enum 14 = Prelude'.Just COMPLEX64
toMaybe'Enum 15 = Prelude'.Just COMPLEX128
toMaybe'Enum 16 = Prelude'.Just BFLOAT16
toMaybe'Enum _ = Prelude'.Nothing

instance Prelude'.Enum DataType where
  fromEnum UNDEFINED = 0
  fromEnum FLOAT = 1
  fromEnum UINT8 = 2
  fromEnum INT8 = 3
  fromEnum UINT16 = 4
  fromEnum INT16 = 5
  fromEnum INT32 = 6
  fromEnum INT64 = 7
  fromEnum STRING = 8
  fromEnum BOOL = 9
  fromEnum FLOAT16 = 10
  fromEnum DOUBLE = 11
  fromEnum UINT32 = 12
  fromEnum UINT64 = 13
  fromEnum COMPLEX64 = 14
  fromEnum COMPLEX128 = 15
  fromEnum BFLOAT16 = 16
  toEnum = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type Onnx.TensorProto.DataType") . toMaybe'Enum
  succ UNDEFINED = FLOAT
  succ FLOAT = UINT8
  succ UINT8 = INT8
  succ INT8 = UINT16
  succ UINT16 = INT16
  succ INT16 = INT32
  succ INT32 = INT64
  succ INT64 = STRING
  succ STRING = BOOL
  succ BOOL = FLOAT16
  succ FLOAT16 = DOUBLE
  succ DOUBLE = UINT32
  succ UINT32 = UINT64
  succ UINT64 = COMPLEX64
  succ COMPLEX64 = COMPLEX128
  succ COMPLEX128 = BFLOAT16
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Onnx.TensorProto.DataType"
  pred FLOAT = UNDEFINED
  pred UINT8 = FLOAT
  pred INT8 = UINT8
  pred UINT16 = INT8
  pred INT16 = UINT16
  pred INT32 = INT16
  pred INT64 = INT32
  pred STRING = INT64
  pred BOOL = STRING
  pred FLOAT16 = BOOL
  pred DOUBLE = FLOAT16
  pred UINT32 = DOUBLE
  pred UINT64 = UINT32
  pred COMPLEX64 = UINT64
  pred COMPLEX128 = COMPLEX64
  pred BFLOAT16 = COMPLEX128
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type Onnx.TensorProto.DataType"

instance P'.Wire DataType where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'

instance P'.GPB DataType

instance P'.MessageAPI msg' (msg' -> DataType) DataType where
  getVal m' f' = f' m'

instance P'.ReflectEnum DataType where
  reflectEnum
   = [(0, "UNDEFINED", UNDEFINED), (1, "FLOAT", FLOAT), (2, "UINT8", UINT8), (3, "INT8", INT8), (4, "UINT16", UINT16),
      (5, "INT16", INT16), (6, "INT32", INT32), (7, "INT64", INT64), (8, "STRING", STRING), (9, "BOOL", BOOL),
      (10, "FLOAT16", FLOAT16), (11, "DOUBLE", DOUBLE), (12, "UINT32", UINT32), (13, "UINT64", UINT64),
      (14, "COMPLEX64", COMPLEX64), (15, "COMPLEX128", COMPLEX128), (16, "BFLOAT16", BFLOAT16)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".onnx.TensorProto.DataType") [] ["Onnx", "TensorProto"] "DataType")
      ["Onnx", "TensorProto", "DataType.hs"]
      [(0, "UNDEFINED"), (1, "FLOAT"), (2, "UINT8"), (3, "INT8"), (4, "UINT16"), (5, "INT16"), (6, "INT32"), (7, "INT64"),
       (8, "STRING"), (9, "BOOL"), (10, "FLOAT16"), (11, "DOUBLE"), (12, "UINT32"), (13, "UINT64"), (14, "COMPLEX64"),
       (15, "COMPLEX128"), (16, "BFLOAT16")]
      Prelude'.False

instance P'.TextType DataType where
  tellT = P'.tellShow
  getT = P'.getRead