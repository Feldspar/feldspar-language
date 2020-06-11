{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Onnx.AttributeProto.AttributeType (AttributeType(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data AttributeType = UNDEFINED
                   | FLOAT
                   | INT
                   | STRING
                   | TENSOR
                   | GRAPH
                   | SPARSE_TENSOR
                   | FLOATS
                   | INTS
                   | STRINGS
                   | TENSORS
                   | GRAPHS
                   | SPARSE_TENSORS
                     deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data,
                               Prelude'.Generic)

instance P'.Mergeable AttributeType

instance Prelude'.Bounded AttributeType where
  minBound = UNDEFINED
  maxBound = SPARSE_TENSORS

instance P'.Default AttributeType where
  defaultValue = UNDEFINED

toMaybe'Enum :: Prelude'.Int -> P'.Maybe AttributeType
toMaybe'Enum 0 = Prelude'.Just UNDEFINED
toMaybe'Enum 1 = Prelude'.Just FLOAT
toMaybe'Enum 2 = Prelude'.Just INT
toMaybe'Enum 3 = Prelude'.Just STRING
toMaybe'Enum 4 = Prelude'.Just TENSOR
toMaybe'Enum 5 = Prelude'.Just GRAPH
toMaybe'Enum 11 = Prelude'.Just SPARSE_TENSOR
toMaybe'Enum 6 = Prelude'.Just FLOATS
toMaybe'Enum 7 = Prelude'.Just INTS
toMaybe'Enum 8 = Prelude'.Just STRINGS
toMaybe'Enum 9 = Prelude'.Just TENSORS
toMaybe'Enum 10 = Prelude'.Just GRAPHS
toMaybe'Enum 12 = Prelude'.Just SPARSE_TENSORS
toMaybe'Enum _ = Prelude'.Nothing

instance Prelude'.Enum AttributeType where
  fromEnum UNDEFINED = 0
  fromEnum FLOAT = 1
  fromEnum INT = 2
  fromEnum STRING = 3
  fromEnum TENSOR = 4
  fromEnum GRAPH = 5
  fromEnum SPARSE_TENSOR = 11
  fromEnum FLOATS = 6
  fromEnum INTS = 7
  fromEnum STRINGS = 8
  fromEnum TENSORS = 9
  fromEnum GRAPHS = 10
  fromEnum SPARSE_TENSORS = 12
  toEnum
   = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type Onnx.AttributeProto.AttributeType") .
      toMaybe'Enum
  succ UNDEFINED = FLOAT
  succ FLOAT = INT
  succ INT = STRING
  succ STRING = TENSOR
  succ TENSOR = GRAPH
  succ GRAPH = SPARSE_TENSOR
  succ SPARSE_TENSOR = FLOATS
  succ FLOATS = INTS
  succ INTS = STRINGS
  succ STRINGS = TENSORS
  succ TENSORS = GRAPHS
  succ GRAPHS = SPARSE_TENSORS
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Onnx.AttributeProto.AttributeType"
  pred FLOAT = UNDEFINED
  pred INT = FLOAT
  pred STRING = INT
  pred TENSOR = STRING
  pred GRAPH = TENSOR
  pred SPARSE_TENSOR = GRAPH
  pred FLOATS = SPARSE_TENSOR
  pred INTS = FLOATS
  pred STRINGS = INTS
  pred TENSORS = STRINGS
  pred GRAPHS = TENSORS
  pred SPARSE_TENSORS = GRAPHS
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type Onnx.AttributeProto.AttributeType"

instance P'.Wire AttributeType where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'

instance P'.GPB AttributeType

instance P'.MessageAPI msg' (msg' -> AttributeType) AttributeType where
  getVal m' f' = f' m'

instance P'.ReflectEnum AttributeType where
  reflectEnum
   = [(0, "UNDEFINED", UNDEFINED), (1, "FLOAT", FLOAT), (2, "INT", INT), (3, "STRING", STRING), (4, "TENSOR", TENSOR),
      (5, "GRAPH", GRAPH), (11, "SPARSE_TENSOR", SPARSE_TENSOR), (6, "FLOATS", FLOATS), (7, "INTS", INTS), (8, "STRINGS", STRINGS),
      (9, "TENSORS", TENSORS), (10, "GRAPHS", GRAPHS), (12, "SPARSE_TENSORS", SPARSE_TENSORS)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".onnx.AttributeProto.AttributeType") [] ["Onnx", "AttributeProto"] "AttributeType")
      ["Onnx", "AttributeProto", "AttributeType.hs"]
      [(0, "UNDEFINED"), (1, "FLOAT"), (2, "INT"), (3, "STRING"), (4, "TENSOR"), (5, "GRAPH"), (11, "SPARSE_TENSOR"), (6, "FLOATS"),
       (7, "INTS"), (8, "STRINGS"), (9, "TENSORS"), (10, "GRAPHS"), (12, "SPARSE_TENSORS")]
      Prelude'.False

instance P'.TextType AttributeType where
  tellT = P'.tellShow
  getT = P'.getRead